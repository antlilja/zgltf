const std = @import("std");

pub fn parse(
    comptime TopLevel: type,
    reader: anytype,
    allocator: std.mem.Allocator,
    tmp_allocator: std.mem.Allocator,
) !TopLevel {
    if (@typeInfo(TopLevel) != .@"struct") @compileError("Unsupported top level type: " ++ @typeName(TopLevel));

    var self: Parser(TopLevel, @TypeOf(reader)) = .{
        .reader = reader,
        .allocator = allocator,
        .tmp_allocator = tmp_allocator,
        .scanner = std.json.Scanner.initStreaming(tmp_allocator),
        .partial = std.ArrayList(u8).init(tmp_allocator),
    };
    defer {
        self.partial.deinit();
        self.scanner.deinit();
    }

    return self.parseTopLevel();
}

fn Parser(comptime TopLevel: type, comptime Reader: type) type {
    return struct {
        const Self = @This();

        comptime {
            std.debug.assert(@typeInfo(TopLevel) == .@"struct");
        }

        reader: Reader,
        allocator: std.mem.Allocator,
        tmp_allocator: std.mem.Allocator,

        scanner: std.json.Scanner,

        partial: std.ArrayList(u8),
        read_buffer: [512]u8 = undefined,

        peek_token: ?std.json.Token = null,

        pub fn parseTopLevel(self: *Self) !TopLevel {
            return try self.innerParse(TopLevel);
        }

        fn innerParse(self: *Self, comptime T: type) !T {
            return switch (@typeInfo(T)) {
                .void => blk: {
                    try self.skip();
                    break :blk {};
                },
                .optional => |info| switch (try self.nextToken()) {
                    .null => null,
                    else => |token| blk: {
                        self.peek_token = token;
                        break :blk try self.innerParse(info.child);
                    },
                },
                .bool => switch (try self.nextToken()) {
                    .false => false,
                    .true => true,
                    else => error.InvalidToken,
                },
                .int, .float => try self.handleNumber(T),
                .@"enum" => |info| if (info.is_exhaustive) blk: {
                    break :blk if (@hasDecl(T, "lookup"))
                        (T.lookup.get(try self.handleString()) orelse error.InvalidEnum)
                    else
                        std.meta.stringToEnum(T, try self.handleString()) orelse error.InvalidEnum;
                } else @enumFromInt(try self.handleNumber(info.tag_type)),
                .array => |info| blk: {
                    var result: T = undefined;
                    if (try self.nextToken() != .array_begin) return error.InvalidToken;
                    for (0..info.len) |i| {
                        result[i] = try self.innerParse(info.child);
                    }
                    if (try self.nextToken() != .array_end) return error.InvalidToken;
                    break :blk result;
                },
                .pointer => |info| blk: {
                    comptime if (info.size != .slice) @compileError("Invalid type");

                    break :blk switch (info.child) {
                        u8 => try self.allocator.dupe(u8, try self.handleString()),
                        u32 => {
                            if (try self.nextToken() != .array_begin) return error.InvalidToken;

                            var tmp_storage = std.ArrayList(u32).init(self.tmp_allocator);
                            defer tmp_storage.deinit();

                            while (true) {
                                const token = try self.nextToken();
                                if (token == .array_end) break;
                                self.peek_token = token;
                                try tmp_storage.append(try self.handleNumber(u32));
                            }
                            break :blk try self.allocator.dupe(u32, tmp_storage.items);
                        },
                        else => {
                            if (try self.nextToken() != .array_begin) return error.InvalidToken;

                            var tmp_storage = std.ArrayList(info.child).init(self.tmp_allocator);
                            defer tmp_storage.deinit();

                            while (true) {
                                switch (try self.nextToken()) {
                                    .array_end => break,
                                    else => |token| {
                                        self.peek_token = token;
                                        try tmp_storage.append(try self.innerParse(info.child));
                                    },
                                }
                            }
                            break :blk try self.allocator.dupe(info.child, tmp_storage.items);
                        },
                    };
                },
                .@"struct" => blk: {
                    if (try self.nextToken() != .object_begin) return error.InvalidToken;

                    const map = comptime getStructMap(T);

                    var result: T = .{};
                    loop: while (try self.handleStringOrEndObject()) |string| {
                        if (map.get(string)) |index| {
                            inline for (std.meta.fields(T), 0..) |field, i| {
                                if (i == index) {
                                    switch (@typeInfo(field.type)) {
                                        .@"union" => |info| {
                                            inline for (info.fields) |union_field| {
                                                if (std.mem.eql(u8, union_field.name, string)) {
                                                    @field(
                                                        result,
                                                        field.name,
                                                    ) = @unionInit(
                                                        field.type,
                                                        union_field.name,
                                                        try self.innerParse(union_field.type),
                                                    );

                                                    continue :loop;
                                                }
                                            }
                                            unreachable;
                                        },
                                        else => {
                                            @field(
                                                result,
                                                field.name,
                                            ) = try self.innerParse(field.type);
                                            continue :loop;
                                        },
                                    }
                                }
                            }
                        } else try self.skip();
                    }

                    break :blk result;
                },
                else => @compileError("Unsupported type: " ++ @typeName(T)),
            };
        }

        fn skip(self: *Self) !void {
            var depth: usize = 0;
            while (true) {
                switch (try self.nextToken()) {
                    .array_begin,
                    .object_begin,
                    => depth += 1,
                    .array_end,
                    .object_end,
                    => {
                        depth -= 1;
                        if (depth == 0) break;
                    },
                    .partial_number,
                    .partial_string,
                    .partial_string_escaped_1,
                    .partial_string_escaped_2,
                    .partial_string_escaped_3,
                    .partial_string_escaped_4,
                    => {},
                    .true,
                    .false,
                    .null,
                    .number,
                    .string,
                    => if (depth == 0) break,
                    else => return error.InvalidToken,
                }
            }
        }

        fn getStructMap(comptime T: type) std.StaticStringMap(usize) {
            const fields = std.meta.fields(T);
            const KeyValue = struct { []const u8, usize };
            var kvs_list: []const KeyValue = &.{};
            var index: usize = 0;
            for (fields) |field| {
                if (@hasDecl(T, "field_name_lookup")) {
                    kvs_list = kvs_list ++ &[_]KeyValue{.{ T.field_name_lookup.get(field.name) orelse field.name, index }};
                    index += 1;
                    continue;
                }

                switch (@typeInfo(field.type)) {
                    .optional => |info| if (@typeInfo(info.child) == .@"struct") {
                        if (@hasDecl(info.child, "name")) {
                            kvs_list = kvs_list ++ &[_]KeyValue{.{ info.child.name, index }};
                            index += 1;
                            continue;
                        }
                    },
                    .@"struct" => if (@hasDecl(field.type, "name")) {
                        kvs_list = kvs_list ++ &[_]KeyValue{.{ field.type.name, index }};
                        index += 1;
                        continue;
                    },
                    .@"union" => |info| {
                        for (info.fields) |union_field| {
                            kvs_list = kvs_list ++ &[_]KeyValue{.{ union_field.name, index }};
                            index += 1;
                        }
                    },
                    else => {},
                }

                var gltf_name: []const u8 = &.{};

                var start_name_index = 0;
                var name_index = 0;
                while (name_index < field.name.len) {
                    switch (field.name[name_index]) {
                        'a'...'z' => name_index += 1,
                        '_' => {
                            gltf_name = gltf_name ++ field.name[start_name_index..name_index];
                            name_index += 1;
                            gltf_name = gltf_name ++ &[_]u8{std.ascii.toUpper(field.name[name_index])};
                            name_index += 1;
                            start_name_index = name_index;
                        },
                        else => unreachable,
                    }
                }

                if (start_name_index != name_index) {
                    gltf_name = gltf_name ++ field.name[start_name_index..name_index];
                }

                kvs_list = kvs_list ++ &[_]KeyValue{.{ gltf_name, index }};
                index += 1;
            }

            return std.StaticStringMap(usize).initComptime(kvs_list);
        }

        fn handleStringOrEndObject(
            self: *Self,
        ) !?[]const u8 {
            const token = try self.nextToken();
            if (token == .object_end) return null;

            self.peek_token = token;
            return try self.handleString();
        }

        fn handleString(
            self: *Self,
        ) ![]const u8 {
            self.partial.clearRetainingCapacity();
            while (true) {
                switch (try self.nextToken()) {
                    .string => |str| {
                        if (str.len != 0) try self.partial.appendSlice(str);
                        return self.partial.items;
                    },
                    .partial_string => |partial_str| try self.partial.appendSlice(partial_str),
                    .partial_string_escaped_1 => |partial_string| try self.partial.appendSlice(&partial_string),
                    .partial_string_escaped_2 => |partial_string| try self.partial.appendSlice(&partial_string),
                    .partial_string_escaped_3 => |partial_string| try self.partial.appendSlice(&partial_string),
                    .partial_string_escaped_4 => |partial_string| try self.partial.appendSlice(&partial_string),
                    else => return error.InvalidToken,
                }
            }

            unreachable;
        }

        fn handleNumber(
            self: *Self,
            comptime Number: type,
        ) !Number {
            self.partial.clearRetainingCapacity();
            while (true) {
                switch (try self.nextToken()) {
                    .number => |str| {
                        if (str.len != 0) try self.partial.appendSlice(str);
                        return switch (@typeInfo(Number)) {
                            .int => try std.fmt.parseInt(Number, self.partial.items, 0),
                            .float => try std.fmt.parseFloat(Number, self.partial.items),
                            else => @compileError("Not a number"),
                        };
                    },
                    .partial_number => |partial_str| try self.partial.appendSlice(partial_str),
                    else => return error.InvalidToken,
                }
            }

            unreachable;
        }

        fn nextToken(self: *Self) !std.json.Token {
            if (self.peek_token) |token| {
                self.peek_token = null;
                return token;
            }

            while (true) {
                return self.scanner.next() catch |err| switch (err) {
                    error.BufferUnderrun => {
                        const size = try self.reader.read(&self.read_buffer);
                        if (size == 0) {
                            self.scanner.endInput();
                            break;
                        }
                        self.scanner.feedInput(self.read_buffer[0..size]);
                        continue;
                    },
                    else => return err,
                };
            }

            return try self.scanner.next();
        }
    };
}
