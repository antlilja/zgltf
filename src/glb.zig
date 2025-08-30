const std = @import("std");
const json = @import("json.zig");

const Header = extern struct {
    magic: u32,
    version: u32,
    length: u32,

    pub fn validate(self: Header) !void {
        if (self.magic != 0x46546C67) return error.InvalidHeaderMagic;
        if (self.version != 2) return error.InvalidHeaderVersion;
    }
};

const Chunk = extern struct {
    const Type = enum(u32) {
        json = 0x4E4F534A,
        bin = 0x004E4942,
        _,
    };

    length: u32,
    type: Type,
};

pub fn parse(
    comptime TopLevel: type,
    reader: *std.Io.Reader,
    allocator: std.mem.Allocator,
    tmp_allocator: std.mem.Allocator,
) !struct {
    gltf: TopLevel,
    buffer_offset: usize,
    buffer_length: u32,
} {
    const header = try reader.takeStruct(Header, .little);
    try header.validate();

    const json_length, const gltf = blk: {
        const chunk = try reader.takeStruct(Chunk, .little);
        if (chunk.type != .json) return error.MissingJsonChunk;

        var buf: [1024]u8 = undefined;
        var limited_reader = reader.limited(.limited(chunk.length), &buf);

        break :blk .{
            std.mem.alignForward(usize, chunk.length, 4),
            try json.parse(
                TopLevel,
                &limited_reader.interface,
                allocator,
                tmp_allocator,
            ),
        };
    };

    const chunk = try reader.takeStruct(Chunk, .little);
    if (chunk.type != .bin) return error.MissingBinChunk;

    return .{
        .gltf = gltf,
        .buffer_offset = @sizeOf(Header) + @sizeOf(u32) * 4 + json_length,
        .buffer_length = chunk.length,
    };
}
