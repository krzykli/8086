use clap::Parser;
use core::panic;
use std::fmt;
use std::fs;
use std::io::{Error, ErrorKind};

enum Mask {
    // byte 1
    OpcodeMask = 0b1111_1100,
    ToRegisterMask = 0b0000_0010,
    WordMask = 0b0000_0001,
    Nibble = 0b1111_0000,
    // byte 2
    RegisterMode = 0b1100_0000,
    RegisterExt = 0b0011_1000,
    RegisterOp = 0b0000_0111,
}

#[derive(Debug)]
enum OpCode {
    MOV(MovType),
}

#[derive(Debug)]
enum MovType {
    RegToReg,
    ImmToRegMem,
    ImmToReg,
    Acc,
    Segment,
}

impl OpCode {
    fn is_mov(&self) -> bool {
        matches!(self, OpCode::MOV(_))
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_mov() {
            write!(f, "mov")
        } else {
            write!(f, "unknown")
        }
    }
}

impl OpCode {
    fn from_encoding(encoding: u8) -> Result<OpCode, std::io::Error> {
        let nibble = (encoding & Mask::Nibble as u8) >> 4;
        let opcode = (encoding & Mask::OpcodeMask as u8) >> 2;
        if (nibble) == 0b001011 {
            return Ok(OpCode::MOV(MovType::ImmToReg));
        }

        match opcode {
            0b00100010 => Ok(OpCode::MOV(MovType::RegToReg)),
            0b00110001 => Ok(OpCode::MOV(MovType::ImmToRegMem)),
            0b00101000 => Ok(OpCode::MOV(MovType::Acc)),
            0b00100011 => Ok(OpCode::MOV(MovType::Segment)),

            unmatched => Err(Error::new(
                ErrorKind::Other,
                format!("Unimplemented handling of opcode {:b}", unmatched),
            )),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Register {
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH,
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Register::AL => write!(f, "al"),
            Register::CL => write!(f, "cl"),
            Register::DL => write!(f, "dl"),
            Register::BL => write!(f, "bl"),
            Register::AH => write!(f, "ah"),
            Register::CH => write!(f, "ch"),
            Register::DH => write!(f, "dh"),
            Register::BH => write!(f, "bh"),
            Register::AX => write!(f, "ax"),
            Register::CX => write!(f, "cx"),
            Register::DX => write!(f, "dx"),
            Register::BX => write!(f, "bx"),
            Register::SP => write!(f, "sp"),
            Register::BP => write!(f, "bp"),
            Register::SI => write!(f, "si"),
            Register::DI => write!(f, "di"),
        }
    }
}

impl Register {
    fn from_encoding(encoding: u8, w: u8) -> Register {
        match (encoding, w) {
            (0b000, 0) => Register::AL,
            (0b001, 0) => Register::CL,
            (0b010, 0) => Register::DL,
            (0b011, 0) => Register::BL,
            (0b100, 0) => Register::AH,
            (0b101, 0) => Register::CH,
            (0b110, 0) => Register::DH,
            (0b111, 0) => Register::BH,
            (0b000, 1) => Register::AX,
            (0b001, 1) => Register::CX,
            (0b010, 1) => Register::DX,
            (0b011, 1) => Register::BX,
            (0b100, 1) => Register::SP,
            (0b101, 1) => Register::BP,
            (0b110, 1) => Register::SI,
            (0b111, 1) => Register::DI,
            _ => panic!("unknown REG encoding"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Mode {
    MemoryNoDisplacement,
    Memory8BitDisplacement,
    Memory16BitDisplacement,
    RegisterMode,
}

impl Mode {
    fn from_encoding(encoding: u8, rm: u8) -> Mode {
        match (encoding, rm) {
            // Special case where R/M field is 110
            (0b00, 0b110) => Mode::Memory16BitDisplacement,
            (0b00, _) => Mode::MemoryNoDisplacement,
            (0b01, _) => Mode::Memory8BitDisplacement,
            (0b10, _) => Mode::Memory16BitDisplacement,
            (0b11, _) => Mode::RegisterMode,
            _ => panic!("unknown MOD encoding"),
        }
    }
}


fn get_effective_address(rm: u8, displacement: u16) -> String {
    let result = match rm {
        0b000 => "bx + si".to_owned(),
        0b001 => "bx + di".to_owned(),
        0b010 => "bp + si".to_owned(),
        0b011 => "bp + di".to_owned(),
        0b100 => "si".to_owned(),
        0b101 => "di".to_owned(),
        0b110 => "bp".to_owned(),
        0b111 => "bx".to_owned(),
        _ => panic!("unknown rm match"),
    };

    if displacement > 0 {
        format!("[{result} + {displacement}]")
    }
    else {
        format!("[{result}]")
    }
}

fn decode_bytes(bytes: Vec<u8>) -> String {
    let mut i = 0;

    let mut output = "bits 16\n".to_string();
    while i < bytes.len() {
        let mut cursor = i;
        let first = bytes[cursor];

        let opcode = OpCode::from_encoding(first).unwrap_or_else(|err| {
            println!("{output}");
            eprintln!("{}, exiting", err);
            std::process::exit(1);
        });
        dbg!(&opcode);

        match opcode {
            OpCode::MOV(MovType::RegToReg) => {
                cursor += 1;
                let second = bytes[cursor];

                // direction -> 0 REG is source, 1 REG is dest
                let mut d = (first & Mask::ToRegisterMask as u8) >> 1;
                let w = first & Mask::WordMask as u8;

                // mode tells us whether one of the operands or both are registers
                let mode = (second & Mask::RegisterMode as u8) >> 6;
                let rm = (second & Mask::RegisterExt as u8) >> 3;
                let reg = second & Mask::RegisterOp as u8;

                let mode = Mode::from_encoding(mode, rm);

                dbg!(w);
                dbg!(&mode);
                dbg!(rm);
                dbg!(reg);

                let reg_name = Register::from_encoding(reg, w);

                match mode {
                    Mode::RegisterMode => {
                        let rm_name = Register::from_encoding(rm, w);
                        if d == 0 {
                            output += &format!("\n{} {}, {}", opcode, reg_name, rm_name);
                        } else {
                            output += &format!("\n{} {}, {}", opcode, rm_name, reg_name);
                        }
                    },
                    _ => {
                        let mut displacement_l = 0;
                        let mut displacement_h = 0;

                        if rm == 0b110 && mode == Mode::MemoryNoDisplacement {
                            cursor += 2
                        }

                        if mode == Mode::Memory8BitDisplacement {
                            cursor += 1;
                            displacement_l = bytes[cursor];
                        }
                        if mode == Mode::Memory16BitDisplacement {
                            cursor += 1;
                            displacement_h = bytes[cursor];
                        }
                        let displacement = u16::from_le_bytes([displacement_l, displacement_h]);
                        let address = get_effective_address(rm, displacement);

                        if d == 1 {
                            output += &format!("\n{} {}, {}", opcode, reg_name, address);
                        } else {
                            output += &format!("\n{} {}, {}", opcode, address, reg_name);
                        }
                    }
                };
            }

            OpCode::MOV(MovType::ImmToReg) => {
                cursor += 1;
                let data = bytes[cursor];
                let w = (first & 0b0000_1000) >> 3;
                let reg = first & 0b0000_0111;
                dbg!(w);

                let reg_dest_asm = Register::from_encoding(reg, w);
                let data_byte_l = data;
                let mut data_byte_h = 0;
                if w == 1 {
                    cursor += 1;
                    data_byte_h = bytes[cursor];
                }
                let combined_be = u16::from_le_bytes([data_byte_l, data_byte_h]);
                dbg!(combined_be);
                output += &format!("\n{} {}, {}", opcode, reg_dest_asm, combined_be);
            }
            _ => panic!("asdf"),
        }

        cursor += 1;
        i = cursor;
    }

    output
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long)]
    path: String,
}

fn decode_file(path: &String) -> String {
    let contents = fs::read(path);

    match contents {
        Ok(bytes) => decode_bytes(bytes),
        Err(error) => {
            panic!("unable to load file {path}, {error}")
        }
    }
}

fn main() {
    let args = Args::parse();
    let path = args.path;

    let output = decode_file(&path);
    println!("{output}");
    dbg!(&path);
}

#[test]
fn single_mov() {
    let actual_output = decode_file(&"test/listing_0037_single_register_mov".to_string());

    let expected_output = "bits 16

mov cx, bx";

    assert_eq!(actual_output, expected_output)
}

#[test]
fn many_register_mov() {
    let actual_output = decode_file(&"test/listing_0038_many_register_mov".to_string());

    let expected_output = "bits 16

mov cx, bx
mov ch, ah
mov dx, bx
mov si, bx
mov bx, di
mov al, cl
mov ch, ch
mov bx, ax
mov bx, si
mov sp, di
mov bp, ax";

    assert_eq!(actual_output, expected_output)
}

#[test]
fn more_movs() {
    let actual_output = decode_file(&"test/listing_0038_more_movs".to_string());

    let expected_output = "bits 16

mov si, bx
mov dh, al
mov cl, 12
mov ch, 244
mov cx, 12
mov cx, 244
mov dx, 3948
mov dx, 61588
mov al, [bx + si]
mov bx, [bp + di]
mov dx, [bp]
mov ah, [bx + si + 4]
mov al, [bx + si + 4999]
mov [bx + di], cx
mov [bp + si], cl
mov [bp], ch";

    assert_eq!(actual_output, expected_output)
}
