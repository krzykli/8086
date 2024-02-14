use core::panic;
use std::fs::File;
use std::io::Write;
use std::{fs, path::Path};
use std::fmt;

enum Mask {
    // byte 1
    OpcodeMask = 0b1111_1100,
    ToRegisterMask = 0b0000_0010,
    WordMask = 0b0000_0001,
    // byte 2
    RegisterMode = 0b1100_0000,
    RegisterExt = 0b0011_1000,
    RegisterOp = 0b0000_0111,
}

#[derive(Debug, Clone, Copy)]
enum OpCode {
    MOV,
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpCode::MOV => write!(f, "mov")
        }
    }
}

impl OpCode {
    fn from_encoding(encoding: u8) -> OpCode {

        match encoding {

            0b00100010 => {
                OpCode::MOV
            }

            _ => {
                unimplemented!()
            }

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
            (0b00, _) => Mode::MemoryNoDisplacement,
            (0b01, _) => Mode::Memory8BitDisplacement,
            (0b10, _) => Mode::Memory16BitDisplacement,
            (0b11, _) => Mode::RegisterMode,
            // Special case where R/M field is 110
            (0b00, 0b110) => Mode::Memory16BitDisplacement,
            _ => panic!("unknown MOD encoding")
        }
    }
}


fn main() {
    let contents = fs::read(Path::new("test/listing_0037_single_register_mov"));

    match contents {
        Ok(bytes) => {
            dbg!(&bytes);
            let first = bytes[0];
            let second = bytes[1];

            let opcode = (first & Mask::OpcodeMask as u8) >> 2;
            // direction -> 0 REG is source, 1 REG is dest
            let d = (first & Mask::ToRegisterMask as u8) >> 1;
            let w = first & Mask::WordMask as u8;

            // mode tells us whether one of the operands or both are registers
            let mode = (second & Mask::RegisterMode as u8) >> 6;
            let rm = (second & Mask::RegisterExt as u8) >> 3;
            let reg = second & Mask::RegisterOp as u8;

            let mode = Mode::from_encoding(mode, rm);

            let opcode = OpCode::from_encoding(opcode);
            match mode {
                Mode::RegisterMode => {

                    let reg_dest_asm = Register::from_encoding(reg, w);
                    let reg_src_asm = Register::from_encoding(rm, w);

                    let mut output = "bits 16\n\n".to_string();
                    output += &opcode.to_string();
                    output += " ";
                    output += &reg_dest_asm.to_string();
                    output += ", ";
                    output += &reg_src_asm.to_string();

                    let mut file = File::create("output.asm").unwrap();
                    file.write_all(output.as_bytes());
                    file.flush().expect("something happened")
                }
                Mode::MemoryNoDisplacement => todo!(),
                Mode::Memory8BitDisplacement => todo!(),
                Mode::Memory16BitDisplacement => todo!(),
            }

        }
        Err(error) => {
            println!("{}", error)

        }
    }
}
