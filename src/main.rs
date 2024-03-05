mod bitfields;
mod register;

use clap::Parser;
use std::collections::HashMap;
use std::fmt;
use std::fs;

use register::Register;

#[derive(Debug, Copy, Clone)]
enum FieldLabel {
    Opcode,
    D,
    W,
    Mod,
    Reg,
    Rm,
    // DispLo,
    // DispHi,
    // Data,
}

#[derive(Debug)]
struct MovRegMem {
    _opcode: u8,
    d: u8,
    w: u8,
    mode: u8,
    reg: u8,
    rm: u8,
    disp_lo: Option<u8>,
    disp_hi: Option<u8>,
}

impl fmt::Display for MovRegMem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match Mode::from_encoding(self.mode, self.rm) {
            Mode::MemoryNoDisplacement => {
                let expr = get_effective_address(self.rm, 0);
                match self.d {
                    1 => write!(
                        f,
                        "mov {}, {}",
                        Register::from_encoding(self.reg, self.w),
                        expr,
                    ), // reg is dest
                    0 => write!(
                        f,
                        "mov {}, {}",
                        expr,
                        Register::from_encoding(self.reg, self.w)
                    ), // reg is src
                    _ => write!(f, "error decoding d"),
                }
            }
            Mode::Memory8BitDisplacement => {
                let displacement = self.disp_lo;
                let expr = get_effective_address(self.rm, displacement.unwrap().into());
                match self.d {
                    1 => write!(
                        f,
                        "mov {}, {}",
                        Register::from_encoding(self.reg, self.w),
                        expr,
                    ), // reg is dest
                    0 => write!(
                        f,
                        "mov {}, {}",
                        expr,
                        Register::from_encoding(self.reg, self.w)
                    ), // reg is src
                    _ => write!(f, "error decoding d"),
                }
            }
            Mode::Memory16BitDisplacement => {
                let disp_hi = self.disp_hi.unwrap_or(0);
                let disp_lo = self.disp_lo.unwrap_or(0);
                let displacement: u16 = (disp_hi as u16) << 8 | (disp_lo as u16);
                let expr = get_effective_address(self.rm, displacement);
                match self.d {
                    1 => write!(
                        f,
                        "mov {}, {}",
                        Register::from_encoding(self.reg, self.w),
                        expr,
                    ), // reg is dest
                    0 => write!(
                        f,
                        "mov {}, {}",
                        expr,
                        Register::from_encoding(self.reg, self.w)
                    ), // reg is src
                    _ => write!(f, "error decoding d"),
                }
            }
            Mode::Register => {
                match self.d {
                    1 => write!(
                        f,
                        "mov {}, {}",
                        Register::from_encoding(self.reg, self.w),
                        Register::from_encoding(self.rm, self.w)
                    ), // reg is dest
                    0 => write!(
                        f,
                        "mov {}, {}",
                        Register::from_encoding(self.rm, self.w),
                        Register::from_encoding(self.reg, self.w)
                    ), // reg is src
                    _ => write!(f, "error decoding d"),
                }
            }
        }
    }
}

impl fmt::Display for MovImmToReg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "mov {}, {}",
            Register::from_encoding(self.reg, self.w),
            self.data
        )
    }
}

#[derive(Debug)]
struct MovImmToReg {
    opcode: u8,
    w: u8,
    reg: u8,
    data: u16,
}

#[derive(Debug)]
enum InstructionType {
    MovRegMem,
    MovImmToReg,
}

#[derive(Debug)]
struct Field {
    label: FieldLabel,
    width: u8,
}

trait Instruction {
    fn fields() -> Vec<Field>;
    fn decode(bytes: &[u8]) -> (impl Instruction, usize);
}

impl Instruction for MovRegMem {
    fn fields() -> Vec<Field> {
        vec![
            Field {
                label: FieldLabel::Opcode,
                width: 6,
            },
            Field {
                label: FieldLabel::D,
                width: 1,
            },
            Field {
                label: FieldLabel::W,
                width: 1,
            },
            //
            Field {
                label: FieldLabel::Mod,
                width: 2,
            },
            Field {
                label: FieldLabel::Reg,
                width: 3,
            },
            Field {
                label: FieldLabel::Rm,
                width: 3,
            },
        ]
    }

    fn decode(bytes: &[u8]) -> (MovRegMem, usize) {
        let mut byte_count = 2;
        let fields = MovRegMem::fields();
        let widths: Vec<_> = fields.iter().map(|field| field.width).collect();
        let values = bitfields::extract_fields(byte_count, bytes, widths);

        let opcode = values[0];
        let d = values[1];
        let w = values[2];

        let mode = values[3];
        let reg = values[4];
        let rm = values[5];

        let m = Mode::from_encoding(mode, rm);
        match m {
            Mode::Register | Mode::MemoryNoDisplacement => (
                MovRegMem {
                    _opcode: opcode,
                    d,
                    w,
                    mode,
                    reg,
                    rm,
                    disp_lo: None,
                    disp_hi: None,
                },
                byte_count.into(),
            ),
            Mode::Memory8BitDisplacement => {
                let displ_lo = bytes[2];
                byte_count += 1;
                (
                    MovRegMem {
                        _opcode: opcode,
                        d,
                        w,
                        mode,
                        reg,
                        rm,
                        disp_lo: Some(displ_lo),
                        disp_hi: None,
                    },
                    byte_count.into(),
                )
            }
            Mode::Memory16BitDisplacement => {
                let displ_lo = bytes[2];
                let displ_hi = bytes[3];

                byte_count += 2;
                (
                    MovRegMem {
                        _opcode: opcode,
                        d,
                        w,
                        mode,
                        reg,
                        rm,
                        disp_lo: Some(displ_lo),
                        disp_hi: Some(displ_hi),
                    },
                    byte_count.into(),
                )
            }
        }
    }
}

impl Instruction for MovImmToReg {
    fn fields() -> Vec<Field> {
        vec![
            Field {
                label: FieldLabel::Opcode,
                width: 4,
            },
            Field {
                label: FieldLabel::W,
                width: 1,
            },
            Field {
                label: FieldLabel::Reg,
                width: 3,
            },
        ]
    }

    fn decode(bytes: &[u8]) -> (MovImmToReg, usize) {
        let mut byte_count = 1;
        let fields = MovImmToReg::fields();
        let widths: Vec<_> = fields.iter().map(|field| field.width).collect();
        let values = bitfields::extract_fields(byte_count, bytes, widths);

        let opcode = values[0];
        let w = values[1];
        let reg = values[2];
        //
        let mut data = bytes[1] as u16;
        byte_count += 1;

        if w == 1 {
            byte_count += 1;
            let data16 = bytes[2];
            data |= (data16 as u16) << 8
        }

        (
            MovImmToReg {
                opcode,
                w,
                reg,
                data,
            },
            byte_count.into(),
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Mode {
    MemoryNoDisplacement,
    Memory8BitDisplacement,
    Memory16BitDisplacement,
    Register,
}

impl Mode {
    fn from_encoding(encoding: u8, rm: u8) -> Mode {
        match (encoding, rm) {
            // Special case where R/M field is 110
            (0b00, 0b110) => Mode::Memory16BitDisplacement,
            (0b00, _) => Mode::MemoryNoDisplacement,
            (0b01, _) => Mode::Memory8BitDisplacement,
            (0b10, _) => Mode::Memory16BitDisplacement,
            (0b11, _) => Mode::Register,
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
    } else {
        format!("[{result}]")
    }
}

fn find_opcode(byte: u8, instruction_table: &HashMap<u8, InstructionType>) -> &InstructionType {
    let opcodes = vec![
        (byte & 0b1111_0000) >> 4,
        (byte & 0b1111_1000) >> 3,
        (byte & 0b1111_1100) >> 2,
        (byte & 0b1111_1110) >> 1,
        byte,
    ];

    for opcode in opcodes {
        if let Some(instruction) = instruction_table.get(&opcode) {
            return instruction;
        }
    }
    panic!("unknown instruction {:#b}", byte)
}

fn decode_bytes(bytes: Vec<u8>) -> String {
    let mut cursor: usize = 0;

    let mut instruction_table: HashMap<u8, InstructionType> = HashMap::new();

    instruction_table.insert(0b100010, InstructionType::MovRegMem);
    instruction_table.insert(0b1011, InstructionType::MovImmToReg);

    let mut result = vec!["bits 16".to_string(), "".to_string()];

    while cursor < bytes.len() {
        let byte = bytes[cursor];
        let instruction_type = find_opcode(byte, &instruction_table);

        match instruction_type {
            InstructionType::MovRegMem => {
                let (instr, bytes_consumed) = MovRegMem::decode(&bytes[cursor..]);

                result.push(format!("{}", instr));
                dbg!(&result);
                cursor += bytes_consumed;
            }
            InstructionType::MovImmToReg => {
                let (instr, bytes_consumed) = MovImmToReg::decode(&bytes[cursor..]);
                result.push(format!("{}", instr));
                dbg!(&result);
                cursor += bytes_consumed;
            }
        };
    }

    result.join("\n")
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
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
    let actual_output = decode_file(&"test/listing_0039_more_movs".to_string());

    let expected_output = "bits 16

mov si, bx
mov dh, al
mov cl, 12
mov ch, 244
mov cx, 12
mov cx, 65524
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
