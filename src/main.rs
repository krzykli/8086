use clap::Parser;
use core::panic;
use std::collections::HashMap;
use std::fmt;
use std::fs;

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

#[derive(Debug)]
enum FieldWidths {
    Opcode(u8),
    D(u8),
    W(u8),
    Mod(u8),
    Reg(u8),
    Rm(u8),
    DispLo(u8),
    DispHi(u8),
    Data(u8),
}

#[derive(Debug, Copy, Clone)]
enum FieldLabel {
    Opcode,
    D,
    W,
    Mod,
    Reg,
    Rm,
    DispLo,
    DispHi,
    Data,
}

#[derive(Debug)]
struct MovRegMem {
    opcode: u8,
    d: u8,
    w: u8,
    r#mod: Register,
    reg: Register,
    rm: u8,
    disp_lo: Option<u8>,
    displ_hi: Option<u8>,
    data: Option<u8>,
}

#[derive(Debug)]
struct MovImmToReg;

#[derive(Debug)]
enum InstructionType {
    MovRegMem,
    MovImmToReg,
}

#[derive(Debug, Copy, Clone)]
struct Field {
    label: FieldLabel,
    width: u8,
}

trait Instruction {
    fn mnemonic() -> String;
    fn fields() -> Vec<Field>;
    fn decode(bytes: &Vec<u8>) -> String;
}

impl Instruction for MovRegMem {
    fn mnemonic() -> String {
        "mov".to_owned()
    }

    fn fields() -> Vec<Field> {
        return vec![
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
                width: 3,
            },
            Field {
                label: FieldLabel::Reg,
                width: 3,
            },
            Field {
                label: FieldLabel::Rm,
                width: 2,
            },
        ];
    }

    fn decode(bytes: &Vec<u8>) -> String {
        "".to_owned()
    }
}

// impl Instruction for MovImmToReg {
//     fn mnemonic() -> String {
//         "mov".to_owned()
//     }
//
//     fn fields() -> Vec<u8> {
//         return vec![
//             4,
//             1,
//             3,
//             8,
//             8,
//         ];
//     }
//
//     fn decode(bytes: &Vec<u8>) -> String {
//         "mov".to_owned()
//     }
//
// }

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

// fn get_effective_address(rm: u8, displacement: u16) -> String {
//     let result = match rm {
//         0b000 => "bx + si".to_owned(),
//         0b001 => "bx + di".to_owned(),
//         0b010 => "bp + si".to_owned(),
//         0b011 => "bp + di".to_owned(),
//         0b100 => "si".to_owned(),
//         0b101 => "di".to_owned(),
//         0b110 => "bp".to_owned(),
//         0b111 => "bx".to_owned(),
//         _ => panic!("unknown rm match"),
//     };
//
//     if displacement > 0 {
//         format!("[{result} + {displacement}]")
//     } else {
//         format!("[{result}]")
//     }
// }
fn find_opcode<'a>(
    byte: u8,
    instruction_table: &'a HashMap<u8, InstructionType>,
) -> &'a InstructionType {
    let opcodes = vec![
        (byte & 0b1111_0000) >> 4,
        (byte & 0b1111_1000) >> 3,
        (byte & 0b1111_1100) >> 2,
        (byte & 0b1111_1110) >> 1,
        byte,
    ];

    for opcode in opcodes {
        match instruction_table.get(&opcode) {
            Some(instruction) => return instruction,
            _ => (),
        }
    }
    panic!("unknown instruction {:#b}", byte)
}

static BIT_MASKS: [u8; 9] = [
    0b00000000, 0b10000000, 0b11000000, 0b11100000, 0b11110000, 0b11111000, 0b11111100, 0b11111110,
    0b11111111,
];

fn extract_fields(byte_count: usize, cursor: usize, bytes: &[u8], fields: &Vec<Field>) -> Vec<u8> {
    let mut local_cursor = 0;
    let mut base = bytes[local_cursor];
    let mut width_sum = 0;
    let mut values = vec![];

    for &field in fields {
        let width = field.width as usize;
        width_sum += width;

        if width_sum > 8 {
            local_cursor += 1;
            if local_cursor >= byte_count.into() {
                break;
            }
            width_sum = 0;
            base = bytes[cursor + local_cursor];
            println!("target {:#b}", base);
        }

        let value = (BIT_MASKS[width] & base) >> (8 - width);
        values.push(value);
        base = base << width;
    }

    values
}

fn decode_bytes(bytes: Vec<u8>) -> String {
    let mut cursor: usize = 0;

    let mut instruction_table: HashMap<u8, InstructionType> = HashMap::new();

    instruction_table.insert(0b100010, InstructionType::MovRegMem);
    instruction_table.insert(0b1011, InstructionType::MovImmToReg);

    while cursor < bytes.len() {
        let byte = bytes[cursor];
        println!("target 1 {:#b}", byte);
        println!("target 2 {:#b}", bytes[cursor +1]);
        let instruction_type = find_opcode(byte, &instruction_table);

        match instruction_type {
            InstructionType::MovRegMem => {
                let byte_count: usize = 2;
                let fields = MovRegMem::fields();
                let values = extract_fields(byte_count, cursor, &bytes, &fields);

                for (i, value) in values.iter().enumerate() {
                    println!("{:?} {:08b}", fields[i].label, value);
                }

                let instr = MovRegMem {
                    opcode: values[0],
                    d: values[1],
                    w: values[2],
                    r#mod: Register::from_encoding(values[3], values[2]),
                    reg: Register::from_encoding(values[4], values[2]),
                    rm: values[5],
                    disp_lo: None,
                    displ_hi: None,
                    data: None,
                };
                dbg!(instr);

                cursor += byte_count - 1
            }
            InstructionType::MovImmToReg => {
                todo!()
            }
        }

        // println!("{:#b}", bytes[cursor]);
        cursor += 1;
    }

    todo!()
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
