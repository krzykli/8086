
static BIT_MASKS: [u8; 9] = [
    0b00000000, 0b10000000, 0b11000000, 0b11100000, 0b11110000, 0b11111000, 0b11111100, 0b11111110,
    0b11111111,
];

pub fn extract_fields(byte_count: u8, bytes: &[u8], widths: Vec<u8>) -> Vec<u8> {
    dbg!(widths.iter().sum::<u8>());
    dbg!(byte_count * 8);
    if widths.iter().sum::<u8>() != byte_count * 8 {
        dbg!(widths.iter().sum::<u8>());
        dbg!(byte_count);
        panic!("widths don't align with bytes");
    }

    let mut local_cursor = 0;
    let mut base = bytes[local_cursor];
    let mut width_sum = 0;
    let mut values = vec![];

    for width in widths {
        width_sum += width;

        if width_sum > 8 {
            width_sum = width;

            local_cursor += 1;
            if local_cursor >= byte_count.into() {
                break;
            }
            base = bytes[local_cursor];
        }

        let value = (BIT_MASKS[width as usize] & base) >> (8 - width);
        values.push(value);
        base = base << width;
    }

    values
}

#[test]
fn test_extract_fields_single_byte() {
    let bytes = vec![0b10101010];
    let widths = vec![4, 4];
    let result = extract_fields(1, &bytes, widths);
    assert_eq!(result, vec![0b1010, 0b1010]);
}

#[test]
fn test_extract_fields_multiple_bytes() {
    let bytes = vec![0b10101111, 0b11001100];
    let widths = vec![4, 4, 4, 4];
    let result = extract_fields(2, &bytes, widths);
    assert_eq!(result, vec![0b1010, 0b1111, 0b1100, 0b1100]);
}

#[test]
fn test_extract_fields_mixed_widths() {
    let bytes = vec![0b10101010, 0b11001100];
    let widths = vec![6, 2, 2, 4, 2];
    let result = extract_fields(2, &bytes, widths);
    assert_eq!(result, vec![0b101010, 0b10, 0b11, 0b0011, 0b00]);
}
