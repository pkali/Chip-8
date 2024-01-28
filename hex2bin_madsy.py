import re

data16 = """
    .byte $F0, $F0, $90, $90, $90, $90, $90, $90, $F0, $F0 ; 0
    .byte $20, $20, $60, $60, $20, $20, $20, $20, $70, $70 ; 1
    .byte $F0, $F0, $10, $10, $F0, $F0, $80, $80, $F0, $F0 ; 2
    .byte $F0, $F0, $10, $10, $F0, $F0, $10, $10, $F0, $F0 ; 3
    .byte $90, $90, $90, $90, $F0, $F0, $10, $10, $10, $10 ; 4
    .byte $F0, $F0, $80, $80, $F0, $F0, $10, $10, $F0, $F0 ; 5
    .byte $F0, $F0, $80, $80, $F0, $F0, $90, $90, $F0, $F0 ; 6
    .byte $F0, $F0, $10, $10, $20, $20, $40, $40, $40, $40 ; 7
    .byte $F0, $F0, $90, $90, $F0, $F0, $90, $90, $F0, $F0 ; 8
    .byte $F0, $F0, $90, $90, $F0, $F0, $10, $10, $F0, $F0 ; 9
    .byte $F0, $F0, $90, $90, $F0, $F0, $90, $90, $90, $90 ; A
    .byte $E0, $E0, $90, $90, $E0, $E0, $90, $90, $E0, $E0 ; B
    .byte $F0, $F0, $80, $80, $80, $80, $80, $80, $F0, $F0 ; C
    .byte $E0, $E0, $90, $90, $90, $90, $90, $90, $E0, $E0 ; D
    .byte $F0, $F0, $80, $80, $F0, $F0, $80, $80, $F0, $F0 ; E
    .byte $F0, $F0, $80, $80, $F0, $F0, $80, $80, $80, $80 ; F
"""
data = """    .byte $F0, $90, $90, $90, $F0 ; 0
    .byte $20, $60, $20, $20, $70 ; 1
    .byte $F0, $10, $F0, $80, $F0 ; 2
    .byte $F0, $10, $F0, $10, $F0 ; 3
    .byte $90, $90, $F0, $10, $10 ; 4
    .byte $F0, $80, $F0, $10, $F0 ; 5
    .byte $F0, $80, $F0, $90, $F0 ; 6
    .byte $F0, $10, $20, $40, $40 ; 7
    .byte $F0, $90, $F0, $90, $F0 ; 8
    .byte $F0, $90, $F0, $10, $F0 ; 9
    .byte $F0, $90, $F0, $90, $90 ; A
    .byte $E0, $90, $E0, $90, $E0 ; B
    .byte $F0, $80, $80, $80, $F0 ; C
    .byte $E0, $90, $90, $90, $E0 ; D
    .byte $F0, $80, $F0, $80, $F0 ; E
    .byte $F0, $80, $F0, $80, $80 ; F"""
# Split the data into lines
lines = data.strip().split('\n')

# Process each line
converted_lines = []
for line in lines:
    # Find all hexadecimal values in the line
    hex_values = re.findall(r'\$[0-9A-Fa-f]+', line)
    # Extract the character after ';'
    char_after_semicolon = re.search(r';\s*(.)', line)
    if char_after_semicolon:
        char_after_semicolon = char_after_semicolon.group(1)
        # Add a header for each block
        converted_lines.append(f'; ---- {char_after_semicolon} ----')

    # Convert each hex value to binary and format it
    for hex_value in hex_values:
        # Convert hex to binary, removing '0b' prefix and padding to 8 bits
        binary_value = format(int(hex_value[1:], 16), '08b')
        converted_lines.append(f'  .byte %{binary_value}')

# Join the converted lines into a single string
converted_data = '\n'.join(converted_lines)

print(converted_data)

