import re
import sys

replacements = {
    r'(\.opt|OPT.*)': r';\1',  # P% to * -- P% A special symbol which returns the current address being assembled
    r'inc\.PC': r'inc_PC',
    r'Chip8\.([0-9A-F]XXX)': r'Chip8_\1',
    r'C8\.': r'C8_',
    r'C8.8XXX\.JumpTable': 'C8_8XXX_JumpTable',
    r'dl\.': 'dl_',
    r'temp\.': 'temp_',
    
    r'\.include\#(.*$)': r" icl '\1'",
    r'\.incbin\#(.*$)': r" ins '\1'",
    r'\*\s*\=': r'org ',
    r'moveq\.b\s*(.*)\,(.*)': r'mva #\1 \2',
    r'moveq\.w (.*)\,(.*)': r'mwa #\1 \2',
    r'move\.w (.*)\,(.*)': r'mwa \1 \2',
    r'sub\.q\s*(\w+)\s*\,\s*(\w+)': r'sbw \2 #\1',
    r'add\.q\s*(\w+)\s*\,\s*(\w+)': r'adw \2 #\1',
    r'dcmp\.q\s*(\w+)\s*\,\s*(\w+)': r'cpw \2 #\1',
    r'lsr [aA](\s)': r'lsr\1',
    r'asl [aA](\s)': r'asl\1',
    r'rol [aA](\s)': r'rol\1',
    r'asl\.w': r'ASLW',
    r'\.sbyte "': r'dta d"',
    r'lbeq': 'jeq',
}


def apply_replacements(text, replacements):
    for pattern, replacement in replacements.items():
        text = re.sub(pattern, replacement, text, flags=re.MULTILINE)

    # ------------additional transformations------------
    # multiline comment
    #text = re.sub(r'(\*{10,}.*?\*{10,})', r'/\1/', text, flags=re.DOTALL)

    # add macros
    macros = """; -----MADS replacement macros-----
; -----end of MADS replacement macros-----
"""
    # text = macros + text
    return text


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        src = f.read()
    out = apply_replacements(src, replacements)
    print(out)
