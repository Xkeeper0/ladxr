from assembler import ASM


def fixChests(rom):
    # Patch the chest code, so it can give a lvl1 sword.
    # Normally, there is some code related to the owl event when getting the tail key,
    # as we patched out the owl. We use it to jump to our custom code in bank $3E to handle getting the item
    rom.patch(0x03, 0x109C, ASM("""
        cp $11 ; if not tail key, skip
        jr nz, end
        push af
        ld   a, [$C501]
        ld   e, a
        ld   hl, $C2F0
        add  hl, de
        ld   [hl], $38
        pop af
    end:
        ld   e, a
        cp   $21 ; if is message chest or higher number, next instruction is to skip giving things.
    """), ASM("""
        ld   a, $01
        call $3FF0

        and  a   ; clear the carry flag to always skip giving stuff.
    """), fill_nop=True)

    # Instead of the normal logic to on which sprite data to show, we jump to our custom code in bank 3E.
    rom.patch(0x07, 0x3C36, None, ASM("""
        ld   a, $00
        call $3FF0
        jp $7C5E
    """), fill_nop=True)

    # Instead of the normal logic of showing the proper dialog, we jump to our custom code in bank 3E.
    rom.patch(0x07, 0x3C9C, None, ASM("""
        ld   a, $02
        call $3FF0
        jp $7CE9
    """))