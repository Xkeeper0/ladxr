from assembler import ASM
from utils import formatText, setReplacementName
from roomEditor import RoomEditor
import entityData
import os


def imageTo2bpp(filename):
    import PIL.Image
    img = PIL.Image.open(filename)
    assert (img.size[0] % 8) == 0
    tileheight = 8 if img.size[1] == 8 else 16
    assert (img.size[1] % tileheight) == 0

    cols = img.size[0] // 8
    rows = img.size[1] // tileheight
    result = bytearray(rows * cols * tileheight * 2)
    index = 0
    for ty in range(rows):
        for tx in range(cols):
            for y in range(tileheight):
                a = 0
                b = 0
                for x in range(8):
                    c = img.getpixel((tx * 8 + x, ty * 16 + y))
                    if c & 1:
                        a |= 0x80 >> x
                    if c & 2:
                        b |= 0x80 >> x
                result[index] = a
                result[index+1] = b
                index += 2
    return result


def updateGraphics(rom, bank, offset, data):
    if offset + len(data) > 0x4000:
        updateGraphics(rom, bank, offset, data[:0x4000-offset])
        updateGraphics(rom, bank + 1, 0, data[0x4000 - offset:])
    else:
        rom.banks[bank][offset:offset+len(data)] = data
        if bank < 0x34:
            rom.banks[bank-0x20][offset:offset + len(data)] = data


def gfxMod(rom, filename):
    if os.path.exists(filename + ".names"):
        for line in open(filename + ".names", "rt"):
            if ":" in line:
                k, v = line.strip().split(":", 1)
                setReplacementName(k, v)

    ext = os.path.splitext(filename)[1].lower()
    if ext == ".bin":
        updateGraphics(rom, 0x2C, 0, open(filename, "rb").read())
    elif ext in (".png", ".bmp"):
        updateGraphics(rom, 0x2C, 0, imageTo2bpp(filename))
    elif ext == ".json":
        import json
        data = json.load(open(filename, "rt"))

        for patch in data:
            if "gfx" in patch:
                updateGraphics(rom, int(patch["bank"], 16), int(patch["offset"], 16), imageTo2bpp(os.path.join(os.path.dirname(filename), patch["gfx"])))
            if "name" in patch:
                setReplacementName(patch["item"], patch["name"])
    else:
        updateGraphics(rom, 0x2C, 0, imageTo2bpp(filename))


def createGfxImage(rom, filename):
    import PIL.Image
    bank_count = 8
    img = PIL.Image.new("P", (32 * 8, 32 * 8 * bank_count))
    img.putpalette((
        128, 0, 128,
        0, 0, 0,
        128, 128, 128,
        255, 255, 255,
    ))
    for bank_nr in range(bank_count):
        bank = rom.banks[0x2C + bank_nr]
        for tx in range(32):
            for ty in range(16):
                for y in range(16):
                    a = bank[tx * 32 + ty * 32 * 32 + y * 2]
                    b = bank[tx * 32 + ty * 32 * 32 + y * 2 + 1]
                    for x in range(8):
                        c = 0
                        if a & (0x80 >> x):
                            c |= 1
                        if b & (0x80 >> x):
                            c |= 2
                        img.putpixel((tx*8+x, bank_nr * 32 * 8 + ty*16+y), c)
    img.save(filename)


def noSwordMusic(rom):
    # Skip no-sword music override
    # Instead of loading the sword level, we put the value 1 in the A register, indicating we have a sword.
    rom.patch(2, 0x0151, ASM("ld a, [$DB4E]"), ASM("ld a, $01"), fill_nop=True)
    rom.patch(2, 0x3AEF, ASM("ld a, [$DB4E]"), ASM("ld a, $01"), fill_nop=True)
    rom.patch(3, 0x0996, ASM("ld a, [$DB4E]"), ASM("ld a, $01"), fill_nop=True)
    rom.patch(3, 0x0B35, ASM("ld a, [$DB44]"), ASM("ld a, $01"), fill_nop=True)


def removeNagMessages(rom):
    # Remove "this object is heavy, bla bla", and other nag messages when touching an object
    rom.patch(0x02, 0x32BB, ASM("ld a, [$C14A]"), ASM("ld a, $01"), fill_nop=True)  # crystal blocks
    rom.patch(0x02, 0x32EC, ASM("ld a, [$C5A6]"), ASM("ld a, $01"), fill_nop=True) # cracked blocks
    rom.patch(0x02, 0x32D3, ASM("jr nz, $25"), ASM("jr $25"), fill_nop=True)  # stones/pots
    rom.patch(0x02, 0x2B88, ASM("jr nz, $0F"), ASM("jr $0F"), fill_nop=True)  # ice blocks


def removeLowHPBeep(rom):
    rom.patch(2,  0x233A, ASM("ld hl, $FFF3\nld [hl], $04"), b"", fill_nop=True) # Remove health beep


def slowLowHPBeep(rom):
    rom.patch(2, 0x2338, ASM("ld a, $30"), ASM("ld a, $60"))  # slow slow hp beep


def removeFlashingLights(rom):
    # Remove the switching between two backgrounds at mamu, always show the spotlights.
    rom.patch(0x00, 0x01EB, ASM("ldh a, [$E7]\nrrca\nand $80"), ASM("ld a, $80"), fill_nop=True)
    # Remove flashing colors from shopkeeper killing you after stealing and the mad batter giving items.
    rom.patch(0x24, 0x3B77, ASM("push bc"), ASM("ret"))


def forceLinksPalette(rom, index):
    # This forces the link sprite into a specific palette index ignoring the tunic options.
    rom.patch(0, 0x1D8C,
            ASM("ld a, [$DC0F]\nand a\njr z, $03\ninc a"),
            ASM("ld a, $%02X" % (index)), fill_nop=True)
    rom.patch(0, 0x1DD2,
            ASM("ld a, [$DC0F]\nand a\njr z, $03\ninc a"),
            ASM("ld a, $%02X" % (index)), fill_nop=True)


def fastText(rom):
    rom.patch(0x00, 0x24CA, ASM("jp $2485"), ASM("call $2485"))


def noText(rom):
    for idx in range(len(rom.texts)):
        if not isinstance(rom.texts[idx], int) and (idx < 0x217 or idx > 0x21A):
            rom.texts[idx] = rom.texts[idx][-1:]

def turboText(rom):
    # Remove the calls to message popups for the more useless things
    # as a balance between no text at all and normal fast text

    # remove message popups on guardian acorns, pieces of power
    rom.patch(0x00, 0x105C, ASM("call $2385"), "", fill_nop=True)
    # Level # - Who Cares
    rom.patch(0x01, 0x21FF, ASM("call $2385"), "", fill_nop=True)

    # Remove instructions from fishing game
    # ...when starting
    rom.patch(0x04, 0x2032, ASM("""
        ld a, $47
        jp $2385
    """), ASM("ret"), fill_nop=True)
    # ...when continuing  (todo: check if question being disabled is normal)
    rom.patch(0x04, 0x622D, ASM("""
        ld a, $47
        call $2385
    """), "", fill_nop=True)

    # Great Fairy healing fountains
    rom.patch(0x06, 0x3180, None, ASM("ret"))

    # Mad Batter
    rom.patch(0x18, 0x0F8A, ASM("call $5080"), "", fill_nop = True)


def reduceMessageLengths(rom, rnd):
    # Into text from Marin. Got to go fast, so less text. (This intro text is very long)
    rom.texts[0x01] = formatText(rnd.choice([
        "Let's a go!",
        "Remember, sword goes on A!",
        "Avoid the heart piece of shame!",
        "Marin? No, this is Zelda. Welcome to Hyrule",
        "Why are you in my bed?",
        "This is not a Mario game!",
        "MuffinJets was here...",
        "Remember, there are no bugs in LADX",
        "#####, #####, you got to wake up!\nDinner is ready.",
        "Go find the stepladder",
        "Pizza power!",
        "Eastmost penninsula is the secret",
        "There is no cow level",
        "You cannot lift rocks with your bear hands",
    ]))

    # Reduce length of a bunch of common texts
    rom.texts[0xEA] = formatText("You've got a Guardian Acorn!")
    rom.texts[0xEB] = rom.texts[0xEA]
    rom.texts[0xEC] = rom.texts[0xEA]
    rom.texts[0x08] = formatText("You got a Piece of Power!")
    rom.texts[0xEF] = formatText("You found a {SEASHELL}!")
    rom.texts[0xA7] = formatText("You've got the {COMPASS}!")

    rom.texts[0x07] = formatText("You need the {NIGHTMARE_KEY}!")
    rom.texts[0x8C] = formatText("You need a {KEY}!")  # keyhole block

    rom.texts[0x09] = formatText("What will your {TOADSTOOL} become?")
    rom.texts[0x0A] = formatText("burp {TOADSTOOL} fart raccoon!\nbye")
    rom.texts[0x0F] = formatText("You pick the {TOADSTOOL}!")
    rom.texts[0x13] = formatText("You've learned the ^{SONG1}!^")
    rom.texts[0x18] = formatText("Medicine, 28 {RUPEES}?", ask="Give Don't")
    rom.texts[0x19] = formatText("Medicine, 42 {RUPEES}?", ask="Give Don't")
    rom.texts[0x1e] = formatText("...You're so cute! I'll give you a 7 {RUPEE} discount!")
    rom.texts[0x2d] = formatText("{ARROWS_10}\n10 {RUPEES}!", ask="Buy  Don't")
    rom.texts[0x32] = formatText("{SHIELD}\n20 {RUPEES}!", ask="Buy  Don't")
    rom.texts[0x33] = formatText("Ten {BOMB}\n10 {RUPEES}", ask="Buy  Don't")
    rom.texts[0x38] = formatText("The castle gate has been opened!")
    rom.texts[0x3d] = formatText("It's a {SHIELD}!")
    rom.texts[0x42] = formatText("It's 30 {RUPEES}!")
    rom.texts[0x45] = formatText("Fishing pond,   10 {RUPEES}!", ask="Fish Nah")
    rom.texts[0x4b] = formatText("It's a lunker!! 20 {RUPEES}!\nTry again?", ask="Cast Nah")
    #                            |0123456789abcde|0123456789abcde|0123456789abcde|0123456789abcde|
    rom.texts[0x4d] = formatText("It's a runt!    Only 5 {RUPEES}!")
    rom.texts[0x4e] = formatText("You need more {RUPEES}!")
    rom.texts[0x4f] = formatText("You've got a {HEART_PIECE}!")
    rom.texts[0x8e] = formatText("You don't know  any songs...")
    rom.texts[0x90] = formatText("You found the {POWER_BRACELET}!")
    rom.texts[0x91] = formatText("You found your {SHIELD}!")
    rom.texts[0x93] = formatText("You've got the {HOOKSHOT}!")
    rom.texts[0x94] = formatText("You've got the {MAGIC_ROD}!")
    rom.texts[0x95] = formatText("You've got the {PEGASUS_BOOTS}!")
    rom.texts[0x96] = formatText("You've got the {OCARINA}!")
    rom.texts[0x97] = formatText("You've got the {FEATHER}!")
    rom.texts[0x98] = formatText("You've got a {SHOVEL}!")
    rom.texts[0x99] = formatText("You've got some {MAGIC_POWDER}!")
    rom.texts[0x9b] = formatText("You found your {SWORD}!")
    rom.texts[0x9c] = formatText("You've got the {FLIPPERS}!")
    rom.texts[0x9e] = formatText("You found a better {SWORD}!")
    rom.texts[0x9f] = formatText("You found a better {SWORD}!")
    rom.texts[0xa0] = formatText("You found some {MEDICINE}!")
    rom.texts[0xa1] = formatText("You've got the {TAIL_KEY}!")
    rom.texts[0xa2] = formatText("You've got the {SLIME_KEY}!")
    rom.texts[0xa3] = formatText("You've got the {ANGLER_KEY}!")
    rom.texts[0xa4] = formatText("You've got the {FACE_KEY}!")
    rom.texts[0xa5] = formatText("You've got the {BIRD_KEY}!")
    rom.texts[0xa6] = formatText("You found the {MAP}!")
    rom.texts[0xa8] = formatText("You found the {STONE_BEAK}!")
    rom.texts[0xa9] = formatText("You've got the {NIGHTMARE_KEY}!")
    rom.texts[0xaa] = formatText("You got a {KEY}!")
    rom.texts[0xab] = formatText("You got 20 {RUPEES}! JOY!", center=True)
    rom.texts[0xac] = formatText("You got 50 {RUPEES}! Very Nice!", center=True)
    rom.texts[0xad] = formatText("You got 100 {RUPEES}! You're Happy!", center=True)
    rom.texts[0xae] = formatText("You got 200 {RUPEES}! You're Ecstatic!", center=True)
    rom.texts[0xdc] = formatText("Listen to Mamu?", ask="Pay  Leave")
    rom.texts[0xe8] = formatText("You've found a {GOLD_LEAF}!")
    rom.texts[0xed] = formatText("You've got the Mirror Shield!")
    rom.texts[0xee] = formatText("You've got a stronger {POWER_BRACELET}!")
    rom.texts[0xf0] = formatText("Ride the raft   for 100 {RUPEES}?", ask="Yes  No Way")


def allowColorDungeonSpritesEverywhere(rom):
    # Set sprite set numbers $01-$40 to map to the color dungeon sprites
    rom.patch(0x00, 0x2E6F, "00", "15")
    # Patch the spriteset loading code to load the 4 entries from the normal table instead of skipping this for color dungeon specific exception weirdness
    rom.patch(0x00, 0x0DA4, ASM("jr nc, $05"), ASM("jr nc, $41"))
    rom.patch(0x00, 0x0DE5, ASM("""
        ldh  a, [$F7]
        cp   $FF
        jr   nz, $06
        ld a, $01
        ldh [$91], a
        jr $40
    """), ASM("""
        jr $0A ; skip over the rest of the code
        cp $FF ; check if color dungeon
        jp nz, $0DAB
        inc d
        jp $0DAA
    """), fill_nop=True)
    # Disable color dungeon specific tile load hacks
    rom.patch(0x00, 0x06A7, ASM("jr nz, $22"), ASM("jr $22"))
    rom.patch(0x00, 0x2E77, ASM("jr nz, $0B"), ASM("jr $0B"))
    
    # Finally fill in the sprite data for the color dungeon
    for n in range(22):
        data = bytearray()
        for m in range(4):
            idx = rom.banks[0x20][0x06AA + 44 * m + n * 2]
            bank = rom.banks[0x20][0x06AA + 44 * m + n * 2 + 1]
            if idx == 0 and bank == 0:
                v = 0xFF
            elif bank == 0x35:
                v = idx - 0x40
            elif bank == 0x31:
                v = idx
            elif bank == 0x2E:
                v = idx + 0x40
            else:
                assert False, "%02x %02x" % (idx, bank)
            data += bytes([v])
        rom.room_sprite_data_indoor[0x200 + n] = data

    # Patch the graphics loading code to use DMA and load all sets that need to be reloaded, not just the first and last
    rom.patch(0x00, 0x06FA, 0x07AF, ASM("""
        ;We enter this code with the right bank selected for tile data copy,
        ;d = tile row (source addr = (d*$100+$4000))
        ;e = $00
        ;$C197 = index of sprite set to update (target addr = ($8400 + $100 * [$C197]))
        ld  a, d
        add a, $40
        ldh [$51], a
        xor a
        ldh [$52], a
        ldh [$54], a
        ld  a, [$C197]
        add a, $84
        ldh [$53], a
        ld  a, $0F
        ldh [$55], a

        ; See if we need to do anything next
        ld  a, [$C10E] ; check the 2nd update flag
        and a
        jr  nz, getNext
        ldh [$91], a ; no 2nd update flag, so clear primary update flag
        ret
    getNext:
        ld  hl, $C197
        inc [hl]
        res 2, [hl]
        ld  a, [$C10D]
        cp  [hl]
        ret nz
        xor a ; clear the 2nd update flag when we prepare to update the last spriteset
        ld  [$C10E], a
        ret
    """), fill_nop=True)
    rom.patch(0x00, 0x0738, "00" * (0x073E - 0x0738), ASM("""
        ; we get here by some color dungeon specific code jumping to this position
        ; We still need that color dungeon specific code as it loads background tiles
        xor a
        ldh [$91], a
        ldh [$93], a
        ret
    """))
    rom.patch(0x00, 0x073E, "00" * (0x07AF - 0x073E), ASM("""
        ;If we get here, only the 2nd flag is filled and the primary is not. So swap those around.
        ld  a, [$C10D] ;copy the index number
        ld  [$C197], a
        xor a
        ld  [$C10E], a ; clear the 2nd update flag
        inc a
        ldh [$91], a ; set the primary update flag
        ret
    """), fill_nop=True)


def updateSpriteData(rom):
    # Remove all the special sprite change exceptions.
    rom.patch(0x00, 0x0DAD, 0x0DDB, ASM("jp $0DDB"), fill_nop=True)

    # For each room update the sprite load data based on which entities are in there.
    for room_nr in range(0x316):
        if room_nr == 0x2FF:
            continue
        values = [None, None, None, None]
        if room_nr == 0x00E:  # D7 entrance opening
            values[2] = 0xD6
            values[3] = 0xD7
        if 0x211 <= room_nr <= 0x21E:  # D7 throwing ball thing.
            values[0] = 0x66
        r = RoomEditor(rom, room_nr)
        for obj in r.objects:
            if obj.type_id == 0xC5 and room_nr < 0x100: # Pushable Gravestone
                values[3] = 0x82
        for x, y, entity in r.entities:
            sprite_data = entityData.SPRITE_DATA[entity]
            if callable(sprite_data):
                sprite_data = sprite_data(r)
            if sprite_data is None:
                continue
            for m in range(0, len(sprite_data), 2):
                idx, value = sprite_data[m:m+2]
                if values[idx] is None:
                    values[idx] = value
                elif isinstance(values[idx], set) and isinstance(value, set):
                    values[idx] = values[idx].intersection(value)
                    assert len(values[idx]) > 0
                elif isinstance(values[idx], set) and value in values[idx]:
                    values[idx] = value
                elif isinstance(value, set) and values[idx] in value:
                    pass
                elif values[idx] == value:
                    pass
                else:
                    assert False, "Room: %03x cannot load graphics for entity: %02x (Index: %d Failed: %s, Active: %s)" % (room_nr, entity, idx, value, values[idx])

        data = bytearray()
        for v in values:
            if isinstance(v, set):
                v = next(iter(v))
            elif v is None:
                v = 0xff
            data.append(v)

        if room_nr < 0x100:
            rom.room_sprite_data_overworld[room_nr] = data
        else:
            rom.room_sprite_data_indoor[room_nr - 0x100] = data
