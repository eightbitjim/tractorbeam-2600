from squareType import SquareType

class Landscape:
    defaultWidth = 40
    pixelsPerLine = 8
    defaultHeight = 256 / pixelsPerLine

    def __init__(self):
        self.width = self.defaultWidth
        self.internalWidth = self.width
        self.height = self.defaultHeight
        self.grid = self.initialiseGrid()
        self.clearGrid()
        self.level = 0

    def initialiseGrid(self):
        gridunderconstruction = [[0] * self.internalWidth for i in range(self.height)]
        return gridunderconstruction

    def clearGrid(self):
        for x in range(self.internalWidth):
            for y in range(self.height):
                self.clear(x, y)

    def tostring(self):
        for x in range(self.width):
            for y in range(self.height):
                print(self.grid[y][self.internalColumnFor(x)])
            print ''

    def set(self, x, y):
        self.grid[y][self.internalColumnFor(x)] = SquareType.ROCK

    def clear(self, x, y):
        self.grid[y][self.internalColumnFor(x)] = SquareType.EMPTY

    def isSet(self, x, y):
        return self.grid[y][self.internalColumnFor(x)] != SquareType.EMPTY

    def clearLine(self, y):
        for x in range(self.internalWidth):
            self.clear(x, y)

    def copyLine(self, sourceY, destinationY):
        for x in range(self.internalWidth):
            if self.isSet(x, sourceY):
                self.set(x, destinationY)
            else:
                self.clear(x, destinationY)

    def internalColumnFor(self, x):
        return x

    def generateCode(self):
        code = ''
        code += '; data for text block ' + str(self.level) + '\n\n'

        lastline = ''
        scanline = 0
        codeline = ['', '', '', '', '', '', '']
        for line in range(7):
            codeline[line] = '    dc.b '

        for y in range(0, self.height):
            pf0 = self.makecodelinepart(y, 0)
            pf1 = self.makecodelinepart(y, 1)
            pf2 = self.makecodelinepart(y, 2)
            pf3 = self.makecodelinepart(y, 3)
            pf4 = self.makecodelinepart(y, 4)
            pf5 = self.makecodelinepart(y, 5)
            line = pf0 + pf1 + pf2 + pf3 + pf4 + pf5

            if scanline > 255:
                scanline = 255
                
            if (line != lastline):
                codeline[0] += pf0
                codeline[1] += pf1
                codeline[2] += pf2
                codeline[3] += pf3
                codeline[4] += pf4
                codeline[5] += pf5
                if scanline > 0:
                    codeline[6] += str(scanline) + ', '

            lastline = line
            scanline += self.pixelsPerLine

        codeline[6] += '255, '

        code += 'text' + str(self.level) + 'Start0\n'
        code += codeline[0][:-2] + '\n'
        code += 'text' + str(self.level) + 'Start1\n'
        code += codeline[1][:-2] + '\n'
        code += 'text' + str(self.level) + 'Start2\n'
        code += codeline[2][:-2] + '\n'
        code += 'text' + str(self.level) + 'Start3\n'
        code += codeline[3][:-2] + '\n'
        code += 'text' + str(self.level) + 'Start4\n'
        code += codeline[4][:-2] + '\n'
        code += 'text' + str(self.level) + 'Start5\n'
        code += codeline[5][:-2] + '\n'
        code += 'text' + str(self.level) + 'NextLine\n'
        code += codeline[6][:-2] + '\n'

        return code

    def makecodelinepart(self, y, playfieldbyte):
        code = '%'

        if playfieldbyte == 0:
            for x in range(3, -1, -1):
                if self.isSet(x, y):
                    code += '1'
                else:
                    code += '0'
            code += '0000, '

        if playfieldbyte == 1:
            for x in range(4, 12):
                if self.isSet(x, y):
                    code += '1'
                else:
                    code += '0'
            code += ', '

        if playfieldbyte == 2:
            for x in range(19, 11, -1):
                if self.isSet(x, y):
                    code += '1'
                else:
                    code += '0'
            code += ', '

        if playfieldbyte == 3:
            for x in range(20, 28):
                if self.isSet(x, y):
                    code += '1'
                else:
                    code += '0'
            code += ', '

        if playfieldbyte == 4:
            for x in range(35, 27, -1):
                if self.isSet(x, y):
                    code += '1'
                else:
                    code += '0'
            code += ', '

        if playfieldbyte == 5:
            for x in range(36, 40):
                if self.isSet(x, y):
                    code += '1'
                else:
                    code += '0'
            code += '0000, '

        return code
