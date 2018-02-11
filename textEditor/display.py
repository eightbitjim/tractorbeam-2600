from console import Console

class Display:
    def __init__(self, landscape, console):
        self.cursorX = 2
        self.cursorY = 2
        self.height = landscape.height
        self.width = 80
        self.landscape = landscape
        self.console = console
        self.console.clear()

    def setCursorPosition(self, x, y):
        self.cursorX = x
        self.cursorY = y

    def getCursorPosition(self):
        return self.cursorX, self.cursorY

    def draw(self):
        self.console.setLineBackground(0, Console.RED, self.width)
        self.console.printAtWithColourAndBackground(0, 0, 'Tractor Beam 2600 Text Editor', Console.WHITE, Console.RED)
        for y in range(self.height):
            self.printLine(y)

        self.console.setLineBackground(self.height + 2, Console.RED, self.width)
        self.console.printAtWithColourAndBackground(0, self.height + 2, 's: Set, c: Clear, v: Clear line, a: Copy line above, q: Output code and QUIT', Console.WHITE, Console.RED)

    def printLine(self, line):
        self.console.gotoLine(line + 2)
        for x in range(self.width / 2):
            if line < 0 or line >= self.landscape.height or x < 0 or x >= self.landscape.width:
                self.printCell(x, line, '  ', self.console.BLACK)
            else:
                if self.landscape.isSet(x, line):
                    self.printCell(x, line, '  ', self.console.BLUE)
                else:
                    self.printCell(x, line, '  ', self.console.BLACK)

    def printCell(self, x, y, text, background):
        if x == self.cursorX and y == self.cursorY:
            self.console.printWithColourAndBackground(text, self.console.YELLOW, self.console.GREEN)
        else:
            self.console.printWithColourAndBackground(text, self.console.RED, background)

