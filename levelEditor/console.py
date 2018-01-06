import tty
import termios
import sys


class Console:
    escape = chr(27)
    reset = escape + '[0m'
    UP = escape + '[A'
    DOWN = escape + '[B'
    RIGHT = escape + '[C'
    LEFT = escape + '[D'

    BLACK = '0'
    RED = '1'
    GREEN = '2'
    YELLOW = '3'
    BLUE = '4'
    MAGENTA = '5'
    CYAN = '6'
    WHITE = '7'

    def __init__(self):
        pass

    def clear(self):
        self.printText(self.escape + '[2J'),

    def printAt(self, x, y, text):
        self.printAtWithColour(x, y, text, self.WHITE)

    def printText(self, text):
        sys.stdout.write(text)

    def printWithColor(self, text, foregroundColor):
        self.printText(self.reset +
              self.escape + '[3' + foregroundColor + 'm' +
              text +
              self.reset),

    def setLineBackground(self, y, color, length):
        self.goto(0, y)
        self.clearLine();
        line = ''
        for i in range(length):
            line += ' '
        self.printText( 
            self.escape + '[4' + color + 'm' +
            line +
            self.reset)    
        
    def printAtWithColour(self, x, y, text, foreGroundColor):
        self.goto(x, y)
        self.printWithColor(text, foreGroundColor)

    def printWithColourAndBackground(self, text, foregroundColor, backgroundColor):
        self.printText(self.reset +
              self.escape + '[3' + foregroundColor + 'm' +
              self.escape + '[4' + backgroundColor + 'm' +
              text +
              self.reset)

    def printAtWithColourAndBackground(self, x, y, text, foregroundColor, backgroundColor):
        self.goto(x, y)
        self.printWithColourAndBackground(text, foregroundColor, backgroundColor)

    def clearLine(self):
        self.printText(self.escape + '[2K'),

    def goto(self, x, y):
        self.printText(self.escape + '[' + str(y) + ';' + str(x) + 'H'),

    def gotoLine(self, line):
        self.printText(self.escape + '[' + str(line) + ';0H'),
        self.clearLine()

    def getchar(self):
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(sys.stdin.fileno())
            ch = sys.stdin.read(1)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return ch
