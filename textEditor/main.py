from landscape import Landscape
from console import Console
from display import Display
from controller import Controller

class Main:

    def __init__(self):
        self.console = Console()
        self.landscape = Landscape()
        self.display = Display(self.landscape, self.console)
        self.control = Controller(self.console)
        self.cursorX = 2
        self.cursorY = 2

    def run(self):
        exit = False
        while not exit:
            self.display.setCursorPosition(self.cursorX, self.cursorY)
            self.display.draw()
            userinput = self.control.getKey()
            if userinput == self.control.QUIT:
                exit = True
            else:
                self.processinput(self.control.buffer)

        self.console.gotoLine(self.landscape.height + 3)
        self.console.printText(self.landscape.generateCode())


    def processinput(self, command):
        if command.endswith(self.control.UP) and self.cursorY > 0:
            self.cursorY = self.cursorY - 1

        if command.endswith(self.control.DOWN) and self.cursorY < self.landscape.height - 1:
            self.cursorY = self.cursorY + 1

        if command.endswith(self.control.LEFT) and self.cursorX > 0:
            self.cursorX = self.cursorX - 1

        if command.endswith(self.control.RIGHT) and self.cursorX < self.landscape.width - 1:
            self.cursorX = self.cursorX + 1

        if command.endswith(self.control.SET):
            self.landscape.set(self.cursorX, self.cursorY)

        if command.endswith(self.control.CLEAR):
            self.landscape.clear(self.cursorX, self.cursorY)

        if command.endswith(self.control.CLEARLINE):
            self.landscape.clearLine(self.cursorY)

        if command.endswith(self.control.COPYLINEABOVE) and self.cursorY > 0:
            self.landscape.copyLine(self.cursorY - 1, self.cursorY)

main = Main()
main.run()
