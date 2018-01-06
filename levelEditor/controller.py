
class Controller:
    escape = chr(27)
    UP = escape + '[A'
    DOWN = escape + '[B'
    RIGHT = escape + '[C'
    LEFT = escape + '[D'
    QUIT = 'q'
    SET = 's'
    CLEAR = 'c'
    CLEARLINE = 'v'
    COPYLINEABOVE = 'a'

    def __init__(self, console):
        self.console = console
        self.buffer = '    '

    def getKey(self):
        key = self.console.getchar()
        self.buffer = self.buffer + key
        self.buffer = self.buffer[1::]
        return key
