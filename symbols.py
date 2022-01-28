class Symbols:
    def __init__(self) -> None:
        self.start_addr = 0xC000
        self.next_addr = self.start_addr
        self.table = {}
        self.arrays = {}
        self.label = 0
        pass

    def shift_mem(self, bytes):
        last = self.next_addr
        self.next_addr += bytes
        return last

    def add(self, id, bytes):
        self.table[id] = self.shift_mem(bytes)

    def add_array(self, id, size):
        self.arrays[id] = size;
        self.table[id] = self.shift_mem(size);

    def get(self, id):
        return self.table[id]

    def get_label(self):
        label = self.label
        self.label += 1
        return label

    def array_list(self):
        return self.arrays.keys()

    def __getitem__(self, key):
        return self.table[key]

    def __str__(self):
        return str(self.table);


symbols_table = Symbols()
