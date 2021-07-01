# ApbRegsiter
a APB3 BUS with auto register generate model
spinalhdl file ----------------- ./src/main.scala/ApbRegister.scala
generate file ------------------ ./Apb3_inst.v
csv file ----------------------- ./register.csv

format fo csv file(Reg as Register,field begin with the lowest address)

Reg Address | Reg name | Reg Field Number | Field1 Name | Field1 width| Field1 read-write property | Field1 reset value | Field1 Special Control | Field2 Name | ... |Field 3


Field Special Control
W:write/R:read | C:clear/S:set | Other Reg Address | Other Reg Field | Write data


