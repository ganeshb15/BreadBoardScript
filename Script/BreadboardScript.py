import ply.lex as lex
import ply.yacc as yacc
import logging
from dataclasses import dataclass, field
from typing import List, Optional
import re

class MyParser:
    @dataclass
    class ALUOperator:
        OpName: str
        OpList: List[str]

    @dataclass
    class ALUDetail:
        ALUBit: str
        ALUName: str
        ALUBUS:str
        ALUOperatorList: List['MyParser.ALUOperator'] = field(default_factory=list)
    def __init__(self):

        self.MY_CONSTANT = 42
        self.REGISTER_BIT_LOW = 1
        self.REGISTER_BIT_HIGH = 32
        self.RAM_BIT_LOW = 1
        self.RAM_BIT_HIGH = 32
        self.RAM_OPTIMIZATION = 1
        self.ALUCurrentIndex=0
        self.VariableName=[]
        self.VariableType=[]
        self.ControlList=[]
        self.TTYControlList=[]
        self.TTYControlList.append("tty_S")
        self.TTYControlList.append("tty_E")
        self.TTYControlList.append("tty_L")
        self.TTYControlList.append("tty_M0")
        self.TTYControlList.append("tty_M1")
        self.TTYControlList.append("tty_IP")
        self.ControlList.append("CLK_1")
        self.StaticCounter=0
        self.StaticROM=[]
        self.CodeExplain=[]
        self.CodeSeq=[]
        self.HEXControlOPCode=[]
        self.TTYHEXControlOPCode=[]
        self.ifStart=[]
        self.ifend=[]
        self.whileEnd=[]
        self.whileStart=[]
        self.JP_Cnt=0
        self.JP_ROM=[]

        # Variable Declaration
        self.NameStack = []
        self.TTYStack = []
        self.TTYLen = []
        self.TTYROM = []
        self.RAM_Stack = []
        self.RAM_BUS_Stack=[]
        self.ALUInfo = []
        self.loc_ALUStack = []
        self.loc_ALUOpName = ''
        self.loc_ALUIdx=0
        self.loc_ALUWire = []
        self.loc_ALUNameClash=True;
        self.BUS_Name=[]
        self.loc_BUS_Name=[]
        self.loc_BUS_BitSize=[]
        self.Assembly=[]
        self.CondType=[]
        self.XMLStack = '<root>'
        # XML Templates
        self.register_xml_template = "\t<reg val_name=\"{name}\" val_bitsize=\"{bit_size}\" co_st_ip=\"Input_{name}\" co_cf_en=\"Ena_{name}\" co_st_clk=\"Globalclock\" co_cf_clr=\"Clr_{name}\" co_st_op=\"Out_{name}\"/>"
        self.ram_start_xml_template = "\t<ram val_name=\"RAM_{name}\" val_bitsize=\"{bit_size}\" co_st_address=\"RAM_{name}_ADDRESS\" co_st_indata=\"BUS_{busname}_OP\" co_st_outdata=\"RAM_Out_{name}\" co_st_clock=\"RAM_Clk_{name}\"  val_address_size=\"{address_size}\">"
        self.ram_address_xml_template = "\t\t<address val_name=\"{name}\" val_addressvalue=\"{address_value}\" />"
        self.XML_Ram_Buf = "\t<buffer val_name=\"Buff_{name}\" val_bitsize=\"{bit_size}\" co_st_ip=\"{input}\" co_cf_en=\"{enable}\"  co_st_op=\"{output}\"/>"
        self.ram_end_xml_template="\t</ram>"
        self.tty_display_xml_template = "\t<tty val_name=\"{name}\" co_cf_startaddress=\"StartAddress_{name}\"  co_cf_endaddress=\"EndAddress_{name}\" co_st_clk=\"Clock_{name}\" co_cf_load=\"Load_{name}\" co_cf_mode=\"Mode_{name}\" co_st_ip=\"Input_{name}\" co_st_op=\"Output_{name}\" co_st_outclk=\"Outclk_{name}\" />"
        self.XML_ALUStart = "\t<alu val_name=\"{name}\" val_bitsize=\"{bit_size}\">"
        self.XML_OperatorStart = "\t\t<operator val_name=\"{name}\">"
        self.XML_Adder = "\t\t\t<adder val_name=\"{name}\" val_bitsize=\"{bit_size}\" co_st_input1=\"{name}_Input1\" co_st_input2=\"{name}_Input2\" co_st_output=\"{name}_Output\"/>"
        self.XML_Sub = "\t\t\t<sub val_name=\"{name}\" val_bitsize=\"{bit_size}\" co_st_ip1=\"{name}_Input1\" co_st_ip2=\"{name}_Input2\" co_st_op=\"{name}_Output\"/>"
        self.XML_Mul = "\t\t\t<mul val_name=\"{name}\" val_bitsize=\"{bit_size}\" co_st_input1=\"{name}_Input1\" co_st_input2=\"{name}_Input2\" co_st_output=\"{name}_Output\"/>"
        self.XML_Div = "\t\t\t<mul val_name=\"{name}\" val_bitsize=\"{bit_size}\" co_st_input1=\"{name}_Input1\" co_st_input2=\"{name}_Input2\" co_st_output=\"{name}_Output\"/>"
        self.XML_Mux = "\t\t<mux val_name=\"{name}\" val_bitsize=\"{bit_size}\" "
        self.XML_Wire = "\t<wire co_st_source=\"{source}\" co_st_destination=\"{destination}\" />"
        self.XML_Bus_Start = "\t<bus val_name=\"{name}\" >"
        self.XML_Bus_Reg = "\t\t<reg val_name=\"BUS_REG_{name}\" val_bitsize=\"{bit_size}\" co_st_ip=\"BUS_{name}_CO \"co_cf_en=\"Ena_{name}\" co_st_clk=\"Globalclock\" co_cf_clr=\"Clr_{name}\" co_st_op=\"BUS_{name}_OP\"/>"
        self.XML_Bus_Mux = "\t\t<mux val_name=\"BUS_MUX_{name}\" val_bitsize=\"{bit_size}\" co_st_input0=\"BUS_{name}_ROM\" co_st_input1=\"BUS_{name}_IP\" val_bitwidth=\"1\" co_cf_selector=\"BUS_{name}_SEL\" co_st_Output=\"BUS_{name}_CO\"/>" 
        self.XML_Bus_ROM = "\t\t<rom val_name=\"BUS_ROM_{name}\" val_bitsize=\"{bit_size}\" co_st_add=\"BUS_ROM_{name}_Add\"co_cf_Output=\"BUS_{name}_ROM\"/>" 
        self.XML_Bus_ROM_Split="\t\t<splitter val_name=\"BUS_SPLIT_{name}\" co_cf_Input_1=\"SPLIT_1_{name}\" co_cf_Input_2=\"SPLIT_2_{name}\" co_cf_Input_3=\"SPLIT_3_{name}\" co_cf_Input_4=\"SPLIT_4_{name}\"  co_st_out=\"BUS_ROM_{name}_Add\"/>"
        self.XML_Bus_End = "\t</bus>"
        self.XML_Reg = "\t<reg val_name=\"{name}\" val_bitsize=\"{bit_size}\" co_st_ip=\"BUS_{busname}_OP\" co_cf_en=\"Ena_{name}\" co_st_clk=\"Globalclock\" co_cf_clr=\"Clr_{name}\" co_st_op=\"Out_{name}\"/>"
        self.XML_Reg_Buf = "\t<buffer val_name=\"Buff_{name}\" val_bitsize=\"{bit_size}\" co_st_ip=\"Out_{name}\" co_cf_en=\"BUS_{name}\"  co_st_op=\"BUS_{busname}_IP\"/>"
        self.XML_OperatorEnd = "\t\t</operator>"
        self.XML_RAM="\t<ram val_name=\"{name}\" val_bitsize=\"{bit_size}\" val_bitwidth=\"{bit_width}\" co_st_address=\"Address_{name}\" co_st_Data=\"{bus_name}\" co_st_Clock=\"Clk_{name}\" co_st_Output=\"Output_{name}\">"
        self.XML_ALUEnd="\t</alu>"
        self.XML_Buffer= "\t<buffer co_st_source=\"{source}\" co_st_destination=\"{destination}\" co_cf_ena=\"{enable}\"/>"


        ##Controller 
        self.XML_Comp_Reg_1 = "\t\t<reg val_name=\"CP1_REG_{busname}\" val_bitsize=\"{bit_size}\" co_st_ip=\"BUS_{busname}_OP\" co_cf_en=\"CP1_REG_{busname}\" co_st_clk=\"Globalclock\" co_cf_clr=\"\" co_st_op=\"Out_CP1_{busname}\"/>"
        self.XML_Comp_Reg_2 = "\t\t<reg val_name=\"CP2_REG_{busname}\" val_bitsize=\"{bit_size}\" co_st_ip=\"BUS_{busname}_OP\" co_cf_en=\"CP2_REG_{busname}\" co_st_clk=\"Globalclock\" co_cf_clr=\"\" co_st_op=\"Out_CP2_{busname}\"/>"
        self.XML_Comp_COMP = "\t\t<comp val_name=\"CP_{busname}\" val_bitsize=\"{bit_size}\" co_st_ip1=\"Out_CP1_{busname}\" co_st_ip2=\"Out_CP2_{busname}\" co_st_out1=\"Out1_{busname}\" co_st_out2=\"Out2_{busname}\" co_st_out3=\"Out3_{busname}\" />"
        self.XML_Comp_Mux = "\t\t<mux val_name=\"CP_MUX_{busname}\" val_bitsize=\"{bit_size}\" co_st_input0=\"\" co_st_input1=\"Out1_{busname}\"  co_st_input2=\"Out2_{busname}\" co_st_input3=\"Out3_{busname}\"  val_bitwidth=\"2\" co_cf_selector=\"CP_{busname}_SEL\" co_st_Output=\"CP_{busname}_OP\"/>" 
        self.XML_Comp_SP = "\t\t<splitter val_name=\"CP_SPLIT_{busname}\" val_bitsize=\"{bit_size}\" co_cf_Input_1=\"CP_BIT1_{busname}\" co_cf_Input_2=\"CP_BIT2_{busname}\" co_st_out=\"CP_{busname}_SEL\">" 
        self.XML_Comp_NOT = "\t\t<not co_st_ip=\"CP_{busname}_OP\" co_st_out=\"CP_{busname}_OP__1\">"
        self.XML_Comp_AND = "\t\t<and co_st_ip1=\"CP_{busname}_OP\" co_st_ip2=\"EN_JP_{busname}\" co_st_out=\"{busname}_c1\">"
        self.XML_Comp_OR = "\t\t<or co_st_ip1=\"{busname}_c1\" co_st_ip2=\"DR_JP_{busname}\" co_st_out=\"CP_{busname}_OP__1\">"

                


        # Reserved words
        self.reserved = {
            'bus': 'BUS',
            'reg': 'REGISTER',
            'ram': 'RAM',
            'tty': 'TTY',
            'alu': 'ALU',
            'if': 'IF',
            'while': 'WHILE',
            'endif':'ENDIF',
            'endwhile':'ENDWHILE',
            'delay':'DELAY'

        }

        self.tokens = ['NAME', 'SEM', 'COL', 'NUMBER','QUOTES','LSHIFT','DOT','EQUALS','PLUS','MINUS','MUL','DIV','COMMA','LPAREN','RPAREN','GT', 'LT', 'EQ'] + list(self.reserved.values())
    t_DOT =  r'\.'
    t_QUOTES = r'"'
    t_LSHIFT = r'<<'
    t_EQUALS = r'='
    t_PLUS = r'\+'
    t_MINUS = r'-'
    t_MUL = r'\*'
    t_DIV = r'\/'
    t_COMMA = r','
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_GT = r'>'
    t_LT = r'<'
    t_EQ = r'=='


    def build(self, **kwargs):
        self.lexer = lex.lex(module=self, **kwargs)
        self.parser = yacc.yacc(module=self, **kwargs)

    def setup_logger(self,code_path):
        # Create a logger
        modified_code_path = re.sub(r'\..*', '', code_path)
        self.logger = logging.getLogger()
        self.logger.setLevel(logging.DEBUG)  # Set the logging level

        # Create a file handler
        self.file_handler = logging.FileHandler(modified_code_path+'.log')
        self.file_handler.setLevel(logging.DEBUG)  # Set the level for file output

        # Create a console handler
        self.console_handler = logging.StreamHandler()
        self.console_handler.setLevel(logging.INFO)  # Set the level for console output

        # Create a formatter and set it for both handlers
        formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
        self.file_handler.setFormatter(formatter)
        self.console_handler.setFormatter(formatter)

        # Add handlers to the logger
        self.logger.addHandler(self.file_handler)
        self.logger.addHandler(self.console_handler)

    t_ignore = ' '
    def t_COL(self, t):
        r'\:'
        return t

    def t_SEM(self, t):
        r'\;'
        return t

    def t_NUMBER(self, t):
        r'\d+'
        t.value = int(t.value)
        return t

    def t_NAME(self, t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        t.type = self.reserved.get(t.value, 'NAME')
        return t

    def t_error(self, t):
        print("Illegal chars")
        t.lexer.skip(1)
    # Function to find the index of a particular ALUName
    def find_alu_detail_index(self,alu_list: List['MyParser.ALUDetail'], alu_name: str) -> int:
        for index, alu_detail in enumerate(alu_list):
            if alu_detail.ALUName == alu_name:
                return index
        return -1  # Return -1 if not found
    def p_main(self, p):
        '''
        main : statement
             | empty    
        '''
        self.run(p[1])

    def p_statements(self, p):
        '''
        statements : statements statement
                   | statement
        '''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[2]]

    def p_statement_if(self, p):
        '''
        statement : IF LPAREN condition RPAREN
        '''
        condition = p[3]
        self.Assembly.append("CJ,"+str(condition[0])+","+condition[1]+","+str(condition[2])+",1")
    
    def p_statement_while(self, p):
        '''
        statement : WHILE LPAREN condition RPAREN
        '''
        condition = p[3]
        self.Assembly.append("CJ,"+str(condition[0])+","+condition[1]+","+str(condition[2])+",2")

    def p_statement_delay(self, p):
        '''
        statement : DELAY LPAREN NUMBER RPAREN SEM
        '''        
        self.Assembly.append("Delay,"+str(p[3]))

    def p_condition(self, p):
        '''
        condition : simple_expression GT simple_expression
                  | simple_expression LT simple_expression
                  | simple_expression EQ simple_expression
        '''
        p[0] = [p[1], p[2], p[3]]

    def p_simple_expression(self, p):
        '''
        simple_expression : NAME
                          | NUMBER
        '''
        p[0] = p[1]


    def p_statement_endwhile(self, p):
        '''
        statement : ENDWHILE
        '''
        self.Assembly.append("endwhile")

    def p_statement_endif(self, p):
        '''
        statement : ENDIF
        '''
        self.Assembly.append("endif")
    def add_ALUOperator(self, idx: int, op_name: str, op_list: List[str]):
        if idx < len(self.ALUInfo):
            loc_ALUOp = self.ALUOperator(OpName=op_name, OpList=op_list)
            self.ALUInfo[idx].ALUOperatorList.append(loc_ALUOp)
        else:
            raise IndexError("Index out of range for ALUInfo")

    # Parsing rules
    def p_statement_ALUEQ(self, p):
        '''statement : NAME EQUALS ALU DOT NAME DOT NAME LPAREN parameters RPAREN SEM'''
        var_name = p[1]
        operation = f"{p[5]}.{p[7]}"
        parameters = p[9]
        ALUList="ALU:"+p[1]+":"+p[5]+":"+p[7]+","
        for i in p[9]:
            ALUList=ALUList+str(i)+','        
        self.Assembly.append(ALUList[0:-1])

    def p_parameters_ALUEQ(self, p):
        '''parameters : parameters COMMA parameter
                      | parameter'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]
    def p_parameters_ALUEQ(self, p):
        '''parameters : parameters COMMA parameter
                      | parameter'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    def p_parameter(self, p):
        '''parameter : NAME
                     | NUMBER'''
        p[0] = p[1]

    def p_statement_ALU(self,p):
        '''
        statement : ALU NAME COL NAME SEM
        '''
        if p[4] in self.loc_BUS_Name:
            index = self.loc_BUS_Name.index(p[4])         
            ALUBit_Value=int(self.loc_BUS_BitSize[index])
            if  p[2] in self.NameStack:
                self.logger.critical('Variable Name " '+p[2]+' " already Exists')
            else:
                self.ALUInfo.append(self.ALUDetail(ALUBit=ALUBit_Value, ALUName=p[2],ALUBUS=p[4]))
                self.NameStack.append(p[2])
    def p_statement_ALUExp(self,p):
        '''
        statement : ALU DOT NAME DOT NAME EQUALS expression SEM
        '''
        if  p[5] in self.NameStack:
            self.loc_ALUNameClash=False
            self.logger.critical('Variable Name " '+p[5]+' " already Exists')
        else:
            self.loc_ALUNameClash=True;
            self.NameStack.append(p[5])
            self.loc_ALUOpName = p[5]
            self.loc_ALUIdx=self.find_alu_detail_index(self.ALUInfo, p[3])
            self.ALUInfo_Update(p[7])
    def p_statement_RegExp(self,p):
        '''
        statement : NAME EQUALS NUMBER  SEM
        '''
        self.Assembly.append("LOAD,"+p[1]+","+str(p[3]))        
    def p_statement_Reg1Exp(self,p):
        '''
        statement : NAME EQUALS NAME  SEM
        '''
        self.Assembly.append("MOV,"+p[1]+","+p[3])
    def p_expression(self,p):
        '''
        expression : expression PLUS expression
                   | expression MINUS expression
                   | expression MUL expression
                   | expression DIV expression
                   | term
        '''        
        if len(p) == 4 and self.loc_ALUNameClash:
            if p[2] == '+':
                self.loc_ALUStack.append('+') 
            elif p[2] == '-':
                self.loc_ALUStack.append('-')
            elif p[2] == '*':
                self.loc_ALUStack.append('*')
            elif p[2] == '/':
                self.loc_ALUStack.append('/')                
    def ALUInfo_Update(self, p):
        if self.loc_ALUIdx!=-1:
            self.add_ALUOperator(idx=self.loc_ALUIdx, op_name=self.loc_ALUOpName, op_list=self.loc_ALUStack)
        self.loc_ALUStack=[]

    def p_term(self,p):
        '''
        term : NAME
             | NUMBER
        '''
        p[0] = p[1]
    def p_statement_Display(self, p):
        '''
        statement : TTY LSHIFT NAME  LSHIFT  NAME  SEM
        '''        
        p[0] = ("ttystr",p[3], p[5])            
        self.Assembly.append("ttyStr,"+p[5])  
    def p_statement_Display_ASCII(self, p):
        '''
        statement : TTY LSHIFT NAME  LSHIFT QUOTES NAME QUOTES SEM
        '''        
        p[0] = ("ttystr",p[3], p[6])            
        print(p[6])
        
        if p[6]=="newline":
            self.Assembly.append("ttyStrASCII,13")
        else:
            for char in p[6]:
                print(str(ord(char)))
                self.Assembly.append("ttyStrASCII,"+str(ord(char)))
 

    def p_statement(self, p):
        '''
        statement : REGISTER COL NAME NAME SEM  
                  | RAM COL NAME NAME SEM                  
        '''
        #
        p[0] = (p[1], p[3], p[4])
    def p_empty(self, p):
        '''
        empty :
        '''
        p[0] = None
    def find_operator_index(self,ALU_List_nam, operator_name):
        # Iterate through the list of ALUOperator objects in ALU_List_nam
        for index, operator in enumerate(ALU_List_nam.ALUOperatorList):
            # Check if the OpName matches the operator_name
            if operator.OpName == operator_name:
                return index
        # If no match is found, return -1 or another appropriate value to indicate not found
        return -1
    def p_statement_BUS(self,p):
        'statement : BUS  NAME NUMBER SEM'
        #p[0] = ('statement', p[3], p[5])
        # Trigger the function with the parsed name and list of names
        #self.trigger_function(p[3], p[5])
        p[0] = (p[1],p[2],p[3])

    def save_ascii_list_to_file(self,ascii_list, filename):
        with open(filename, 'w') as file:
            for ascii_value in ascii_list:
                file.write(str(ascii_value) + ' ')
    def ALUCompile(self):
        loc_RegNo=0
        for alu_detail in self.ALUInfo:
            self.XMLStack=self.XMLStack+'\n'+self.XML_ALUStart.format(name=alu_detail.ALUName, bit_size=alu_detail.ALUBit)
            loc_RegNo=0
            for alu_operator in alu_detail.ALUOperatorList:
                self.XMLStack=self.XMLStack+'\n'+self.XML_OperatorStart.format(name=alu_operator.OpName)
                idx=0
                for Op in reversed(alu_operator.OpList):
                    if Op =='+':
                        self.XMLStack=self.XMLStack+'\n'+self.XML_Adder.format(name=alu_operator.OpName+"_"+str(idx),bit_size=alu_detail.ALUBit)
                    elif Op =='-':
                        self.XMLStack=self.XMLStack+'\n'+self.XML_Sub.format(name=alu_operator.OpName+"_"+str(idx),bit_size=alu_detail.ALUBit)
                    elif Op =='*':
                        self.XMLStack=self.XMLStack+'\n'+self.XML_Mul.format(name=alu_operator.OpName+"_"+str(idx),bit_size=alu_detail.ALUBit)
                    elif Op =='/':
                        self.XMLStack=self.XMLStack+'\n'+self.XML_Div.format(name=alu_operator.OpName+"_"+str(idx),bit_size=alu_detail.ALUBit)
                    if idx>0:
                        self.loc_ALUWire.append("\t"+self.XML_Wire.format(source=alu_operator.OpName+"_"+str(idx-1)+"_Output",destination=alu_operator.OpName+"_"+str(idx)+"_Input1"))
                        self.loc_ALUWire.append("\t"+self.XML_Wire.format(source="Out_Reg_"+alu_detail.ALUName+"_"+str(idx+1),destination=alu_operator.OpName+"_"+str(idx)+"_Input2"))
                    else:
                        self.loc_ALUWire.append("\t"+self.XML_Wire.format(source="Out_Reg_"+alu_detail.ALUName+"_"+str(idx),destination=alu_operator.OpName+"_"+str(idx)+"_Input1"))
                        self.loc_ALUWire.append("\t"+self.XML_Wire.format(source="Out_Reg_"+alu_detail.ALUName+"_"+str(idx+1),destination=alu_operator.OpName+"_"+str(idx)+"_Input2"))

                    idx=idx+1
                    if idx>=len(alu_operator.OpList):
                        self.loc_ALUWire.append("\t"+self.XML_Wire.format(source=alu_operator.OpName+"_"+str(idx-1)+"_Output",destination="Mux_"+alu_operator.OpName))
                    if loc_RegNo<idx:
                        loc_RegNo=idx                    
                self.XMLStack=self.XMLStack+'\n'+self.XML_OperatorEnd
            self.XMLStack=self.XMLStack+'\n'+self.XML_Mux.format(name='Mux_'+alu_detail.ALUName, bit_size=alu_detail.ALUBit)
            j=0
            for k in alu_detail.ALUOperatorList:
                self.XMLStack=self.XMLStack+' co_st_input'+str(j)+'=\"Mux_'+k.OpName+'\" '
                j=j+1
            Mux_bits = j.bit_length()
            self.XMLStack=self.XMLStack+' val_bitwidth=\"'+str(Mux_bits)+'\" co_cf_selector=\"'+'Mux_'+alu_detail.ALUName+'_selector\" co_st_Output=\"'+'ALU_Output_'+alu_detail.ALUName+'\" >'
            self.XMLStack=self.XMLStack+'\n'+"\t\t<splitter val_name=\"Mux_"+alu_detail.ALUName+'_selector_splitter\" '
            for i in range(0,Mux_bits):
                self.XMLStack=self.XMLStack+" co_cf_Input_"+str(i)+"=\"Mux_"+alu_detail.ALUName+'_selector_Bit_'+str(i)+'\" '
                self.ControlList.append("Mux_"+alu_detail.ALUName+'_selector_Bit_'+str(i))

            self.XMLStack=self.XMLStack+" co_st_out=\"Mux_"+alu_detail.ALUName+"_selector\" />"
            for i in range(0,loc_RegNo+1):
                self.XMLStack=self.XMLStack+'\n'+"\t" +self.register_xml_template.format(name='Reg_'+alu_detail.ALUName+'_'+str(i), bit_size=alu_detail.ALUBit)
                self.ControlList.append("Ena_Reg_"+alu_detail.ALUName+'_'+str(i))

            for i in self.loc_ALUWire:
                self.XMLStack=self.XMLStack+'\n'+i
            self.loc_ALUWire=[]
            self.XMLStack=self.XMLStack+'\n'+self.XML_ALUEnd
            for i in range(0,loc_RegNo+1):
                self.XMLStack=self.XMLStack+'\n'+self.XML_Wire.format(source='Input_Reg_'+alu_detail.ALUName+'_'+str(i),destination="BUS_"+alu_detail.ALUBUS+"_OP")
            self.XMLStack=self.XMLStack+'\n'+self.XML_Buffer.format(source='ALU_Output_'+alu_detail.ALUName,destination='BUS_'+alu_detail.ALUBUS+'_IP',enable="Ena_BUS_"+alu_detail.ALUName)
            self.ControlList.append("Ena_BUS_"+alu_detail.ALUName)
    def Register(self, Register_BUS, Register_Name):
        if Register_BUS in self.loc_BUS_Name:
            index = self.loc_BUS_Name.index(Register_BUS)            
            Register_NumberOfBits=int(self.loc_BUS_BitSize[index])
            if Register_Name in self.NameStack:
                self.logger.critical('Variable Name '+Register_Name+' already Exists')
            elif Register_NumberOfBits < self.REGISTER_BIT_LOW or Register_NumberOfBits > self.REGISTER_BIT_HIGH:
                self.logger.critical('Number of Bit should be between '+str(self.REGISTER_BIT_LOW)+' and '+str(self.REGISTER_BIT_HIGH))
            else:
                self.NameStack.append(Register_Name)
                #Register_XML = "\t<reg name=\""+Register_Name+"\" BitSize=\""+str(Register_NumberOfBits)+"\" enable=\"Ena_"+Register_Name+"\" clock=\"Globalclock\" clear=\"Clr_"+Register_Name+"\" output=\"Out_"+Register_Name+"\"/>"
                self.VariableName.append(Register_Name)
                self.VariableType.append("Register:"+Register_BUS)
                self.logger.info("Register Created with below specification")
                self.XMLStack=self.XMLStack+'\n'+self.XML_Reg.format(name=Register_Name, bit_size=Register_NumberOfBits,busname=Register_BUS)
                self.XMLStack=self.XMLStack+'\n'+self.XML_Reg_Buf.format(name=Register_Name, bit_size=Register_NumberOfBits,busname=Register_BUS)
                self.BUS_Name.append(Register_BUS);
                self.ControlList.append("Ena_"+Register_Name)
                self.ControlList.append("Clr_"+Register_Name)
                self.ControlList.append("BUS_"+Register_Name)

    def write_hex_output(self,num_list, filename):
        """
        This function takes a list of numbers as strings, converts each number to hexadecimal,
        groups them into lines of up to 8 elements, and writes the result to a file.

        :param num_list: List of strings containing numbers
        :param filename: String, the name of the file to write the output to
        """
        # Convert each number to its hexadecimal representation (without '0x' prefix)
        hex_list = [hex(int(num)).replace('0x', '') for num in num_list]

        # Group the list into lines of up to 8 elements
        lines = [hex_list[i:i+8] for i in range(0, len(hex_list), 8)]

        # Open the file in write mode
        with open(filename, 'w') as file:
            # Write the version string at the beginning
            file.write('v2.0 raw\n')
            
            # Write each line of formatted output
            for line in lines:
                file.write(' '.join(line) + '\n')
    def write_formatted_output(self,hex_list, filename):
        """
        This function takes a list of hex values, formats them by removing the '0x' prefix,
        groups them into lines of up to 8 elements, and writes the result to a file.

        :param hex_list: List of strings containing hex values
        :param filename: String, the name of the file to write the output to
        """
        # Remove '0x' prefix
        formatted_list = [x.replace('0x', '') for x in hex_list]

        # Group the list into lines of up to 8 elements
        lines = [formatted_list[i:i+8] for i in range(0, len(formatted_list), 8)]

        # Open the file in write mode
        with open(filename, 'w') as file:
            # Write the version string at the beginning
            file.write('v2.0 raw\n')
            
            # Write each line of formatted output
            for line in lines:
                file.write(' '.join(line) + '\n')

    def write_hex_output_from_integers(self,int_list, filename):
        """
        This function takes a list of integers, converts each number to hexadecimal,
        groups them into lines of up to 8 elements, and writes the result to a file.

        :param int_list: List of integers
        :param filename: String, the name of the file to write the output to
        """
        # Convert each integer to its hexadecimal representation (without '0x' prefix)
        hex_list = [hex(num).replace('0x', '') for num in int_list]

        # Group the list into lines of up to 8 elements
        lines = [hex_list[i:i+8] for i in range(0, len(hex_list), 8)]

        # Open the file in write mode
        with open(filename, 'w') as file:
            # Write the version string at the beginning
            file.write('v2.0 raw\n')
            
            # Write each line of formatted output
            for line in lines:
                file.write(' '.join(line) + '\n')
    
    def collect_unique_values(self,list_one, list_two):
        # Initialize a dictionary to store the results
        result = {}

        # Iterate through both lists
        for i in range(len(list_one)):
            key = list_one[i]
            value = list_two[i]
            if key not in result:
                result[key] = []
            if value not in result[key]:  # Avoid duplicates
                result[key].append(value)
        return result

    def pop_and_sum_tty(self, n):
        # Pop the last n items       
        popped_items = [int(self.TTYHEXControlOPCode.pop(), 16) for _ in range(n)]#TTYHEXControlOPCode
        
        # Calculate the sum
        total_sum = sum(popped_items)
        popped_items___ = [int(self.HEXControlOPCode.pop(), 16) for _ in range(n)]
        # Convert the sum back to hex and append it to the list
        self.TTYHEXControlOPCode.append(hex(total_sum))
        self.HEXControlOPCode.append("0")

    def pop_and_sum(self, n):
        # Pop the last n items       
        popped_items = [int(self.HEXControlOPCode.pop(), 16) for _ in range(n)]
        
        # Calculate the sum
        total_sum = sum(popped_items)
        popped_items___ = [int(self.TTYHEXControlOPCode.pop(), 16) for _ in range(n)]
        # Convert the sum back to hex and append it to the list
        self.HEXControlOPCode.append(hex(total_sum))
        self.TTYHEXControlOPCode.append("0")
    def RAM(self, *args):
        if not args:  # If no arguments are passed, iterate through RAM_Stack
            RAM_Value=self.collect_unique_values(self.RAM_BUS_Stack,self.RAM_Stack)
            for key, values in RAM_Value.items():
                RAM_Count=len(values)
                RAM_AddressSize=RAM_Count.bit_length()
                index = self.loc_BUS_Name.index(key)            
                NumberOfBits=int(self.loc_BUS_BitSize[index])            
                self.XMLStack=self.XMLStack+'\n'+self.ram_start_xml_template.format(name=key, bit_size=NumberOfBits,address_size=str(RAM_AddressSize),busname=key)                
                i=0
                for valu in values:
                    self.VariableName.append(valu)
                    self.VariableType.append("RAM:"+key+":"+str(i))                    
                    self.XMLStack=self.XMLStack+'\n'+self.ram_address_xml_template.format(name=valu, address_value=str(i))
                    i=i+1
                self.XMLStack=self.XMLStack+'\n'+self.ram_end_xml_template
                self.XMLStack=self.XMLStack+'\n'+self.XML_Ram_Buf.format(name="Out_"+key, bit_size=NumberOfBits,input="RAM_Out_"+key,output="BUS_"+key+"_CO",enable="RAM_BusOut_"+key)
                self.XMLStack=self.XMLStack+'\n'+self.XML_Ram_Buf.format(name="Mode_"+key, bit_size=NumberOfBits,input="Globalclock",output="RAM_Clk_"+key,enable="RAM_BusMode_"+key)
                self.ControlList.append("RAM_BusOut_"+key)
                self.ControlList.append("RAM_BusMode_"+key)
                self.XMLStack=self.XMLStack+'\n'+"\t<splitter val_name=\"Splitter_"+key+"\" "
                for x in range(0,RAM_AddressSize):
                    self.XMLStack=self.XMLStack+"co_cf_Input_"+str(x)+"=\"ADD_"+key+"_BIT_"+str(x)+"\" "
                    self.ControlList.append("ADD_"+key+"_BIT_"+str(x))
                self.XMLStack=self.XMLStack+" co_st_out=\"RAM_"+key+"_ADDRESS\" />"

        elif len(args) == 2:
            RAM_Bus, RAM_Name = args            
            if RAM_Bus in self.loc_BUS_Name:
                index = self.loc_BUS_Name.index(RAM_Bus)
                RAM_NumberOfBits=int(self.loc_BUS_BitSize[index])                            
                if RAM_Name in self.NameStack:
                    self.logger.critical('Variable Name '+RAM_Name+' already Exists')
                elif RAM_NumberOfBits < self.RAM_BIT_LOW or RAM_NumberOfBits > self.RAM_BIT_HIGH:
                    self.logger.critical('Number of Bit should be between '+str(self.RAM_BIT_LOW)+' and '+str(self.RAM_BIT_HIGH))
                else:
                    self.NameStack.append(RAM_Name)                    
                    self.RAM_Stack.append(RAM_Name)
                    self.RAM_BUS_Stack.append(RAM_Bus)
                    self.logger.info("RAM Memory Created with below specification")
                    self.logger.info("RAM Name is RAM_"+str(RAM_NumberOfBits)+" with Address Name of "+str(RAM_Name))
            else:
                self.logger.error("Invalid number of arguments for RAM function")                        
    def ControlList_XML(self):
        k=0
        self.XMLStack=self.XMLStack+"\n\t<ControlUnit>\n"
        for bus  in self.loc_BUS_Name:
            self.XMLStack=self.XMLStack+self.XML_Comp_Reg_1.format(busname=bus,bit_size=self.loc_BUS_BitSize[k])+"\n"
            self.XMLStack=self.XMLStack+self.XML_Comp_Reg_2.format(busname=bus,bit_size=self.loc_BUS_BitSize[k])+"\n"
            self.XMLStack=self.XMLStack+self.XML_Comp_COMP.format(busname=bus,bit_size=self.loc_BUS_BitSize[k])+"\n"
            self.XMLStack=self.XMLStack+self.XML_Comp_Mux.format(busname=bus,bit_size=self.loc_BUS_BitSize[k])+"\n"
            self.XMLStack=self.XMLStack+self.XML_Comp_SP.format(busname=bus,bit_size=self.loc_BUS_BitSize[k])+"\n"
            self.XMLStack=self.XMLStack+self.XML_Comp_NOT.format(busname=bus)+"\n"
            self.XMLStack=self.XMLStack+self.XML_Comp_AND.format(busname=bus)+"\n"
            self.XMLStack=self.XMLStack+self.XML_Comp_OR.format(busname=bus)+"\n"
        self.XMLStack=self.XMLStack+"\t\t<splitter val_name=\"ControlROM_Output\" "
        k=0
        for control in self.ControlList:
            self.XMLStack=self.XMLStack+" co_st_Input_"+str(k)+"=\""+control+"\" "
            k=k+1
        self.XMLStack=self.XMLStack+" co_st_out=\"CountrolOutput\">\n"
        self.XMLStack=self.XMLStack+"\t\t<rom co_st_Input=\"Control\" co_st_out=\"CountrolOutput\">\n"
        self.XMLStack=self.XMLStack+"\t</ControlUnit>\n"

    def TTYFun(self, TTYDisplay, TTYValue, TTYType):
        if TTYType == 1:
            if TTYDisplay in self.TTYStack:
                idx = self.TTYStack.index(TTYDisplay)
                self.TTYROM[idx]=self.TTYROM[idx]+TTYValue
                self.TTYLen[idx].append(len(TTYValue))
                self.logger.info("Static Display "+TTYValue+ " Added to ROM")                
            else:
                self.TTYStack.append(TTYDisplay)
                self.TTYROM.append(TTYValue)
                self.TTYLen.append([len(TTYValue)])
                self.logger.info("New TTY Display \""+TTYDisplay+ " \"Created")
                self.logger.info("Static Display "+TTYValue+ " Added to ROM")

    def string_to_ascii_list(self,string):
        return [format(ord(char), 'x') for char in string]
    def TTY_end(self):
        for index, tty in enumerate(self.TTYStack): 
            TTY_XMLOutput = self.tty_display_xml_template.format(name=tty)
            ascii_list = self.string_to_ascii_list(self.TTYROM[index])
            self.save_ascii_list_to_file(ascii_list, tty)
            self.XMLStack=self.XMLStack+'\n'+TTY_XMLOutput
                    
    def BUS_assignment(self,bus_name,bus_bit):
        self.XMLStack=self.XMLStack+'\n'+self.XML_Bus_Start.format(name=bus_name)
        self.XMLStack=self.XMLStack+'\n'+self.XML_Bus_Reg.format(name=bus_name,bit_size=bus_bit)
        self.XMLStack=self.XMLStack+'\n'+self.XML_Bus_Mux.format(name=bus_name,bit_size=bus_bit)
        self.XMLStack=self.XMLStack+'\n'+self.XML_Bus_ROM.format(name=bus_name,bit_size=bus_bit)
        self.XMLStack=self.XMLStack+'\n'+self.XML_Bus_ROM_Split.format(name=bus_name)
        self.XMLStack=self.XMLStack+'\n'+self.XML_Bus_End
        self.loc_BUS_Name.append(bus_name)
        self.loc_BUS_BitSize.append(bus_bit)
        self.ControlList.append("Ena_"+bus_name)
        self.ControlList.append("Clr_"+bus_name)
        self.ControlList.append("BUS_"+bus_name+"_SEL")
        self.ControlList.append("SPLIT_1_"+bus_name)
        self.ControlList.append("SPLIT_2_"+bus_name)
        self.ControlList.append("SPLIT_3_"+bus_name)
        self.ControlList.append("SPLIT_4_"+bus_name)
        #Control Jump
        self.ControlList.append("CP_BIT1_"+bus_name)
        self.ControlList.append("CP_BIT2_"+bus_name)
        self.ControlList.append("CP1_REG_"+bus_name)
        self.ControlList.append("CP2_REG_"+bus_name)
        self.ControlList.append("JP_"+bus_name+"_BIT0")
        self.ControlList.append("JP_"+bus_name+"_BIT1")
        self.ControlList.append("JP_"+bus_name+"_BIT2")
        self.ControlList.append("JP_"+bus_name+"_BIT3")
        self.ControlList.append("EN_JP_"+bus_name)
        self.ControlList.append("DR_JP_"+bus_name)

    def run(self, p):
        if type(p) == tuple:
            if p[0] == 'reg':
                self.Register(p[1], p[2])
            elif p[0] == 'ram':
                if len(p) == 3:
                    self.RAM(p[1], p[2])
                elif len(p) == 4:
                    self.RAM(p[1], p[2], p[3])
            elif p[0] == 'ttystr':
                self.TTYFun(p[1],p[2],1)
            elif p[0] == 'bus':
                self.BUS_assignment(p[1],p[2])

    def find_index(self,my_list, search_string):
        for i, item in enumerate(my_list):
            if item == search_string:
                return i
        return -1

    def parse_file(self, code_path):
        self.logger.info("Start Reading the code file \""+code_path+"\"")
        with open(code_path, 'r') as file:
            for line in file:
                ParsingString = line.strip()
                self.parser.parse(ParsingString)
        self.logger.info("End Reading the code file \""+code_path+"\"")
    def OutputXML(self,XML_path):        
        self.XMLStack=self.XMLStack+'\n </root>'
        with open(XML_path, 'w') as file:
            file.write(self.XMLStack)
        self.logger.info("Genrating the XML Ouput")
    def decimal_to_binary_string_array(self, n: int) -> List[str]:        # Convert the decimal number to binary (bin function returns a string prefixed with '0b')
        binary_representation = bin(n)[2:]  # [2:] to remove the '0b' prefix
        
        # Convert the binary string into a list of strings, each representing one bit
        binary_string_array = list(binary_representation)
        
        return binary_string_array
    def decimal_to_binary_4bits(self,n):
        if n < 0 or n > 15:
            raise ValueError("Number out of range. Please enter a number between 0 and 15.")
        binary = ""
        while n > 0:
            binary = str(n % 2) + binary
            n = n // 2
        # Pad with leading zeros to ensure the result is 4 bits long
        while len(binary) < 4:
            binary = "0" + binary
        return binary

    def binary_list_to_hex_tty(self,binary_list_Value):
        #a = ['sdad', 's', 'ds', 'ffds']
        binary_list = [0] * len(self.TTYControlList)
        #self.ControlList.reverse()
        for i in binary_list_Value:
            Value=i.split('=')
            index = self.find_index(self.TTYControlList,Value[0])            
            if index>=0:
                binary_list[index]=int(Value[1])         
        # Join the binary list into a single binary string
        binary_list.reverse()
        binary_string = ''.join(str(bit) for bit in binary_list)
        # Convert the binary string to an integer
        binary_number = int(binary_string, 2)
        # Convert the integer to a hexadecimal string
        hex_value = hex(binary_number)
        
        return hex_value
    
    def binary_list_to_hex(self,binary_list_Value):
        binary_list = [0] * len(self.ControlList)
        for i in binary_list_Value:
            Value=i.split('=')
            index = self.find_index(self.ControlList,Value[0])        
            if index>=0:
                binary_list[index]=int(Value[1])         
        # Join the binary list into a single binary string
        binary_list.reverse()
        binary_string = ''.join(str(bit) for bit in binary_list)
        # Convert the binary string to an integer
        binary_number = int(binary_string, 2)
        # Convert the integer to a hexadecimal string
        hex_value = hex(binary_number)
        
        return hex_value
    
    def MachineLangage(self):
        
        for Command in self.Assembly:
            EachToken=Command.split(',')
            if EachToken[0]=='LOAD':
                index = self.find_index(self.VariableName, EachToken[1])
                if index>=0:
                    EachToken_Variable=self.VariableType[index].split(':')
                    if EachToken_Variable[0]=='Register':
                        #SetOpCode=[]
                        BusValue=EachToken_Variable[1]
                        self.CodeSeq.append("N,"+str(len(self.StaticROM))+","+BusValue)
                        self.CodeExplain.append("#"+str(len(self.StaticROM))+"-->BUS_"+BusValue)
                        self.CodeSeq.append("B,"+BusValue+","+EachToken[1])
                        self.CodeExplain.append("BUS_"+BusValue+"-->"+EachToken[1])
                        self.StaticROM.append(EachToken[2])
            elif EachToken[0]=='MOV':
                index = self.find_index(self.VariableName, EachToken[2])                
                if index>=0:
                    EachToken_Variable=self.VariableType[index].split(':')
                    if EachToken_Variable[0]=='Register':
                        BusValue=EachToken_Variable[1]
                        self.CodeSeq.append("V,"+EachToken[2]+","+BusValue)
                        self.CodeExplain.append(EachToken[2]+"-->"+"BUS_"+BusValue)
                index = self.find_index(self.VariableName, EachToken[1])                
                if index>=0:
                    EachToken_Variable=self.VariableType[index].split(':')
                    if EachToken_Variable[0]=='Register':
                        BusValue=EachToken_Variable[1]
                        self.CodeSeq.append("B,"+BusValue+","+EachToken[1])
                        self.CodeExplain.append("BUS_"+BusValue+"-->"+EachToken[1])
            elif "ALU" in Command:
                ALU_Value=Command.split(":")                
                idx = self.find_alu_detail_index(self.ALUInfo,ALU_Value[2])                
                BusValue=self.ALUInfo[idx].ALUBUS
                #print(ALU_Value[3])
                opera=ALU_Value[3].split(",")
                index_Op = self.find_operator_index(self.ALUInfo[idx], opera[0])
                ALUPar=Command.split(",")
                ALUPar.pop(0)
                k=0                
                for i in ALUPar:
                    index = self.find_index(self.VariableName, i)
                    if index>=0:
                        EachToken_Variable=self.VariableType[index].split(':')
                        if EachToken_Variable[0]=='Register':                    
                            BusValue=EachToken_Variable[1]
                            self.CodeSeq.append("V,"+i+","+BusValue)
                            self.CodeExplain.append(i+"-->BUS_"+BusValue)
                            self.CodeSeq.append("S,Ena_Reg_"+ALU_Value[2]+"_"+str(k))
                            self.CodeExplain.append("BUS_"+BusValue+"-->"+"Ena_Reg_"+ALU_Value[2]+"_"+str(k))
                    elif i.isdigit():
                        self.CodeSeq.append("N,"+str(len(self.StaticROM))+","+BusValue)
                        self.CodeExplain.append("#"+str(len(self.StaticROM))+"-->BUS_"+BusValue)
                        self.CodeSeq.append("S,Ena_Reg_"+ALU_Value[2]+"_"+str(k))
                        self.CodeExplain.append("BUS_"+BusValue+"-->"+"Ena_Reg_"+ALU_Value[2]+"_"+str(k))
                        self.StaticROM.append(i)
                    k=k+1
                self.CodeSeq.append("S,Ena_BUS_"+ALU_Value[2])
                self.CodeExplain.append("Ena_BUS_"+ALU_Value[2]+"-->"+"BUS_"+BusValue)

                self.CodeSeq.append("S,BUS_"+BusValue+"_SEL")
                self.CodeExplain.append("BUS_"+BusValue+"_SEL"+"-->1")

                self.CodeSeq.append("A,Mux_"+ALU_Value[2]+"_selector,"+str(index_Op))
                self.CodeExplain.append("Mux_"+ALU_Value[2]+"_selector-->"+str(index_Op))

                self.CodeSeq.append("S,Ena_"+BusValue)
                self.CodeExplain.append("Ena_"+BusValue+"-->1")

                self.CodeSeq.append("M,4")
                self.CodeExplain.append("Merge Last 4")

                self.CodeSeq.append("B,"+BusValue+","+ALU_Value[1])
                self.CodeExplain.append("BUS_"+BusValue+"-->"+ALU_Value[1])
            elif EachToken[0]=='CJ':
                if EachToken[4]=="1":
                    self.CodeSeq.append("JS")
                    self.CodeExplain.append("If_Start") 
                else:
                    self.CodeSeq.append("JM")
                    self.CodeExplain.append("While_Start") 
                index_1 = self.find_index(self.VariableName, EachToken[1])
                index_2 = self.find_index(self.VariableName, EachToken[3])
                if index_1>=0:
                    EachToken_Variable=self.VariableType[index_1].split(':')
                    if EachToken_Variable[0]=='Register':
                        BusValue=EachToken_Variable[1]
                        self.CodeSeq.append("V,"+EachToken[1]+","+BusValue)
                        self.CodeExplain.append("CJ:"+EachToken[1]+"-->BUS_"+BusValue)
                        self.CodeSeq.append("S,"+"CP1_REG_"+BusValue)
                        self.CodeExplain.append("CJ:BUS_"+BusValue+"-->"+"CP1_REG_"+BusValue)
                elif EachToken[1].isdigit():
                    self.CodeSeq.append("N,"+str(len(self.StaticROM))+","+BusValue)
                    self.CodeExplain.append("CJ:#"+str(len(self.StaticROM))+"-->BUS_"+BusValue)
                    self.CodeSeq.append("S,"+"CP1_REG_"+BusValue)
                    self.CodeExplain.append("CJ:BUS_"+BusValue+"-->"+"CP1_REG_"+BusValue)
                    self.StaticROM.append(EachToken[1])
                if index_2>=0:
                    EachToken_Variable=self.VariableType[index_2].split(':')
                    if EachToken_Variable[0]=='Register':
                        BusValue=EachToken_Variable[1]
                        self.CodeSeq.append("V,"+EachToken[3]+","+BusValue)
                        self.CodeExplain.append("CJ:"+EachToken[3]+"-->BUS_"+BusValue)
                        self.CodeSeq.append("S,"+"CP2_REG_"+BusValue)
                        self.CodeExplain.append("CJ:BUS_"+BusValue+"-->"+"CP2_REG_"+BusValue)
                elif EachToken[3].isdigit():
                    self.CodeSeq.append("N,"+str(len(self.StaticROM))+","+BusValue)
                    self.CodeExplain.append("CJ:#"+str(len(self.StaticROM))+"-->BUS_"+BusValue)
                    self.CodeSeq.append("S,"+"CP2_REG_"+BusValue)
                    self.CodeExplain.append("CJ:BUS_"+BusValue+"-->"+"CP2_REG_"+BusValue)
                    self.StaticROM.append(EachToken[3])
                if EachToken[2]=='>':
                    self.CodeSeq.append("S,"+"CP_BIT1_"+BusValue)
                    self.CodeExplain.append("CJ:>")                    
                elif EachToken[2]=='<':
                    self.CodeSeq.append("S,"+"CP_BIT2_"+BusValue)
                    self.CodeExplain.append("CJ:<")
                    self.CodeSeq.append("S,"+"CP_BIT1_"+BusValue)
                    self.CodeExplain.append("CJ:<")
                    self.CodeSeq.append("M,2")
                    self.CodeExplain.append("Merge Last 2")                    
                elif EachToken[2]=='==':
                    self.CodeSeq.append("S,"+"CP_BIT2_"+BusValue)
                    self.CodeExplain.append("CJ:==")                     
                self.CodeSeq.append("S,"+"EN_JP_"+BusValue)
                self.CodeExplain.append("EN_JP_"+BusValue+"=1")

                BinaryString=self.decimal_to_binary_4bits(self.JP_Cnt)
                k=0
                if BinaryString[0]=="1":
                    self.CodeSeq.append("S,JP_"+BusValue+"_BIT3")
                    self.CodeExplain.append("jump_3")
                    k=k+1
                elif BinaryString[1]=="1":
                    self.CodeSeq.append("S,JP_"+BusValue+"_BIT2")
                    self.CodeExplain.append("jump_2")
                    k=k+1
                elif BinaryString[2]=="1":
                    self.CodeSeq.append("S,JP_"+BusValue+"_BIT1")
                    self.CodeExplain.append("jump_1")
                    k=k+1
                elif BinaryString[3]=="1":
                    self.CodeSeq.append("S,JP_"+BusValue+"_BIT0")
                    self.CodeExplain.append("jump_0")
                    k=k+1               

                self.CodeSeq.append("M,"+str(2+k))
                self.CodeExplain.append("Merge Last "+str(2+k))

                self.JP_Cnt=self.JP_Cnt+1
            elif EachToken[0]=='endif':
                self.CodeSeq.append("JE")
                self.CodeExplain.append("ifend")
            elif EachToken[0]=='endwhile':
                self.CodeSeq.append("S,"+"DR_JP_"+BusValue)
                self.CodeExplain.append("DR_JP_"+BusValue+"=1")

                BinaryString=self.decimal_to_binary_4bits(self.JP_Cnt)
                k=0
                if BinaryString[0]=="1":
                    self.CodeSeq.append("S,JP_"+BusValue+"_BIT3")
                    self.CodeExplain.append("jump_3")
                    k=k+1
                if BinaryString[1]=="1":
                    self.CodeSeq.append("S,JP_"+BusValue+"_BIT2")
                    self.CodeExplain.append("jump_2")
                    k=k+1
                if BinaryString[2]=="1":
                    self.CodeSeq.append("S,JP_"+BusValue+"_BIT1")
                    self.CodeExplain.append("jump_1")
                    k=k+1
                if BinaryString[3]=="1":
                    self.CodeSeq.append("S,JP_"+BusValue+"_BIT0")
                    self.CodeExplain.append("jump_0")
                    k=k+1
                self.CodeSeq.append("M,"+str(1+k))
                self.CodeExplain.append("Merge Last "+str(2+k))
                self.JP_Cnt=self.JP_Cnt+1
                self.CodeSeq.append("JK")
                self.CodeExplain.append("whileend")
            elif EachToken[0]=='ttyStr':                              
                self.CodeSeq.append("V,"+EachToken[1]+",a")
                self.CodeSeq.append("TS,tty_L")
                self.CodeSeq.append("TS,tty_M1")
                self.CodeSeq.append("TS,tty_IP")              
                self.CodeSeq.append("TM,2")
                self.CodeSeq.append("TS,tty_M1")
                self.CodeSeq.append("TS,tty_IP")              
                self.CodeSeq.append("TM,2")
                self.CodeSeq.append("TS,tty_M1")
                self.CodeSeq.append("TS,tty_IP")              
                self.CodeSeq.append("TM,2")

                self.CodeSeq.append("N,"+str(len(self.StaticROM))+",a")
                self.CodeSeq.append("TS,tty_L")
                self.CodeSeq.append("TS,tty_M0")
                self.CodeSeq.append("TS,tty_IP")              
                self.CodeSeq.append("TM,2")
                self.CodeSeq.append("TS,tty_M0")
                self.CodeSeq.append("TS,tty_IP")              
                self.CodeSeq.append("TM,2")
                self.StaticROM.append("10")
            elif EachToken[0]=="ttyStrASCII":
                self.CodeSeq.append("N,"+str(len(self.StaticROM))+",a")
                self.CodeSeq.append("TS,tty_L")
                self.CodeSeq.append("TS,tty_M0")
                self.CodeSeq.append("TS,tty_IP")              
                self.CodeSeq.append("TM,2")
                self.StaticROM.append(EachToken[1])
            elif EachToken[0] == "Delay":
                 self.CodeSeq.append("D,"+EachToken[1])        
    def GetOpCode(self,filename):
        self.CodeSeq.append("S,CLK_1")
        self.CodeExplain.append("HALT")
        for i in self.CodeSeq:
            Command=i.split(",")
            if Command[0]=='N':
                SetOpCode=[]
                BusValue=Command[2]
                #print(BusValue)
                BinaryString=self.decimal_to_binary_4bits(int(Command[1]))
                SetOpCode.append("SPLIT_1_"+BusValue+"="+BinaryString[3])                        
                SetOpCode.append("SPLIT_2_"+BusValue+"="+BinaryString[2])
                SetOpCode.append("SPLIT_3_"+BusValue+"="+BinaryString[1])
                SetOpCode.append("SPLIT_4_"+BusValue+"="+BinaryString[0])
                SetOpCode.append("Ena_"+BusValue+"=1")
                HexValue=self.binary_list_to_hex(SetOpCode)
                self.HEXControlOPCode.append(HexValue)
                self.TTYHEXControlOPCode.append("0")                
            elif Command[0]=='V':
                SetOpCode=[]
                SetOpCode.append("BUS_"+Command[1]+"=1")
                SetOpCode.append("BUS_"+Command[2]+"_SEL=1")
                SetOpCode.append("Ena_"+Command[2]+"=1")
                HexValue=self.binary_list_to_hex(SetOpCode)
                self.HEXControlOPCode.append(HexValue) 
                self.TTYHEXControlOPCode.append("0")                
            elif Command[0]=='B':
                SetOpCode=[]
                SetOpCode.append("Ena_"+Command[2]+"=1")                
                HexValue=self.binary_list_to_hex(SetOpCode)
                self.HEXControlOPCode.append(HexValue) 
                self.TTYHEXControlOPCode.append("0")
            elif Command[0]=='S':
                SetOpCode=[]
                SetOpCode.append(Command[1]+"=1")
                HexValue=self.binary_list_to_hex(SetOpCode)
                self.HEXControlOPCode.append(HexValue) 
                self.TTYHEXControlOPCode.append("0")                
            elif Command[0]=='TS':
                SetOpCode=[]
                SetOpCode.append(Command[1]+"=1")
                HexValue=self.binary_list_to_hex_tty(SetOpCode)
                self.TTYHEXControlOPCode.append(HexValue)#HEXControlOPCode
                self.HEXControlOPCode.append("0")                
            elif Command[0]=='A':
                SetOpCode=[]
                binary_array = self.decimal_to_binary_string_array(int(Command[2]))
                k=0
                for j in binary_array:
                    SetOpCode.append(Command[1]+"_Bit_"+str(k)+"="+str(j))                    
                    k=k+1
                HexValue=self.binary_list_to_hex(SetOpCode)
                self.HEXControlOPCode.append(HexValue) 
                self.TTYHEXControlOPCode.append("0")
            elif Command[0]=='M':
                MergeNo=int(Command[1])
                self.pop_and_sum(MergeNo)#pop_and_sum_tty
            elif Command[0]=='TM':
                MergeNo=int(Command[1])
                self.pop_and_sum_tty(MergeNo)#pop_and_sum_tty
            elif Command[0]=="JS":
                self.ifStart.append(len(self.HEXControlOPCode))
            elif Command[0]=="JM":
                self.whileStart.append(len(self.HEXControlOPCode))
            elif Command[0]=='JE':
                top_value = self.ifStart.pop()
                self.ifend.append([top_value,len(self.HEXControlOPCode)])
                self.JP_ROM.append(len(self.HEXControlOPCode))
            elif Command[0]=='JK':          
                top_value = self.whileStart.pop()
                self.whileEnd.append([top_value,len(self.HEXControlOPCode)])
                self.JP_ROM.append(len(self.HEXControlOPCode))
                self.JP_ROM.append(top_value)
            elif Command[0]=='D':      
                for dt in range(0,int(Command[1])):     
                    self.HEXControlOPCode.append("0") 
                    self.TTYHEXControlOPCode.append("0")
        self.write_formatted_output(self.HEXControlOPCode,filename+'_Control_ROM')
        self.write_formatted_output(self.TTYHEXControlOPCode,filename+'_tty_ROM')
        self.write_hex_output(self.StaticROM,filename+'_BUS_ROM')        
        self.ifend = sorted(self.ifend, key=lambda x: x[0])
        self.whileEnd = sorted(self.whileEnd, key=lambda x: x[0])
        self.JP_ROM= sorted(self.JP_ROM,reverse=True)
        self.write_hex_output_from_integers(self.JP_ROM, filename+'_Jump_ROM')
code_path = 'SumOfN_No.txt'
File=code_path.split('.')
parser_instance = MyParser()
parser_instance.setup_logger(code_path)
parser_instance.build()

# Path to the code fileindex
parser_instance.parse_file(code_path)
if(parser_instance.RAM_OPTIMIZATION==1):
    parser_instance.RAM()
parser_instance.TTY_end()
parser_instance.ControlList_XML()
parser_instance.ALUCompile()
parser_instance.MachineLangage()
parser_instance.GetOpCode(File[0])
parser_instance.OutputXML(File[0]+'.xml')