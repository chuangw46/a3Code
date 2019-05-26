with Instruction;
use Instruction;
with Debug; use Debug;

-- used so we can print TAB character
with Ada.Characters.Latin_1;

package body Machine with SPARK_Mode is
   -- data values are 32-bit integers
   -- this is the type of words used in the virtual machine
   type DataVal is range -(2**31) .. +(2**31 - 1);
      
   -- the registers
   Regs : array (Reg) of DataVal := (others => 0);
   
   -- the memory
   Memory : array (Addr) of DataVal := (others => 0);
   
   -- the program counter
   PC : ProgramCounter := ProgramCounter'First;
      
   procedure IncPC(Ret : out ReturnCode; Offs : in Offset) is
   begin
      PC := ProgramCounter(Integer(PC) + Integer(Offs));
      Ret := Success;
   end IncPC;
   
   procedure DoAdd(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) + Regs(Rs2);
      Ret := Success;
   end DoAdd;
   
   procedure DoSub(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) - Regs(Rs2);
      Ret := Success;
   end DoSub;
   
   procedure DoMul(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) * Regs(Rs2);
      Ret := Success;
   end DoMul;
   
   procedure DoDiv(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) / Regs(Rs2);
      Ret := Success;
   end DoDiv;
   
   procedure DoLdr(Rd : in Reg; 
                   Rs : in Reg; 
                   Offs : in Offset;
                   Ret : out ReturnCode) is
      A : Addr := Addr(Regs(Rs) + DataVal(Offs));
   begin
      Regs(Rd) := Memory(A);
      Ret := Success;
   end DoLdr;
   
   procedure DoStr(Ra : in Reg;
                   Offs : in Offset;
                   Rb : in Reg;
                   Ret : out ReturnCode) is
      A : Addr := Addr(Regs(Ra) + DataVal(Offs));   
   begin
      Memory(A) := Regs(Rb);
      Ret := Success;
   end DoStr;
   
   procedure DoMov(Rd : in Reg;
                   Offs : in Offset;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := DataVal(Offs);
      Ret := Success;
   end DoMov;
   
   procedure ExecuteProgram(Prog : in Program;
                            Cycles : in Integer;
                            Ret : out ReturnCode;
                            Result : out Integer) 
   is
      CycleCount : Integer := 0;
      Inst : Instr;
   begin
      Ret := Success;
      PC := ProgramCounter'First;
      Result := 0;
      while (CycleCount < Cycles and Ret = Success) loop
         Inst := Prog(PC);
         
         -- debug print pc and current instruction
         Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         DebugPrintInstr(Inst);
         New_Line;
         
         case Inst.Op is
            when ADD =>
               DoAdd(Inst.AddRd,Inst.AddRs1,Inst.AddRs2,Ret);
               IncPC(Ret,1);
            when SUB =>
               DoSub(Inst.SubRd,Inst.SubRs1,Inst.SubRs2,Ret);
               IncPC(Ret,1);
            when MUL =>
               DoMul(Inst.MulRd,Inst.MulRs1,Inst.MulRs2,Ret);
               IncPC(Ret,1);
            when DIV =>
               DoDiv(Inst.DivRd,Inst.DivRs1,Inst.DivRs2,Ret);
               IncPC(Ret,1);
            when LDR =>
               DoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs,Ret);
               IncPC(Ret,1);
            when STR =>
               DoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb,Ret);
               IncPC(Ret,1);
            when MOV =>
               DoMov(Inst.MovRd,Inst.MovOffs,Ret);
               IncPC(Ret,1);
            when Instruction.RET =>
               Result := Integer(Regs(Inst.RetRs));
               Ret := Success;
               return;
            when JMP =>
               IncPC(Ret,Inst.JmpOffs);
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  IncPC(Ret,Inst.JzOffs);
               else
                  IncPc(Ret,1);
               end if;
            when NOP =>
               IncPC(Ret,1);
         end case;
         CycleCount := CycleCount + 1;
      end loop;
      if Ret = Success then
         -- Cycles instructions executed without a RET or invalid behaviour
         Ret := CyclesExhausted;
      end if;
   end ExecuteProgram;

   function DetectInvalidBehaviour(Prog : in Program;
                                   Cycles : in Integer) return Boolean 
    is
        CycleCount : Integer := 1;
        Inst : Instr;
        NewAddr : DataVal;
        NewRgValue: DataVal;
    begin
        while (CycleCount < Cycles) loop
            Inst := Prog(ProgramCounter(CycleCount));
            case Inst.Op is
                when ADD =>
                    -- validation
                    if -(2**31) > Regs(Inst.AddRd) or Regs(Inst.AddRd) > +(2**31 - 1) then
                        return True;
                    end if;
                    if -(2**31) > Regs(Inst.AddRs1) or Regs(Inst.AddRs1) > +(2**31 - 1) then
                        return True;
                    end if;
                    if -(2**31) > Regs(Inst.AddRs2) or Regs(Inst.AddRs2) > +(2**31 - 1) then
                        return True;
                    end if;
                    NewRgValue := Regs(Inst.AddRs1) + Regs(Inst.AddRs2);
                    if -(2**31) > NewRgValue or NewRgValue > +(2**31 - 1) then
                        return True;
                    end if;

                when SUB =>
                    -- validation
                    if -(2**31) > Regs(Inst.SubRd) or Regs(Inst.SubRd) > +(2**31 - 1) then
                        return True;
                    end if;
                    if -(2**31) > Regs(Inst.SubRs1) or Regs(Inst.SubRs1) > +(2**31 - 1) then
                        return True;
                    end if;
                    if -(2**31) > Regs(Inst.SubRs2) or Regs(Inst.SubRs2) > +(2**31 - 1) then
                        return True;
                    end if;
                    NewRgValue := Regs(Inst.SubRs1) - Regs(Inst.SubRs2);
                    if -(2**31) > NewRgValue or NewRgValue > +(2**31 - 1) then
                        return True;
                    end if;

                when MUL =>
                    -- validation
                    if -(2**31) > Regs(Inst.MulRd) or Regs(Inst.MulRd) > +(2**31 - 1) then
                        return True;
                    end if;
                    if -(2**31) > Regs(Inst.MulRs1) or Regs(Inst.MulRs1) > +(2**31 - 1) then
                        return True;
                    end if;
                    if -(2**31) > Regs(Inst.MulRs2) or Regs(Inst.MulRs2) > +(2**31 - 1) then
                        return True;
                    end if;
                    NewRgValue := Regs(Inst.MulRs1) * Regs(Inst.MulRs2);
                    if -(2**31) > NewRgValue or NewRgValue > +(2**31 - 1) then
                        return True;
                    end if;

                when DIV =>
                    -- validation
                    if Regs(Inst.DivRs2) = 0 then
                        return True;
                    end if;
                    if -(2**31) > Regs(Inst.DivRd) or Regs(Inst.DivRd) > +(2**31 - 1) then
                        return True;
                    end if;
                    if -(2**31) > Regs(Inst.DivRs1) or Regs(Inst.DivRs1) > +(2**31 - 1) then
                        return True;
                    end if;
                    if -(2**31) > Regs(Inst.DivRs2) or Regs(Inst.DivRs2) > +(2**31 - 1) then
                        return True;
                    end if;
                    NewRgValue := Regs(Inst.DivRs1) / Regs(Inst.DivRs2);
                    if -(2**31) > NewRgValue or NewRgValue > +(2**31 - 1) then
                        return True;
                    end if;

                when LDR =>
                    -- validation
                    if -(2**31) > Regs(Inst.LdrRd) or  Regs(Inst.LdrRd) > +(2**31 - 1) then
                        return True;
                    end if;
                    -- check if value of LdrRs is out of range
                    if -(2**31) > Regs(Inst.LdrRs) or  Regs(Inst.LdrRs) > +(2**31 - 1) then
                        return True;
                    end if;
                    -- check if computed address is invalid
                    NewAddr := Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs);
                    if NewAddr not in 0 .. 65535 then
                        return True;
                    end if;

                    -- check if value in the memory location is out of range
                    if -(2**31) > Memory(Addr(NewAddr)) or Memory(Addr(NewAddr)) > +(2**31 - 1) then
                        return True;
                    end if;

                when STR =>
                    -- check if value of StrRa is out of range
                    if -(2**31) > Regs(Inst.StrRa) or  Regs(Inst.StrRa) > +(2**31 - 1) then
                        return True;
                    end if;

                    -- check if value of StrRb is out of range
                    if -(2**31) > Regs(Inst.StrRb) or  Regs(Inst.StrRb) > +(2**31 - 1) then
                        return True;
                    end if;

                    -- check if computed address is invalid
                    NewAddr := Regs(Inst.StrRa) + DataVal(Inst.StrOffs);
                    if NewAddr not in 0 .. 65535 then
                        return True;
                    end if;

                when MOV =>
                    if -(2**31) > Regs(Inst.MovRd) or  Regs(Inst.MovRd) > +(2**31 - 1) then
                        return True;
                    end if;                

                when Instruction.RET =>
                    -- check if returning value RetRs is out of range
                    if -(2**31) > Regs(Inst.RetRs) or Regs(Inst.RetRs) > +(2**31 - 1) then
                        return True;
                    else   
                    -- successfully RET a valid value, so return False
                        return False;
                    end if;

                when JMP =>
                    -- check if there is a infinite loop by "JMP 0"
                    if Inst.JmpOffs = 0 then
                        return True;
                    end if;
                    -- check if JMP intends to alter the PC to become out of range
                    if Integer(PC) + Integer(Inst.JmpOffs) not in 1 .. MAX_PROGRAM_LENGTH then
                        return True;
                    end if;
                    CycleCount := CycleCount + Integer(Inst.JmpOffs) - 1;

                when JZ =>
                    -- check if value of JzRa is out of range
                    if -(2**31) > Regs(Inst.JzRa) or Regs(Inst.JzRa) > +(2**31 - 1) then
                        return True;
                    end if;

                    if Regs(Inst.JzRa) = 0 then
                        -- JzRa holds the value 0, so check if JZ intends to alter the PC to become out of range
                        if Integer(PC) + Integer(Inst.JzOffs) not in 1 .. MAX_PROGRAM_LENGTH then
                            return True;
                        -- JzRa holds the value 0, so check if there is a infinite loop by "JZ Ra 0"
                        elsif Integer(Inst.JzOffs) = 0 then
                            return True;
                        end if;
                        CycleCount := CycleCount + Integer(Inst.JzOffs) - 1;
                    end if;                  

                when NOP =>
                    null;

            end case;
            CycleCount := CycleCount + 1;
        end loop;

        -- RET instruction never execute, so return True
        return True;
   end DetectInvalidBehaviour;
   
end Machine;
