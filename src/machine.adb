with Instruction;
use Instruction;
with Debug; use Debug;

-- used so we can print TAB character
with Ada.Characters.Latin_1;

package body Machine with SPARK_Mode is
   -- data values are 32-bit integers
   -- this is the type of words used in the virtual machine
   type DataVal is range -(2**31) .. +(2**31 - 1);
   -- define a type on bigger range for the easy of range and overflow checkings
   type GalaxyVal is range -(2**62)..+(2**62);
      
   -- the registers
   Regs : array (Reg) of DataVal := (others => 0);
   
   -- the memory
   Memory : array (Addr) of DataVal := (others => 0);
   
   -- the program counter
   PC : ProgramCounter := ProgramCounter'First;
      
   procedure IncPC(Ret : out ReturnCode; Offs : in Offset)
   is
      TempVal : GalaxyVal;
   begin
      -- validate the value in Offs
      if Offset'First > Offs or Offs > Offset'Last then
          Ret := IllegalProgram;
      -- increment pc by 0 is an infinite loop, so return CyclesExhausted immediately
      elsif Offs = 0 then
          Ret := CyclesExhausted;
      else
        TempVal := GalaxyVal(PC) + GalaxyVal(Offs);
        if TempVal >= 1 and TempVal <= 65536 then
          PC := ProgramCounter(Integer(TempVal));
          Ret := Success;
        else
          Ret := IllegalProgram;
        end if;
      end if;
   end IncPC;
   
   procedure DoAdd(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) 
   is
      TempVal : GalaxyVal;
   begin
      -- validate the value in Regs(Rd)
      if DataVal'First > Regs(Rd) or Regs(Rd) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rs1)
      elsif DataVal'First > Regs(Rs1) or Regs(Rs1) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rs2)
      elsif DataVal'First > Regs(Rs2) or Regs(Rs2) > DataVal'Last then
          Ret := IllegalProgram;
      else
        -- if pass all validations, compute the sum
        TempVal := GalaxyVal(Regs(Rs1)) + GalaxyVal(Regs(Rs2));
        -- validate their sum value
        if TempVal >= -(2**31) and TempVal <= (2**31 - 1) then
          Regs(Rd) := DataVal(TempVal);
          Ret := Success;
        else
          Ret := IllegalProgram;
        end if;
      end if;
   end DoAdd;
   
   procedure DoSub(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) 
   is
      TempVal : GalaxyVal;
   begin
      -- validate the value in Regs(Rd)
      if DataVal'First > Regs(Rd) or Regs(Rd) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rs1)
      elsif DataVal'First > Regs(Rs1) or Regs(Rs1) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rs2)
      elsif DataVal'First > Regs(Rs2) or Regs(Rs2) > DataVal'Last then
          Ret := IllegalProgram;
      else
        -- if pass all validations, compute the difference
        TempVal := GalaxyVal(Regs(Rs1)) - GalaxyVal(Regs(Rs2));
        -- validate their difference
        if TempVal >= -(2**31) and TempVal <= (2**31 - 1) then
          Regs(Rd) := DataVal(TempVal);
          Ret := Success;
        else
          Ret := IllegalProgram;
        end if;
      end if;
   end DoSub;
   
   procedure DoMul(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) 
   is
      TempVal : GalaxyVal;
   begin
      -- validate the value in Regs(Rd)
      if DataVal'First > Regs(Rd) or Regs(Rd) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rs1)
      elsif DataVal'First > Regs(Rs1) or Regs(Rs1) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rs2)
      elsif DataVal'First > Regs(Rs2) or Regs(Rs2) > DataVal'Last then
          Ret := IllegalProgram;
      else
        -- if pass all validation, compute the product
        TempVal := GalaxyVal(Regs(Rs1)) * GalaxyVal(Regs(Rs2));
        -- validate their product
        if TempVal >= -(2**31) and TempVal <= (2**31 - 1) then
          Regs(Rd) := DataVal(TempVal);
          Ret := Success;
        else
          Ret := IllegalProgram;
        end if;
      end if;
   end DoMul;
   
   procedure DoDiv(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) 
   is
      TempVal : GalaxyVal;
   begin
      -- validate if divisor is zero
      if Regs(Rs2) = 0 then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rd)
      elsif DataVal'First > Regs(Rd) or Regs(Rd) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rs1)
      elsif DataVal'First > Regs(Rs1) or Regs(Rs1) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rs2)
      elsif DataVal'First > Regs(Rs2) or Regs(Rs2) > DataVal'Last then
          Ret := IllegalProgram;
      else
        -- if pass all validations, compute the quotient
        TempVal := GalaxyVal(Regs(Rs1)) / GalaxyVal(Regs(Rs2));
        -- validate their quotient
        if TempVal >= -(2**31) and TempVal <= (2**31 - 1) then
          Regs(Rd) := DataVal(TempVal);
          Ret := Success;
        else
          Ret := IllegalProgram;
        end if;
      end if;
   end DoDiv;
   
   procedure DoLdr(Rd : in Reg; 
                   Rs : in Reg; 
                   Offs : in Offset;
                   Ret : out ReturnCode) 
   is
      TempVal : GalaxyVal;
   begin
      -- validate the value in Regs(Rd)
      if DataVal'First > Regs(Rd) or  Regs(Rd) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rs)
      elsif DataVal'First > Regs(Rs) or  Regs(Rs) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Offs
      elsif Offset'First > Offs or Offs > Offset'Last then
          Ret := IllegalProgram;
      else
        -- if individual reg or offs are valid, then compute the address
        TempVal := GalaxyVal(Regs(Rs)) + GalaxyVal(Offs);
        -- validate the computed address
        if TempVal in 0 .. 65535 then
            Regs(Rd) := Memory(Addr(TempVal));
            Ret := Success;
        else
            Ret := IllegalProgram;
        end if;
      end if;
   end DoLdr;
   
   procedure DoStr(Ra : in Reg;
                   Offs : in Offset;
                   Rb : in Reg;
                   Ret : out ReturnCode) 
   is
      TempVal : GalaxyVal;
   begin
      -- validate the value in Regs(Ra)
      if DataVal'First > Regs(Ra) or  Regs(Ra) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Regs(Rb)
      elsif DataVal'First > Regs(Rb) or  Regs(Rb) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Offs
      elsif Offset'First > Offs or Offs > Offset'Last then
          Ret := IllegalProgram;
      else
        -- if individual reg or offs are valid, then compute the address
        TempVal := GalaxyVal(Regs(Ra)) + GalaxyVal(Offs);
        -- validate the computed address
        if TempVal in 0 .. 65535 then
            Memory(Addr(TempVal)) := Regs(Rb);
            Ret := Success;
        else
            Ret := IllegalProgram;
        end if;
      end if;
   end DoStr;
   
   procedure DoMov(Rd : in Reg;
                   Offs : in Offset;
                   Ret : out ReturnCode) is
   begin
      -- validate the value in Regs(Rd)
      if DataVal'First > Regs(Rd) or  Regs(Rd) > DataVal'Last then
          Ret := IllegalProgram;
      -- validate the value in Offs
      elsif Offset'First > Offs or Offs > Offset'Last then
          Ret := IllegalProgram;
      else
          Regs(Rd) := DataVal(Offs);
          Ret := Success;
      end if;
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
                -- terminate the loop immediately if Ret is not Success
                if Ret /= Success then
                    return;
                end if;
               IncPC(Ret,1);
            when SUB =>
               DoSub(Inst.SubRd,Inst.SubRs1,Inst.SubRs2,Ret);
               -- terminate the loop immediately if Ret is not Success
                if Ret /= Success then
                    return;
                end if;
               IncPC(Ret,1);
            when MUL =>
               DoMul(Inst.MulRd,Inst.MulRs1,Inst.MulRs2,Ret);
               -- terminate the loop immediately if Ret is not Success
                if Ret /= Success then
                    return;
                end if;
               IncPC(Ret,1);
            when DIV =>
               DoDiv(Inst.DivRd,Inst.DivRs1,Inst.DivRs2,Ret);
               -- terminate the loop immediately if Ret is not Success
                if Ret /= Success then
                    return;
                end if;
               IncPC(Ret,1);
            when LDR =>
               DoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs,Ret);
               -- terminate the loop immediately if Ret is not Success
                if Ret /= Success then
                    return;
                end if;
               IncPC(Ret,1);
            when STR =>
               DoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb,Ret);
               -- terminate the loop immediately if Ret is not Success
                if Ret /= Success then
                    return;
                end if;
               IncPC(Ret,1);
            when MOV =>
               DoMov(Inst.MovRd,Inst.MovOffs,Ret);
               -- terminate the loop immediately if Ret is not Success
                if Ret /= Success then
                    return;
                end if;
               IncPC(Ret,1);
            when Instruction.RET =>
                -- validate the value in Regs(Inst.RetRs)
                -- terminate the loop immediately if invalid             
                if DataVal'First > Regs(Inst.RetRs) or Regs(Inst.RetRs) > DataVal'Last then
                    Ret := IllegalProgram;
                else
                    -- execute Ret
                    Result := Integer(Regs(Inst.RetRs));
                    Ret := Success;
                end if;
                return;
            when JMP =>
               IncPC(Ret,Inst.JmpOffs);
            when JZ =>
                -- validate the value in Regs(Inst.JzRa)
                if DataVal'First > Regs(Inst.JzRa) or Regs(Inst.JzRa) > DataVal'Last then
                    Ret := IllegalProgram;
                    return;
                end if;
                -- execute JZ
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
        TempVal: GalaxyVal;
        -- the dummy registers for this function only
        DummyRegs : array (Reg) of DataVal;
        -- the dummy memory for this function only
        DummyMemory : array (Addr) of DataVal;
    begin
        -- replicate Regs array
        for I in reg loop
             DummyRegs(I) := Regs(I);   
        end loop;

        -- replicate Memory array
        for I in Addr loop
             DummyMemory(I) := Memory(I);   
        end loop;

        while (CycleCount < Cycles) loop
            if CycleCount in 1 .. MAX_PROGRAM_LENGTH then 
              -- get next instruction if CycleCount is valid
              Inst := Prog(ProgramCounter(CycleCount));
            else
              -- or break loop if invalid
              exit;
            end if;

            case Inst.Op is
                when ADD =>
                    -- validation
                    if DataVal'First > Regs(Inst.AddRd) or Regs(Inst.AddRd) > DataVal'Last then
                        return True;
                    end if;
                    if DataVal'First > Regs(Inst.AddRs1) or Regs(Inst.AddRs1) > DataVal'Last then
                        return True;
                    end if;
                    if DataVal'First > Regs(Inst.AddRs2) or Regs(Inst.AddRs2) > DataVal'Last then
                        return True;
                    end if;

                    -- if individual reg is valid, then compute                
                    TempVal := GalaxyVal(Regs(Inst.AddRs1)) + GalaxyVal(Regs(Inst.AddRs2));
                    -- validate the computed result
                    if -(2**31) > TempVal or TempVal > (2**31 - 1) then
                        return True;
                    end if;
                    -- if computed result is valid, then put into dummy register
                    DummyRegs(Inst.AddRd) := DataVal(TempVal);

                when SUB =>
                    -- validation
                    if DataVal'First > Regs(Inst.SubRd) or Regs(Inst.SubRd) > DataVal'Last then
                        return True;
                    end if;
                    if DataVal'First > Regs(Inst.SubRs1) or Regs(Inst.SubRs1) > DataVal'Last then
                        return True;
                    end if;
                    if DataVal'First > Regs(Inst.SubRs2) or Regs(Inst.SubRs2) > DataVal'Last then
                        return True;
                    end if;

                    -- if individual reg is valid, then compute                
                    TempVal := GalaxyVal(Regs(Inst.SubRs1)) - GalaxyVal(Regs(Inst.SubRs2));
                    -- validate the computed result                    
                    if -(2**31) > TempVal or TempVal > (2**31 - 1) then
                        return True;
                    end if;                    
                    -- if computed result is valid, then put into dummy register
                    DummyRegs(Inst.SubRd) := DataVal(TempVal);

                when MUL =>
                    -- validation
                    if DataVal'First > Regs(Inst.MulRd) or Regs(Inst.MulRd) > DataVal'Last then
                        return True;
                    end if;
                    if DataVal'First > Regs(Inst.MulRs1) or Regs(Inst.MulRs1) > DataVal'Last then
                        return True;
                    end if;
                    if DataVal'First > Regs(Inst.MulRs2) or Regs(Inst.MulRs2) > DataVal'Last then
                        return True;
                    end if;

                    -- if individual reg is valid, then compute                
                    TempVal := GalaxyVal(Regs(Inst.MulRs1)) * GalaxyVal(Regs(Inst.MulRs2));
                    -- validate the computed result                    
                    if -(2**31) > TempVal or TempVal > (2**31 - 1) then
                        return True;
                    end if;                    
                    -- if computed result is valid, then put into dummy register
                    DummyRegs(Inst.MulRd) := DataVal(TempVal);                    

                when DIV =>
                    -- validation
                    if Regs(Inst.DivRs2) = 0 then
                        return True;
                    end if;
                    if DataVal'First > Regs(Inst.DivRd) or Regs(Inst.DivRd) > DataVal'Last then
                        return True;
                    end if;
                    if DataVal'First > Regs(Inst.DivRs1) or Regs(Inst.DivRs1) > DataVal'Last then
                        return True;
                    end if;
                    if DataVal'First > Regs(Inst.DivRs2) or Regs(Inst.DivRs2) > DataVal'Last then
                        return True;
                    end if;

                    -- if individual reg is valid, then compute                
                    TempVal := GalaxyVal(Regs(Inst.DivRs1)) / GalaxyVal(Regs(Inst.DivRs2));
                    -- validate the computed result                    
                    if -(2**31) > TempVal or TempVal > (2**31 - 1) then
                        return True;
                    end if;                    
                    -- if computed result is valid, then put into dummy register
                    DummyRegs(Inst.DivRd) := DataVal(TempVal); 

                when LDR =>
                    -- validate the value in LdrRd
                    if DataVal'First > Regs(Inst.LdrRd) or Regs(Inst.LdrRd) > DataVal'Last then
                        return True;
                    end if;
                    -- validate the value in LdrRs
                    if DataVal'First > Regs(Inst.LdrRs) or Regs(Inst.LdrRs) > DataVal'Last then
                        return True;
                    end if;
                    -- validate the value in LdrOffs
                    if Offset'First > Inst.LdrOffs or Inst.LdrOffs > Offset'Last then
                        return True;
                    end if;
                    -- if individual reg or offs are valid, then compute address
                    TempVal := GalaxyVal(Regs(Inst.LdrRs)) + GalaxyVal(Inst.LdrOffs);
                    -- validate the computed address
                    if TempVal not in 0 .. 65535 then
                        return True;
                    end if;

                    -- validate the value stored in the computed address
                    if DataVal'First > Memory(Addr(TempVal)) 
                        or Memory(Addr(TempVal)) > DataVal'Last then
                        return True;
                    end if;

                    -- if pass all validations, then read value from the computed address
                    -- and put into register
                    DummyRegs(Inst.LdrRd) := DummyMemory(Addr(TempVal));

                when STR =>
                    -- validate the value in StrRa
                    if DataVal'First > Regs(Inst.StrRa) or Regs(Inst.StrRa) > DataVal'Last then
                        return True;
                    end if;
                    -- validate the value in StrRb
                    if DataVal'First > Regs(Inst.StrRb) or Regs(Inst.StrRb) > DataVal'Last then
                        return True;
                    end if;
                    -- validate the value in StrOffs
                    if Offset'First > Inst.StrOffs or Inst.StrOffs > Offset'Last then
                        return True;
                    end if;

                    -- if individual reg or offs are valid, then compute address
                    TempVal := GalaxyVal(Regs(Inst.StrRa)) + GalaxyVal(Inst.StrOffs);
                    -- validate the computed address
                    if TempVal not in 0 .. 65535 then
                        return True;
                    end if;

                    -- if pass all validations, then read value from the register 
                    -- and put into the computed address
                    DummyMemory(Addr(TempVal)) := DummyRegs(Inst.StrRb);

                when MOV =>
                    -- validate the value in MovRd                
                    if DataVal'First > Regs(Inst.MovRd) or Regs(Inst.MovRd) > DataVal'Last then
                        return True;
                    end if;
                    -- validate MovOffs                
                    if Offset'First > Inst.MovOffs or Inst.MovOffs > Offset'Last then
                        return True;
                    end if;

                    -- if all regs and offs are valid, then put the value(offs) into register 
                    DummyRegs(Inst.MovRd) := DataVal(Inst.MovOffs);       

                when Instruction.RET =>
                    -- validate the value in RetRs                
                    if DataVal'First > Regs(Inst.RetRs) or Regs(Inst.RetRs) > DataVal'Last then
                        return True;
                    else   
                    -- if the value is valid, then return. 
                        return False;
                    end if;

                when JMP =>
                    -- check if there is a infinite loop by "JMP 0"
                    if Inst.JmpOffs = 0 then
                        return True;
                    end if;
                    -- check if value of JmpOffs is out of range
                    if Offset'First > Inst.JmpOffs or Inst.JmpOffs > Offset'Last then
                        return True;
                    end if;  
                    -- check if JMP intends to alter the PC to become out of range
                    if Integer(PC) + Integer(Inst.JmpOffs) not in 1 .. MAX_PROGRAM_LENGTH then
                        return True;
                    end if;
                    CycleCount := CycleCount + Integer(Inst.JmpOffs) - 1;

                when JZ =>
                    -- check if value of JzRa is out of range
                    if DataVal'First > Regs(Inst.JzRa) or Regs(Inst.JzRa) > DataVal'Last then
                        return True;
                    end if;
                    -- check if value of JzOffs is out of range
                    if Offset'First > Inst.JzOffs or Inst.JzOffs > Offset'Last then
                        return True;
                    end if; 
                    if Regs(Inst.JzRa) = 0 then
                        -- if JzRa holds the value 0, 
                        -- check if JZ intends to alter the PC to become out of range
                        if Integer(PC) + Integer(Inst.JzOffs) not in 1 .. MAX_PROGRAM_LENGTH then
                            return True;
                        -- JzRa holds the value 0, so check if there is a infinite loop by "JZ Ra 0"
                        elsif Integer(Inst.JzOffs) = 0 then
                            return True;
                        end if;
                    end if;

                    -- check on dummy register and execute JZ
                    if DummyRegs(Inst.JzRa) = 0 then  
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
