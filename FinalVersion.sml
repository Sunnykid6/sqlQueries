(*
Victor Sun
V00894734
*)
structure Babies =
struct
local
  open Csc330
in
  
fun babies_program (fileName, yearSt) =
    let
      (* I think  we do not need the first two  since those were examples *)
      (* val _ = print ("Filename to read " ^ fileName ^ "\n") *)
      (* val _ = print ("First year  " ^ yearSt ^ "\n") *)
      val readFile = read_file(fileName)
      val newLine = #"\n"
      val comma = #","
      val dataSplit = split_at(readFile, newLine)
      val splitTextAgain = split_at(hd dataSplit, comma)
      val inputFile = read_stdin()
      val inputSplit = split_at(inputFile, newLine)

      fun getLength(list : string list)=
        if null (tl list)
        then 1
        else 1 + getLength(tl list)

      val firstYear = yearSt
      val totalYears = (getLength(splitTextAgain) -2)
      val lastYear = valOf(fromString(yearSt)) + totalYears - 1
      (* Main function to parse through the babylist and find data given by the name to check *)
      fun parseInfo(parseList : string list, nameToCheck : string list)=
        if null(nameToCheck)
        then print("")
        else
          let
            
            (* Recursively find the max: Help from dan grossman's code *)
            fun getMax(foundBabyNameList: string list)=
              if null(tl (tl foundBabyNameList))
              then hd foundBabyNameList
              else 
                let
                  val tl_ans = getMax(tl foundBabyNameList)
                in
                  if valOf(fromString(hd foundBabyNameList)) > valOf(fromString(tl_ans))
                  then hd foundBabyNameList
                  else tl_ans
                end
            
            (* Helper function for getMax to get the year: comparing findthis string to find the last occurence*) 
            fun findLastOccuring(foundBabyNameList: string list, findThis:string, final:int, count: int)=
              if null(tl (tl  foundBabyNameList))
              then 
                if hd foundBabyNameList = findThis
                then final + (count - final)
                else final
              else
                if hd foundBabyNameList = findThis
                then findLastOccuring(tl foundBabyNameList, findThis, final +(count - final), count + 1)
                else findLastOccuring(tl foundBabyNameList, findThis, final, count + 1)
            
            (* Recursively find the min: help from dan grossman's max function*)
            fun getMin(foundBabyNameList: string list, seenNumber: string)=
              if null(tl (tl foundBabyNameList))
              then
                if hd foundBabyNameList = int_to_string(0)
                then seenNumber
                else hd foundBabyNameList
              else
                let
                  val tl_ans = getMin(tl foundBabyNameList, hd foundBabyNameList)
                in
                  if tl_ans = int_to_string(0)
                  then hd foundBabyNameList
                  else
                    if valOf(fromString(hd foundBabyNameList)) < valOf(fromString(tl_ans)) andalso hd foundBabyNameList <> int_to_string(0)
                    then hd foundBabyNameList
                    else tl_ans
                end

            (* Helper function for getMin to get the year: comparing findthis string to find the first occurence*)
            fun findFirstOccuring(foundBabyNameList: string list, findThis: string, final:int)=
              if hd foundBabyNameList = findThis
              then final
              else findFirstOccuring(tl foundBabyNameList, findThis, final+1)
            
            (* Helper function to format min and max strings as well as add the years*)
            fun minMaxString(minMax: string, count: int, flag: int)=
              if flag = 0
              then " Min: " ^ int_to_string(valOf(fromString(firstYear)) + count) ^ " " ^ minMax ^ "\n"
              else " Max: " ^ int_to_string(valOf(fromString(firstYear)) + count) ^ " " ^ minMax ^ "\n"

            (* Recursively find the last non zero and store the it as a pair (string int value, index) *)      
            fun getLastNonZero(foundBabyNameList: string list, final: int, count: int, seenNumber: string)=
              if null(tl(tl foundBabyNameList))
              then
                if hd foundBabyNameList = int_to_string(0)
                then (seenNumber, final)
                else (hd foundBabyNameList, final + (count - final))
              else
                if hd foundBabyNameList = int_to_string(0)
                then getLastNonZero(tl foundBabyNameList, final, count + 1, seenNumber)
                else getLastNonZero(tl foundBabyNameList, final + (count - final), count + 1, hd foundBabyNameList)

            (* Recursively find the first non zero and store it as a pair (string int value, index) *)
            fun getfirstNonZero(foundBabyNameList: string list, count: int)=
              if hd foundBabyNameList = int_to_string(0)
              then (getfirstNonZero(tl foundBabyNameList, count + 1))
              else (hd foundBabyNameList, count)
            
            (* Helper function to format strings for First and Last*)
            fun firstString(tuples: string*int, flag: int)=
              if flag = 0
              then" First: " ^ int_to_string((valOf(fromString(firstYear))) + #2 tuples) ^ " " ^ #1 tuples ^ "\n"
              else" Last: " ^ int_to_string((valOf(fromString(firstYear))) + #2 tuples) ^ " " ^ #1 tuples ^ "\n"
            
            (* Recursively find the total, the lastYears value, and the number of non zero values stored as a (string*string*int) *)
            fun getTotal(foundBabyNameList : string list, lastYear: string,totalNonZero: int)=
              if null(tl (tl foundBabyNameList))
              then 
                if hd foundBabyNameList = int_to_string(0)
                then (hd (tl foundBabyNameList), hd foundBabyNameList, totalNonZero)
                else (hd (tl foundBabyNameList), hd foundBabyNameList, totalNonZero + 1)
              else
                if hd foundBabyNameList = int_to_string(0)
                then getTotal(tl foundBabyNameList, hd foundBabyNameList,totalNonZero)
                else getTotal(tl foundBabyNameList, hd foundBabyNameList,totalNonZero + 1)
            
            (* Helper function for getMax to get the year: comparing findthis string to find the last occurence*)
            fun totalString(tuples: string*string*int)=
              (" Total: " ^ int_to_string(valOf(fromString(#1 tuples))) ^ "\n" ^ " Years: " ^ int_to_string(#3
              tuples) ^ "\n" ^ " " ^ int_to_string(lastYear) ^ ": " ^ #2 tuples ^ "\n")
            
            (* Function to calculate the avg and format the string for it*)
            fun calcAvg(tuples: string*string*int)=
              " Avg: "  ^ real_to_string(int_to_real(valOf(fromString(#1 tuples))) /
              int_to_real(totalYears)) ^ "\n" 
            
            (* Function to go through the babies text and compare it to a name to determine if it exists*)
            (* if it does not exist then print "baby was not found else call function above to print info*)
            fun getInfo(anybabyList: string list, name: string)=
              if hd (split_at(hd anybabyList, comma)) = name
              then hd (split_at(hd anybabyList,comma)) ^ "\n" ^
                   totalString(getTotal(tl(split_at(hd anybabyList, comma)), "", 0)) ^ 
                   firstString(getfirstNonZero(tl(split_at(hd anybabyList, comma)),0),0) ^ 
                   firstString(getLastNonZero(tl(split_at(hd anybabyList, comma)), 0, 0, ""), 1) ^
                   minMaxString(getMin(tl(split_at(hd anybabyList, comma)), "") ,
                     (findFirstOccuring(tl(split_at(hd anybabyList, comma)), 
                       getMin(tl(split_at(hd anybabyList, comma)), ""),0)), 0) ^
                   minMaxString(getMax(tl(split_at(hd anybabyList, comma))),
                     (findLastOccuring(tl(split_at(hd anybabyList, comma)), 
                       getMax(tl(split_at(hd anybabyList, comma))), 0, 0)), 1) ^
                   calcAvg(getTotal(tl(split_at(hd anybabyList, comma)), "", 0))
              else
                if null(tl anybabyList)
                then
                  if hd(split_at(hd anybabyList, comma)) = name
                  then hd (split_at(hd anybabyList, comma)) ^ "\n" ^
                       totalString(getTotal(tl(split_at(hd anybabyList, comma)), "",0)) ^ 
                       firstString(getfirstNonZero(tl(split_at(hd anybabyList,comma)),0), 0) ^
                       firstString(getLastNonZero(tl(split_at(hd anybabyList, comma)),0, 0, ""), 1) ^
                       minMaxString(getMin(tl(split_at(hd anybabyList,comma)), "") ,
                         (findFirstOccuring(tl(split_at(hd anybabyList, comma)),
                           getMin(tl(split_at(hd anybabyList, comma)), ""),0)), 0) ^ 
                       minMaxString(getMax(tl(split_at(hd anybabyList, comma))),
                         (findLastOccuring(tl(split_at(hd anybabyList, comma)),
                           getMax(tl(split_at(hd anybabyList, comma))), 0, 0)), 1) ^
                       calcAvg(getTotal(tl(split_at(hd anybabyList, comma)), "", 0))
                  else name ^ "\n" ^ "Baby name [" ^ name ^ "] was not found" ^ "\n"
                else getInfo(tl anybabyList, name)
            
            (* go through the stdin names and call the getInfo checker for each name*)
            fun cycleThrough(nameList: string list)=  
              if null (tl nameList)
              then getInfo(parseList, hd nameList)
              else getInfo(parseList, hd nameList) ^ cycleThrough(tl nameList)
            val temp = cycleThrough(nameToCheck)
          in
            (* print("Read " ^ int_to_string(getLength(dataSplit)) ^ "babies" ^ dot
            ^ " Staring year " ^ yearSt ^ dot ^ " Each baby has " ^
            int_to_string(totalYears) ^ "entries" ^ dot ^ "\n"); *)
            print(temp);
            ()
          end
          
    in  
      print("Read " ^ int_to_string (getLength(dataSplit)) ^ " babies" ^ dot
      ^ " Starting year " ^ yearSt ^ dot ^ " Each baby has " ^ int_to_string
      (totalYears)^ " entries" ^ dot ^ "\n"); 
      parseInfo(dataSplit, inputSplit);
    ()
    end
        

(*
do not modify below this point
*)
        
fun main (prog_name, args) =
    let
      val (_, fileName, offsetSt) = parse_command_line args
      val _ = babies_program(fileName, offsetSt)
    in
      exit()
    end

end

end
