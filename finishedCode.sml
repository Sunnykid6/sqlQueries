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
        
    in
      if null (inputSplit)
      then print("No names to process ending program")
      else
        let
          fun getLength(list : string list)=
          if null (tl list)
          then 1
          else 1 + getLength(tl list)

          val firstYear = yearSt
          val totalYears = (getLength(splitTextAgain) -2)
          val lastYear = valOf(fromString(yearSt)) + totalYears - 1

          fun parseInfo(parseList : string list, nameToCheck : string list)=
            let
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

              fun findLastOccuring(foundBabyNameList: string list, findThis: string, final:int, count: int)=
                if null(tl foundBabyNameList)
                then final
                else
                  if hd foundBabyNameList = findThis
                  then findLastOccuring(tl foundBabyNameList, findThis, final + (count - final), count + 1)
                  else findLastOccuring(tl foundBabyNameList, findThis, final, count + 1)
          
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
                      if valOf(fromString(hd foundBabyNameList)) <
                      valOf(fromString(tl_ans)) andalso hd foundBabyNameList <> int_to_string(0)
                      then hd foundBabyNameList
                      else tl_ans
                  end


              fun findFirstOccuring(foundBabyNameList: string list, findThis: string, final:int)=
                if hd foundBabyNameList = findThis
                then final
                else findFirstOccuring(tl foundBabyNameList, findThis, final+1)
          
              fun minMaxString(minMax: string, count: int, flag: int)=
                if flag = 0
                then " Min: " ^ int_to_string(valOf(fromString(firstYear)) + count) ^ " " ^ minMax ^ "\n"
                else " Max: " ^ int_to_string(valOf(fromString(firstYear)) + count) ^ " " ^ minMax ^ "\n"


              fun getLastNonZero(foundBabyNameList: string list, final: int, count: int, seenNumber: string)=
                if null(tl(tl foundBabyNameList))
                then
                  if hd foundBabyNameList = int_to_string(0)
                  then (seenNumber, final)
                  else (hd foundBabyNameList, final + 1)
                else
                  if hd foundBabyNameList = int_to_string(0)
                  then getLastNonZero(tl foundBabyNameList, final, count + 1, seenNumber)
                  else getLastNonZero(tl foundBabyNameList, final + (count - final), count + 1, hd foundBabyNameList)


              fun getfirstNonZero(foundBabyNameList: string list, count: int)=
                if hd foundBabyNameList = int_to_string(0)
                then (getfirstNonZero(tl foundBabyNameList, count + 1))
                else (hd foundBabyNameList, count)
          
              fun firstString(tuples: string*int, flag: int)=
                if flag = 0
                then" First: " ^ int_to_string((valOf(fromString(firstYear))) + #2 tuples) ^ " " ^ #1 tuples ^ "\n"
                else" Last: " ^ int_to_string((valOf(fromString(firstYear))) + #2 tuples) ^ " " ^ #1 tuples ^ "\n"

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

              fun totalString(tuples: string*string*int)=
                (" Total: " ^ int_to_string(valOf(fromString(#1 tuples))) ^ "\n" ^ " Years: " ^ int_to_string(#3
                tuples) ^ "\n" ^ " " ^ int_to_string(lastYear) ^ ": " ^ #2 tuples ^ "\n")
          
              fun calcAvg(tuples: string*string*int)=
                " Avg: "  ^ real_to_string(int_to_real(valOf(fromString(#1 tuples))) /
                int_to_real(totalYears)) ^ "\n" 

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

              fun cycleThrough(nameList: string list)=  
                if null (tl nameList)
                then getInfo(parseList, hd nameList)
                else getInfo(parseList, hd nameList) ^ cycleThrough(tl nameList)
              val temp = cycleThrough(nameToCheck)
            in
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
    