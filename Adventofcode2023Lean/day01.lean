def input := IO.FS.readFile "input01"

def isDigit (c : Char) : Bool :=
  '0' <= c && c <= '9'

def charDigitToInt (c : Char) : UInt32 := c.val - '0'.val

def getSumFirstLast (l : List UInt32) : UInt32 :=
  10 * (l.get! 0) + l.getLast!

def readDigits (input : String) : List (List UInt32) :=
  let l := String.splitOn input "\n"
  l.map (fun (s : String) => (s.toList.filter isDigit).map charDigitToInt)

def thePureTransformation (input : String) : UInt32 :=
  let ll := readDigits input
  let lll := ll.map getSumFirstLast
  List.foldl (fun a b => (a + b)) 0 lll

def readSplit (fileString : String) : IO (UInt32) :=
  do
  let input <- IO.FS.readFile fileString
  pure (thePureTransformation input)

#eval readSplit "input01"

-- TODO: part 2 does not seem like fun here
