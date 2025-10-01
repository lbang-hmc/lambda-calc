import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List
import qualified System.Environment
import qualified System.IO


import Core
import qualified Testing as T

--
-- Test Specifications
--

disables :: Parser [String]
disables =
  many (do char '-'; macroId)

testParser :: Parser Test
testParser =
  (do char '+'
      points <- number
      disables <- disables
      testName <- many (noneOf "\n")
      char '\n'
      ch '?'
      testExpr <- expr
      expectations <- many1 (do ch '!'; expr)
      return $ DoTest (testName, points, disables, testExpr, expectations)) <|>
  (do string "\"\"\""
      s <- many (noneOf "\"")
      string "\"\"\""
      whitespace
      return $ PrintIt s) <|>
  (do string "*SHOWPOINTS*"
      whitespace
      return $ PrintTotal)

testsParser :: Parser [Test]
testsParser =
  do result <- many testParser
     eof
     return result

parseTests :: String -> IO [Test]
parseTests filename =
   do result <- parseFromFile testsParser filename
      case result of
        Left err -> error (show err)
        Right tests -> return tests

--
-- Main routine
--

parseArgs :: [String] -> (String, Int, String)
parseArgs args =
  let (testFile, args1) =
        case args of
          "-t" : testFile: rest -> (testFile, rest)
          _ -> ("cs131-tests", args)
      (budget, args2) =
        case args1 of
          "-b" : b : rest -> (read b, rest)
          _ -> (500000, args1)
      studentFile =
        case args2 of
          [sf] -> sf
          [] -> error "Missing student filename"
          _ -> error "Too many command-line arguments"
  in (testFile, budget, studentFile)


main =
  do args <- System.Environment.getArgs
     let (testFile, budget, studentFile) = parseArgs args
     env <- parseFile studentFile
     tests' <- parseTests testFile
     let tests = expandTests env 20000 tests'
     let outOf = totalPoints tests
     (_, points) <- runTests env budget (0, outOf) tests
     return ()


{-

eagerMacros = True
eagerPairs = True

whnfStep :: Env -> Expr -> S.Set Identifier -> Maybe Expr
whnfStep env e avoid =
  case e of
    App (Macro "Succ") ( Num n) -> Just $ Num (n+1)
    App (Macro "Pred") ( Num n) -> Just $ Num (max 0 (n-1))
    App (Macro "IsZero") (Num n) ->
       Just (if n == 0 then Macro "True" else Macro "False")

    App (Macro "Fst") (Pair e1 e2) -> Just e1
    App (Macro "Snd") (Pair e1 e2) -> Just e2
    App (Macro "Not") (Macro "True") -> Just (Macro "False")
    App (Macro "Not") (Macro "False") -> Just (Macro "True")

    App (Pair e1 e2) e3 -> Just $ App (App e3 e1) e2

    App (Macro p) e2
      | (eagerMacros || elem p ["Succ", "Pred", "IsZero", "Fst", "Snd", "Not", "Y"]),
        Just e2' <- whnfStep env e2 avoid ->
      Just $ App (Macro p) e2'

    App (App (Macro "Plus") (Num n)) (Num m) -> Just $ Num (n+m)
    App (App (Macro "Add") (Num n)) (Num m) -> Just $ Num (n+m)
    App (App (Macro "Monus") (Num n)) (Num m) -> Just $ Num (if n > m then n-m else 0)
    App (App (Macro "Times") (Num n)) (Num m) -> Just $ Num (n*m)
    App (App (Macro "Mult") (Num n)) (Num m) -> Just $ Num (n*m)

    App (App (Macro "Pair") e1) e2 ->
       Just $ Pair e1 e2

    Pair e1 e2
       | eagerPairs,
         Just e1' <- whnfStep env e1 avoid ->
           Just $ Pair e1' e2

    Pair e1 e2
       | eagerPairs,
         Just e2' <- whnfStep env e2 avoid ->
           Just $ Pair e1 e2'

    App (App (Macro "True") e1) e2 ->
       Just e1
    App (App (Macro "False") e1) e2 ->
       Just e2
    App (Macro "If") e1 ->
       Just e1

    App (Macro p) e1
           | elem p ["Plus", "Add", "Monus", "Times", "Mult"],
             Just e1' <- whnfStep env e1 avoid ->
           Just $ App (Macro p) e1'


    App (App (Macro p) (Num n)) e1
           | (eagerMacros || elem p ["Plus", "Add", "Monus", "Times", "Mult"]),
             Just e1' <- whnfStep env e1 avoid ->
           Just $ App (App (Macro p) (Num n)) e1'



{-
    App (App (Macro "Y") (Macro f)) e1
           | eagerMacros,
             Just e1' <- whnfStep env e1 avoid ->
           Just $ App (App (Macro "Y") (Macro f)) e1'

    App (App (Macro "Y") (Lam f ef)) e1
           | eagerMacros,
             Just e1' <- whnfStep env e1 avoid ->
           Just $ App (App (Macro "Y") (Lam f ef)) e1'
           -}

    App (App (Num 0) e1) e2 -> Just e2

    App (App (Num n) f) e2
       | Nothing <- whnfStep env f avoid ->
           case whnfStep env e2 avoid of
             Just e2' -> Just $ App (App (Num n) f) e2'
             Nothing -> Just $ App (App (Num (n-1)) f) (App f e2)

    App (g @ (App (Macro "Y") (Lam f e1))) e2
       | Just e2' <- whnfStep env e2 avoid ->
       Just $ App g e2'

    App (Macro "Y") e1
       | M.member "Y" env -> Just $ App e1 e

    App (Num n) (Num m) -> Just $ Num (m ^ n)

    (Num n) ->
       Just $ Lam "f" (Lam "b" (foldr App (Var "b") (take n (repeat (Var "f")))))

    Macro m ->
       case M.lookup m env of
         Just defn -> Just defn
         Nothing   -> Nothing

    App e1 e2 ->
      case e1 of
        Lam x e1' ->
          Just (substV e1' x e2 (S.insert x avoid))
        _ -> case whnfStep env e1 avoid of
                 Just e1' -> Just (App e1' e2)
                 Nothing  -> case whnfStep env e2 avoid of
                              Just e2' -> Just $ App e1 e2'
                              Nothing -> Nothing
    _ -> Nothing

whnf :: Env -> (Int, Expr) -> S.Set Identifier -> Maybe (Int, Expr)
whnf env (budget, e) avoid =
  if budget <= 0 then
    Nothing
  else if (size e > 10000) then
    Nothing
  else
    case whnfStep env e avoid of
        Nothing -> Just (budget, e)
        Just e' ->
          if (e /= e') then
            whnf env (budget-1, e') avoid
          else
            Nothing

whnfTrace :: Env -> (Int, Expr) -> S.Set Identifier -> IO ()
whnfTrace env (budget, e) avoid =
  if budget <= 0 then
     putStrLn "..."
  else
    do putStrLn (show e ++ " / " ++ show avoid)
       case whnfStep env e avoid of
          Nothing -> return ()
          Just e' -> whnfTrace env (budget-1, e') avoid



cbn env (budget, e) avoid =
    case (whnf env (budget, e) avoid) of
      Nothing -> Nothing
      Just (budget', e') ->
        case e' of
          Lam x e'' ->
            case cbn env (budget', e'') (S.insert x avoid) of
              Just (budget'', e3) -> Just (budget'', Lam x e3)
              Nothing -> Nothing
          App e1 e2 ->
            case cbn env (budget', e1) avoid of
              Just (budget'', e1') ->
                case cbn env (budget'', e2) avoid of
                  Just (budget''', e2')  -> Just (budget''', App e1' e2')
                  Nothing -> Nothing
              Nothing -> Nothing
          Pair e1 e2 ->
            case cbn env (budget', e1) avoid of
              Just (budget'', e1') ->
                case cbn env (budget'', e2) avoid of
                  Just (budget''', e2')  -> Just (budget''', Pair e1' e2')
                  Nothing -> Nothing
              Nothing -> Nothing
          _ -> Just (budget', e')

-}


--
-- Evaluation
--

let mkTestingCtx env = Ctx { shortcuts = False
                           , expandUnappliedNumerals = False
                           , environment = env
                           , showTrace = False
                           , lambdaChar = 'λ`
                           , allParens = False
                           , showColor = False
                           , lastFilename = Nothing
                           }


data Test = DoTest (String, Int, [String], Expr, [Expr])
          | PrintIt String
          | PrintTotal

disableEnv names env =
  M.filterWithKey (\k -> \_ -> not (elem k names)) env


expandTest :: Env -> Int -> Test -> Test
expandTest env budget (DoTest (name, points, disables, input, expected)) =
  let env' = disableEnv disables env
  in DoTest (name, points, disables, input, Data.List.nub (concat (map (\x -> [x, cBN env' budget x]) expected)))
expandTest env _ t = t

expandTests env budget tests =
  map (expandTest env budget) tests

getPoints (DoTest(_,p,_,_,_)) = p
getPoints _ = 0

totalPoints tests = sum (map getPoints tests)

{-
cBN env budget e =
  case (cbn env (budget, e) (fv e)) of
    Nothing -> error ("Can't normalize the expected term " ++ show e ++ " in budget " ++ show budget)
    Just (_,e') -> e'
-}


spacing str1 str3 =
  let linewidth = 72
      nspaces = 72 - length str1 - length str3
  in if nspaces >= 2 then
       take (linewidth - length str1 - length str3) (repeat ' ')
     else
       "\n" ++ take (linewidth - length str3) (repeat ' ')


runTests :: Env -> Int -> (Int,Int) -> [Test] -> IO (Bool, Int)
runTests env budget (pts,outOf) [] = return (True, pts)
runTests env budget (pts,outOf) (DoTest(name, points, disabled, input, expected) : rest)  =
  do -- putStrLn ("Test:" ++ name)
     let env' = disableEnv disabled env
     (result, points) <-
       case (cbn env' (budget, input) (fv input)) of
         Just (_, output) ->
           do
              let result = (elem output expected)
              let p = if result then points else 0
              let str1 = " " ++ show input ++ " --> " ++ show output
              let str3 = if result then ("+" ++ show p ++ " points") else (show p ++ " points")
              let str2 = spacing str1 str3
              putStrLn (str1 ++ str2 ++ str3)
              if (not result) then putStrLn ("  expected  " ++ Data.List.intercalate "  or  " (map show expected)) else return ()

              return (result, p)
         Nothing ->
           do
              let p = 0
              let str1 = " " ++ show input ++ " --> [TIMEOUT or BLOWUP]"
              let str3 = ("0/" ++ show points)
              let str2 = spacing str1 str3
              putStrLn (str1 ++ str2 ++ str3)
              putStrLn ("   expected  " ++ Data.List.intercalate "  or  " (map show expected))
              {-whnfTrace env' (1000, input) (fv input)-}
              return (False, 0)

     System.IO.hFlush System.IO.stdout
     (results, pointss) <- runTests env budget (pts + points, outOf) rest
     return $ (result && results, pointss)

runTests env budget (pts,outOf) (PrintTotal : rest) =
  do putStrLn ("\nAUTOGRADED POINTS: " ++ show pts ++ "/" ++ show outOf)
     System.IO.hFlush System.IO.stdout
     runTests env budget (pts,outOf) rest

runTests env budget (pts,outOf) (PrintIt s : rest) =
  do putStrLn s
     System.IO.hFlush System.IO.stdout
     runTests env budget (pts,outOf) rest

