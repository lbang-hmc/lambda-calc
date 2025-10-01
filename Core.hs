{-
 -
 - lc: Yet Another Lambda Calculus Interpreter.
 -
 - Chris Stone
 - Original: September 4, 2017
 - Updated: June 1, 2018
 -
 -}

module Core where

  import Text.ParserCombinators.Parsec
  import qualified Control.Monad
  import qualified Control.Exception
  import Data.Char (chr)
  import qualified Data.Map.Strict as M
  import qualified Data.Set as S
  import qualified Data.List
  import qualified System.IO

  import Debug.Trace (trace)


  --
  -- Lambda-calculus syntax
  --

  type Identifier = String    -- lowercase alphanumeric
  type MacroName = String     -- uppercase alphanumeric

  type AnsiCodes = (String, String) -- Codes to enable / disable a given ANSI display feature

  data Expr = Var   Identifier
            | Lam   Identifier Expr
            | App   Expr Expr
            | Num   Integer
            | Macro MacroName
            | Redex AnsiCodes Expr  -- Mark a subexpression to be colorized
            | Parens Expr           -- parsing hack

  --
  -- The interpreter configuration (called "context", though arguably it's really a "state")
  --

  type Env = M.Map MacroName Expr

  data Trace = FullTrace | CompactTrace | NoTrace
      deriving (Show)

  data Ctx = Ctx { shortcuts :: Bool
                 , expandUnappliedNumerals :: Bool
                 , environment :: Env
                 , showTrace :: Trace
                 , lambdaChar :: Char
                 , allParens :: Bool
                 , showColor :: Bool
                 , lastFilename :: Maybe String
                 }
      deriving (Show)

  type Memo = M.Map Expr (Integer, Expr)


  data Reason = ByDefinition
              | Betas  Integer

  type Step = (Expr, Reason, Expr)

  --
  -- Pretty Printing
  --

  {- The default show (i.e., convert-to-string) function 
     inserts minimal parentheses, and uses greek lambdas -}

  instance Show Expr where
    show = sh "" False False
      where sh :: String -> Bool -> Bool -> Expr -> String
             -- Convert an expression to a string.
             -- The given flags tell us whether this expression appears
             -- immediately to the left or immediately to the right
             -- of an application
            sh reset onLeft onRight e =
              case e of
                Var v   -> v
                Num n   -> show n
                Macro m -> m

                Lam x e1 ->
                  -- Why "False False" in recursion? If we have \x.e applied to e',
                  -- then we are forced parenthesize the lambda to (\x.e),
                  -- and so e itself is no longer adjacent to e'
                  -- and it doesn't need further parentheses. And then since \x.e isn't
                  -- immediately to the left of the application, neither is e.
                  parens (onLeft || onRight) ("λ" ++ x ++ "." ++ sh reset False False e1)

                App e1 e2 ->
                  parens
                     onRight                 -- app. is left-associative,
                                             -- so we only need parens as an argument
                     (
                      sh reset True False e1 ++    -- e1 is to the left of an application.
                                             -- if e1 e2 was on the right of an
                                             -- outer application, we've added
                                             -- parens
                      " " ++
                      sh reset (onLeft && not onRight) True e2  -- e2 to left of an app only
                                                          -- if e1 e2 was,
                                                          -- and we didn't add parens.
                                                          -- But it's definitely to the
                                                          -- right of this app.
                     )

                Redex (ansiIn, ansiOut) e1 ->
                    ansiIn ++ sh (reset ++ ansiIn) onLeft onRight e1 ++ ansiOut ++ reset
                Parens e1 -> sh reset onLeft onRight e1

            parens :: Bool -> String -> String
            -- optionally parenthesize a string
            parens True  s = "(" ++ s ++ ")"
            parens False s = s


  strictShow :: Char -> Bool -> Expr -> String
  -- Display the expression with full parenthesization.
  --   Configurable with the character to use for lambdas
  --   and whether or not to show colored subexpressions.
  strictShow lamChar color =
   let recur reset e =
          case e of
            Var v   -> v
            Num n   -> show n
            Macro m -> m

            Lam x e1 ->
              '(' : lamChar : x ++ "." ++ recur reset e1 ++ ")"

            App e1 e2 ->
              "(" ++ recur reset e1 ++ " " ++ recur reset e2 ++ ")"

            Redex (ansiIn, ansiOut) e1 ->
              if color then
                ansiIn ++ recur (reset ++ ansiIn) e1 ++ ansiOut ++ reset
              else
                recur reset e1

            Parens e1 -> recur reset e1
   in
     recur ""



  literalShow :: Expr -> String
   -- Only used for errors in strict parsing
  literalShow e =
    case e of
      Var v   -> v
      Num n   -> show n
      Macro m -> m
      Lam x e1 -> '\\' : x ++ "." ++ literalShow e1
      App e1 e2 -> literalShow e1 ++ " " ++ literalShow e2
      Redex _ _ -> error "literalShow found a Redex"
      Parens e1 -> "(" ++  literalShow  e1 ++ ")"


  show' :: Ctx -> Expr -> String
   -- Converts the expression to string, configured by the context
  show' ctx e =
    if (allParens ctx) then
      strictShow (lambdaChar ctx) (showColor ctx) e
    else
      show e

  printDefn :: Ctx -> MacroName -> Expr -> IO ()
  printDefn ctx name e = do putStr name
                            putStr " = "
                            putStr $ show' ctx e
                            putStr ";\n\n"

  printEnv :: Ctx -> Env -> IO ()
  printEnv ctx env = mapM_ (uncurry $ printDefn ctx) (M.toList env)

  instance Show Reason where
    show ByDefinition = "== (by definition)"
    show (Betas 1) = "→ᵦ"
    show (Betas n) | n > 1 = "→ᵦ ... →ᵦ  (by " ++ show n ++ " steps already seen above)"
    show (Betas _) = "  ??? a miracle occurred ???"

  {-------------------
   - CHURCH NUMERALS -
   -------------------}

  church :: Integer -> Expr
   -- Return the Church-numeral equivalent for a given nonnegative integer.
  church n =
     Lam "f" (Lam "b" (foldr App (Var "b") (replicate (fromIntegral n) (Var "f"))))


  {-
  isChurch :: Expr -> Maybe Integer
   -- Is this expression alpha-equivalent to a church numeral?
   --   As in lci, there might be false positives (e.g., False shows up as 0)
  isChurch (Lam f (Lam b body)) =
    let loop (Var b')          | b == b'                    = Just 0
        loop (App (Var f') e') | f == f', Just n <- loop e' = Just (n+1)
        loop _                                              = Nothing
    in
        loop body
  isChurch e = Nothing
  -}

  -- Map from variable name to integers, used to implement alpha-equality
  type Numbering = M.Map Identifier Integer

  instance Eq Expr where
    expr1 == expr2 = alphaEq M.empty M.empty expr1 expr2
      where
        alphaEq ::  Numbering -> Numbering -> Expr -> Expr -> Bool
         -- Compare the given two expressions for alpha-equality
         --  The mappings send bound variables (only) on the left/right
         --     to their nesting level.
         -- NB: numbers are *not* considered equal to the
         --      corresponding Church numeral
        alphaEq mapL mapR left right =
          let
            recurse = alphaEq mapL mapR
          in
            case (left, right) of
              (Num n, Num m)     -> n == m

              (Macro x, Macro y) -> x == y

              (Var x, Var y) ->
                  case (M.lookup x mapL, M.lookup y mapR) of
                    (Just i, Just j) -> i == j   -- corresponding bound vars
                    (Nothing, Nothing) -> x == y -- corresponding free vars
                    _ -> False

              (App e1 e2, App e3 e4) ->
                 recurse e1 e3 && recurse e2 e4

              (Lam x e1, Lam y e2) ->
                let
                  n = fromIntegral $ M.size mapL -- same as M.size mapR
                in
                  alphaEq (M.insert x (n+1) mapL)
                          (M.insert y (n+1) mapR)
                          e1 e2

              _ -> False


  instance Ord Expr where
    compare expr1 expr2 = alphaCmp M.empty M.empty expr1 expr2
      where
        alphaCmp :: Numbering -> Numbering -> Expr -> Expr -> Ordering
        alphaCmp mapL mapR left right =
          let recurse = alphaCmp mapL mapR
          in
            case (left, right) of
              (Num n, Num m) ->  compare n m
              (Num _, _)     ->  LT

              (Macro x, Macro y) -> compare x y
              (Macro _, _)       -> LT

              (Var x, Var y) ->
                  case (M.lookup x mapL, M.lookup y mapR) of
                    (Just i, Just j) -> compare i j
                    (Nothing, Nothing) -> compare x y
                    (Just _, _) -> LT   -- bound vars are "less" than free vars
                    (_, Just _) -> GT
              (Var _, _) -> LT

              (App e1 e2, App e3 e4) ->
                 case recurse e1 e3 of
                    EQ  -> recurse e2 e4
                    ans -> ans
              (App _ _, _) -> LT

              (Lam x e1, Lam y e2) ->
                let
                  n = fromIntegral $ M.size mapL -- same as M.size mapR
                in
                  alphaCmp (M.insert x (n+1) mapL)
                           (M.insert y (n+1) mapR)
                           e1 e2
              (Lam _ _, _) -> LT

              (Parens e1, Parens e2) -> recurse e1 e2
              (Parens _, _) -> LT

              (Redex col1 e1, Redex col2 e2) ->
                 case compare col1 col2 of
                   EQ -> recurse e1 e2
                   ans -> ans

              (Redex _ _, _) -> LT


  {-
  size :: Expr -> Integer
  -- ^size measure for the given expression
  size (Var _)      = 1
  size (Lam _ e)    = 1 + size e
  size (App e1 e2)  = size e1 + size e2 + 1
  size (Num _)      = 1
  size (Macro _)    = 1
  size (Parens e)   = size e
  size (Redex _ e)  = size e
  -}

  --
  -- Free Variables
  --

  fv :: Expr -> S.Set Identifier
  -- ^ the set of free variables in the given expression
  fv e =
    case e of
      Num _      -> S.empty
      Macro _    -> S.empty

      Var v      -> S.singleton v

      App e1 e2  -> S.union (fv e1) (fv e2)

      Lam x e1   -> S.difference (fv e1) (S.singleton x)
      Redex _ e1 -> fv e1
      Parens e1  -> fv e1


  --
  -- Circularity Check
  --

  envOK :: Env -> Maybe String
  envOK env = Control.Monad.msum errors  -- Returns the first Just in the list,
                                         -- or Nothing if there were no errors found
     where

      errors = map (\(k,v) -> traverse (S.singleton k) (S.elems (macros v))) (M.assocs env)

      traverse :: S.Set MacroName -> [MacroName] -> Maybe String
      traverse _ [] = Nothing   -- Everything checked out fine
      traverse seen (m:rest) =
        if S.member m seen then
          Just ("Illegal circular definition: " ++ m ++ " expands into code containing " ++ m)
        else
          case M.lookup m env of
            Nothing -> Just ("Macro " ++ m ++ " is used but not defined anywhere")
            Just defn ->
                let seen' = S.insert m seen
                -- Rewrite to use <|> here? 
                in  case traverse seen' (S.elems (macros defn)) of
                      Just errMsg -> Just errMsg
                      Nothing -> traverse seen rest


      macros :: Expr -> S.Set Identifier
      -- ^ the set of macros used in the given expression
      macros e =
        case e of
          Num _      -> S.empty
          Macro m    -> S.singleton m
          Var v      -> S.empty
    
          App e1 e2  -> S.union (macros e1) (macros e2)
    
          Lam _ e1   -> macros e1  -- ignore bound variables
          Redex _ e1 -> macros e1
          Parens e1  -> macros e1

  {-
   - PARSING
   -}


  comment :: Parser ()
  -- A line comment (from '#' to end-of-line)
  comment =
     do _ <- char '#'
        many (noneOf "\n")
        _ <- char '\n'
        return ()

  whitespace :: Parser ()
  -- ^ Optional whitespace
  whitespace =
     do try (many ((do _ <- space; return ()) <|> comment) <?> "whitespace")
        return ()


  ident :: Parser String
  -- ^ identifiers (lambda vars) start with a lowercase letter
  --   and continue with 0 or more lowercase letters, digits, or
  --   primes (single quotes)
  ident =
     (do c <- (lower <?> "variable")
         cs <- many (lower <|> upper <|> digit <|> oneOf "\'")
         whitespace
         return $ c : cs) <?> "variable"

  ch :: Char -> Parser ()
  -- ^ Confirm the presnce of a given character, possibly followed by whitespace
  ch c =
     do _ <- char c
        whitespace

  number :: Parser Integer
  -- Nonnegative integer
  number =
     do s <- (many1 digit) <?> "a number"
        whitespace
        return $ read s

  num :: Parser Expr
  num =
     (do n <- number
         return $ Num n) <?> "a number"

  lam :: Parser Expr
  lam =
     do ((ch '\\' <|> ch 'λ') <?> "a lambda")
        id <- ident
        _ <- (oneOf ".,:" <?> "a separator")
        whitespace
        body <- expr
        return (Lam id body)

  var :: Parser Expr
  var =
     do c <- ident
        return $ Var c

  macroId :: Parser String
  macroId =
     (do c <- upper
         cs <- many alphaNum
         whitespace
         return $ c:cs) <?> "macro name"

  macro :: Parser Expr
  macro =
     do m <- macroId
        return $ Macro m

  atomic :: Parser Expr
  atomic =
     lam <|> group <|> num <|> macro <|> var

  expr :: Parser Expr
  expr =
     lam <|> path

  {-
  expr_p :: Parser Expr
   -- Requires proper parenthesization
  expr_p =
    ( var <|> num <|> macro <|>
       do ch '('
          (do _ <- (oneOf "\\λ" <?> "a lambda")
              whitespace
              v <- ident
              _ <- (oneOf ".,:" <?> "a separator")
              whitespace
              body <- (expr_p <?> "a function body")
              ch ')'
              return (Lam v body)) <|>
           (do e1 <- (expr_p <?> "an expression")
               e2 <- (expr_p <?> "an (argument) expression")
               ch ')'
               return (App e1 e2))
           ) <?> "an expression"
  -}

  group :: Parser Expr
   -- Allow () or [] or {} grouping, but they must match.
  group =
    (do ch '('
        result <- expr
        ch ')' <?> ")"
        return $ Parens result
      <|>
     do ch '['
        result <- expr
        ch ']' <?> "]"
        return $ Parens result
      <|>
     do ch '{'
        result <- expr
        ch '}' <?> "}"
        return $ Parens result) <?> "a parenthesized expression"



  path :: Parser Expr
   -- a path is a *non-lambda* that is applied to
   --   zero or arguments (in succession).
  path =
     do hd <- var <|> group <|> num <|> macro   -- the non-lambda
        rest <- many atomic                     -- the arguments
          -- By the implicit parenthesis conventions,
          -- only the last argument in a path can be a
          -- *non-parenthesized* lambda abstraction. So we
          -- we check for that case specially.
        (do f <- lam; return $ foldl App hd (rest ++ [f])) <|> return (foldl App hd rest)

  lciFile :: Ctx -> Parser [(MacroName, Expr)]
  lciFile ctx =
     do whitespace
        result <- sepEndBy (line ctx) (ch ';')
        eof
        return result

  line :: Ctx -> Parser (MacroName, Expr)
  line ctx =
     do mac <- try $ const <$> macroId <*> ch '='
        {-body <- if (allParens ctx) then expr_p else expr-}
        body <- exprInCtx ctx
        return (mac,  body)


  exprInCtx :: Ctx -> Parser Expr
  exprInCtx ctx =
    if (allParens ctx) then
      do e <- expr
         case checkParens e of
           Left err -> fail err
           Right e' -> return e'
    else
      do e <- expr
         return $ stripParens e

  defnOrExpr :: Ctx -> Parser (Either (MacroName, Expr) Expr)
  defnOrExpr ctx =     (fmap Left $ const <$> line ctx <*> optional (ch ';'))
                   <|> (fmap Right $ exprInCtx ctx)

  parseFile :: Ctx -> String -> IO Env
  parseFile ctx filename =
     do result <- Control.Exception.try
                   (do result <- parseFromFile (lciFile ctx) filename
                       case result of
                            Left err -> do putStrLn (show err)
                                           putStrLn "ERROR: No macros loaded."
                                           return M.empty
                            Right macros -> return (M.fromList macros))
        case result of
          Left err -> do  putStrLn (showErr err)
                          putStrLn "ERROR: No macros loaded."
                          return M.empty
          Right env -> case envOK env of
                          Nothing -> return env
                          Just errMsg ->
                              do  putStrLn errMsg
                                  putStrLn "ERROR: No macros loaded."
                                  return M.empty

     where
      showErr :: IOError -> String
      showErr x = show x

  stripParens :: Expr -> Expr
  stripParens e =
    case e of
      Var _ -> e
      Num _ -> e
      Macro _ -> e
      Lam x e1 -> Lam x (stripParens e1)
      App e1 e2 -> App (stripParens e1) (stripParens e2)
      Parens e1 -> stripParens e1
      Redex col e1 -> Redex col (stripParens e1)

  checkParens :: Expr -> Either String Expr
  checkParens e =
    case e of
      Var _ -> Right e
      Num _ -> Right e
      Macro _ -> Right e

      Lam _ e1 ->
        case checkParens e1 of
           Left err -> Left err
           Right _  -> Left $ "Unparenthesized lambda: " ++ literalShow e

      App e1 e2 ->
        case checkParens e2 of
           Left err -> Left err
           Right _ ->
             case checkParens e1 of
               Left err -> Left err
               Right _  -> Left $ "Unparenthesized application: " ++ literalShow e

      Parens (Var _) -> Left $ "Lonely variable in parentheses: " ++ literalShow e
      Parens (Num _) -> Left $ "Lonely number in parentheses: " ++ literalShow e
      Parens (Macro _) -> Left $ "Lonely constant in parentheses: " ++ literalShow e

      Parens (Lam x e1) ->
        case checkParens e1 of
          Left err -> Left err
          Right e1' -> Right $ Lam x e1'

      Parens (App e1 e2) ->
        case checkParens e2 of
          Left err -> Left err
          Right e2' ->
            case checkParens e1 of
              Left err -> Left err
              Right e1' -> Right $ App e1' e2'

      Parens (Parens _) ->
        Left $ "Unnecessarily nested parentheses: " ++ literalShow e


      Redex _ _ -> error "checkParens found a Redex"
      Parens (Redex _ _) -> error "checkParens found a Redex in parentheses"


  run :: Ctx -> Expr -> IO ()
   -- Evaluate the given expression, using the context to control output
  run ctx e =
    do let (_, n, steps, whn) = cbnLazy ctx M.empty e (fv e)
       case (showTrace ctx, n) of
         (NoTrace, _)      -> do {- putStrLn $ "\n" ++ show' ctx e -}
                                 putStrLn ""
         (_, 0)            -> putStrLn "\nAlready in normal form!\n" 
         (FullTrace, _)    -> printSteps ctx 0 steps
         (CompactTrace, _) -> printSteps' ctx steps
       putStrLn $ show' ctx whn
       putStrLn $ "\n" ++ (show n) ++ " total steps" -- ++ " (" ++ show (length steps) ++ " shown)\n"


  shortIndent :: String
  shortIndent = "  "

  indent :: String
  indent = shortIndent ++ shortIndent

  printStep :: Ctx -> Step -> IO ()
  printStep ctx (e1,r,e2)  =
       do putStrLn $ indent ++ show' ctx e1
          putStrLn $ shortIndent ++ show r
          putStrLn $ indent ++ show' ctx e2
          putStrLn ""

  printSteps :: Ctx -> Integer -> [Step] -> IO ()
  printSteps _   _ [] = return ()
  printSteps ctx n ((e1,r,e2):steps) =
    do putStrLn ("\nAfter " ++ show n ++ " β-reductions:\n")
       printStep ctx (e1,r,e2)
       case r of
         Betas m -> printSteps ctx (n+m) steps
         ByDefinition -> printSteps ctx n steps


  printSteps' :: Ctx -> [Step] -> IO ()
  -- Compact trace
  printSteps' _   [] = return ()
  printSteps' ctx [lastStep] = printStep ctx lastStep
  printSteps' ctx ((e1,r,_):rest) =
    do putStrLn $ indent ++ show' ctx e1
       putStrLn $ shortIndent ++ show r
       printSteps' ctx rest





  {----------------
   - Substitution -
   ----------------}

  fresh :: Identifier -> S.Set Identifier -> Identifier
  fresh x avoid =
    if S.member x avoid then
       fresh (x ++ "'") avoid
    else
       x

  substV :: Expr -> Identifier -> Expr -> S.Set Identifier -> Expr
  substV e x e' avoid =
    case e of
      Var y ->  if x==y then e' else e
      App e1 e2 -> App (substV e1 x e' avoid) (substV e2 x e' avoid)
      Lam y e1
        | x == y -> e
        | S.member y avoid ->
            let y' = fresh y avoid
                avoid' = S.insert y avoid
                e1' = substV e1 y (Var y') avoid'
            in
                Lam y' (substV e1' x e' avoid')
        | otherwise -> Lam y (substV e1 x e' avoid)
      Num _ -> e
      Macro _ -> e
      Redex col e1 -> Redex col (substV e1 x e' avoid)
      Parens e1 -> Parens (substV e1 x e' avoid)




  {-
   - Pure cbn + lazy memoization
   -}


  memoize :: Expr -> (Integer, Expr) -> Memo -> Memo
   -- Save the weak-head-normalization and the number of steps,
   --   unless it's a 0 or 1 step reduction.
   -- (If you can get from A to B by definition or in 1 beta-step,
   --   it's more confusing to say "we get from A to B by 0/1 steps we saw before"
   --   than to redo the variable expansion or redo the single beta step.
  memoize _ (0, _) memo = memo
  memoize _ (1, _) memo = memo
  memoize e p memo = M.insert e p memo

  cbnLazy :: Ctx -> Memo -> Expr -> S.Set Identifier -> (Memo, Integer, [Step], Expr)
  cbnLazy ctx memo e avoid =
     let (memo', n, steps, whn) = whnfLazy ctx memo e avoid in
     case whn of
        Lam x e' ->
          let (memo'', n', steps', whn') =
                cbnLazy ctx memo' e' (S.insert x avoid)
          in
              (memo'',
               n+n',
               steps ++ map (\ (w,r,z) -> (Lam x w, r, Lam x z)) steps',
               Lam x whn')

        App e2 e3 ->
           let (memo2, nsteps2, steps2, whn2) = cbnLazy ctx memo' e2 avoid
               (memo3, nsteps3, steps3, whn3) = cbnLazy ctx memo2 e3 avoid
               ntot = n + nsteps2 + nsteps3
           in
               (memo3, ntot,
                steps ++
                   map (\ (w,r,z) -> (App w e2  , r, App z e2  )) steps2 ++
                   map (\ (w,r,z) -> (App whn2 w, r, App whn2 z)) steps3 ,
                App whn2 whn3)

        _ -> (memo', n, steps,  whn)


  whnfLazy :: Ctx -> Memo -> Expr -> S.Set Identifier -> (Memo, Integer, [(Expr, Reason, Expr)], Expr)
  -- Reduce the given expression to weak-head-normal form. (a variable, number,
  --    lambda, or a "path"  [where a path is a variable applied to 1 or more arguments in
  --    succession, where the arguments might not be normal]
  -- The context controls printing (and someday, perhaps, evaluation shortcuts)
  -- The memo pad is used to simulate lazy evaluation to whnf.
  -- The set of identifiers contains all the free variables of the expression (and
  --    maybe more), and is used in capture-avoiding substitution. It's not
  --    obvious thatthis is any more efficient in practice than re-computing free
  --    variables from scratch every time we do a substitution, but it's how things are
  --    currently implemented.
  --
  -- The return value is the updated memo pad; the number of beta (only) steps performed,
  --   the actual steps (which might include shortcuts), and [for efficiency] the
  --   final whnf resulting from the final step.
  whnfLazy ctx =
    let

      -- arbitrarily chosen colors
      ansiBefore = ansiColorU 36  -- color for subterm to be reduced
      ansiAfter = ansiColorU 32   -- color for the replacement subterm
      ansiArg = ansiColor 35      -- special color for arguments in single beta-reductions

      -- main whnf reduction loop, just so we don't have to keep passing the
      -- context around explicitly.
      recur :: Memo -> Expr -> S.Set Identifier -> (Memo, Integer, [(Expr, Reason, Expr)], Expr)
      {-recur _ e _ | trace ("recur " ++ show e) False = undefined-}
      recur memo e avoid =
         case M.lookup e memo of
           Just (n, whn) ->
             -- Hey; I recognize this input!  And I remember what its whnf was...
             (memo, n, [(Redex ansiBefore e, Betas n, Redex ansiAfter whn)], whn)

           Nothing ->
            -- Too bad, it's new to me.
            case e of

              Num n ->
                if (expandUnappliedNumerals ctx) then
                  -- Reduce the decimal numeral to a church numeral
                  let f = church n
                  in (memo, 0, [(Redex ansiBefore e, ByDefinition, Redex ansiAfter f)], f)
                else
                  -- Leave it as a more-readable decimal numeral until we see it
                  -- being applied to something
                  (memo, 0, [], e)

              Lam _ _ ->
                -- Lambda abstractions are weak-head normal
                (memo, 0, [], e)

              Var _ ->
                -- Variables are weak-head normal
                (memo, 0, [], e)

              Macro m ->
                 case M.lookup m (environment ctx) of
                   Just defn ->
                      -- Most macros we write are already in whnf, but normalize
                      -- just in case. Note that the free variables of the macro
                      -- [which should be empty but we don't check] probably
                      -- aren't reflected in the avoid set [which contains
                      -- the free variables of e]
                      let (memo', n, steps, whn) = recur memo defn (fv defn)
                            -- We don't want to memoize the mere expansion of
                            --   a definition. But we call memoize anyway, just
                            --   in case it's a particularly weird macro that
                            --   does take many steps to reduce.
                          memo'' = memoize e (n,whn) memo'
                      in
                          (memo'', n, (Redex ansiBefore e, ByDefinition, Redex ansiAfter defn) : steps, whn)
                   Nothing   ->
                      -- Undefined macros are weak-head normal, like a free
                      -- variable. Though arguably they should be an error...
                      (memo, 0, [], e)

              App e1 e2  ->
                case recur memo e1 avoid of
                  (memo', nsteps1, steps1, Lam x e1') ->
                    let e3 = substV e1' x e2 (S.insert x avoid)
                        upThroughBetaSteps =
                              map (\ (w,r,z) -> (App w e2, r, App z e2)) steps1  ++
                                 [ (App (Redex ansiBefore (Lam x e1')) (Redex ansiArg e2),
                                    Betas 1,
                                    Redex ansiAfter (substV e1' x (Redex ansiArg e2) (S.insert x avoid))) ]
                    in if e == e3 then
                         let ans = Macro "...infinite loop..."
                         in
                           (memo',
                            nsteps1 + 1,
                            upThroughBetaSteps ++ [ (e3, Betas 1, ans) ],
                            ans)
                       else
                         let
                           (memo'', nsteps2, steps2, whn) = recur memo' e3 avoid
                           nsteps = nsteps1 + nsteps2 + 1
                         in
                          (memoize e (nsteps, whn) memo'',
                           nsteps,
                           upThroughBetaSteps ++ steps2,
                           whn)


                  (memo', nsteps1, steps1, Num n) ->
                     -- can only happen if expandUnappliedNumerals is False
                     let f = (church n)
                         (memo'', nsteps2, steps2, whn) = recur memo' (App f e2) avoid
                         nsteps = nsteps1 + nsteps2
                     in (memoize e (nsteps, whn) memo'',
                         nsteps,
                         ( map (\ (w,r,z) -> (App w e2, r, App z e2)) steps1 ) ++
                           [ (App (Redex ansiBefore (Num n)) e2,
                              ByDefinition ,
                              App (Redex ansiAfter f) e2)] ++
                           steps2,
                         whn)

                  (memo', n1, steps1, e1') ->
                     (memo', n1,
                        map ( \ (w,r,z) -> (App w e2, r, App z e2)) steps1,
                        App e1' e2)


              _ -> (memo, 0, [], Macro ("oops"))

    in
       recur

  {- Color Coding -}

  
  {-
  ansiBrightColor :: Integer -> AnsiCodes
  ansiBrightColor n =
    (chr 27 : '[' : (show n) ++ ";1m", chr 27 : "[39m")


  ansiReset :: String
  ansiReset =
    chr 27 : "[0m"

  ansiBrightColorU :: Integer -> AnsiCodes
  ansiBrightColorU n =
   let (uon, uoff) = ansiUnderline
       (con, coff) = ansiBrightColor n
   in (uon ++ con, uoff ++ coff)

  ansiBold :: AnsiCodes
  ansiBold =
    (chr 27 : "[1m", chr 27 : "[22m")

  ansiBackground :: Integer -> AnsiCodes
  ansiBackground n =
    (chr 27 : '[' : (show n) ++ "m", chr 27 : "[49m")

  ansiInverse :: AnsiCodes
  ansiInverse =
    (chr 27 : "[7m", chr 27 : "[27m")

  -}

  ansiColor :: Integer -> AnsiCodes
  ansiColor n =
    (chr 27 : '[' : (show n) ++ "m", chr 27 : "[39m")

  ansiUnderline :: AnsiCodes
  ansiUnderline =
    (chr 27 : "[4m", chr 27 : "[24m")

  ansiColorU :: Integer -> AnsiCodes
  ansiColorU n =
   let (uon, uoff) = ansiUnderline
       (con, coff) = ansiColor n
   in (uon ++ con, uoff ++ coff)

