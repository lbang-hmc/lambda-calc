{-
 -
 - lc: Yet Another Lambda Calculus Interpreter.
 -
 - Chris Stone
 - Original: September 4, 2017
 - Updated: June 1, 2018
 -
 -}

import Text.ParserCombinators.Parsec
import Control.Monad (when)
import qualified Control.Concurrent
import qualified Control.Exception
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List
--import qualified System.Console.Haskeline as HL
--import qualified System.Console.Readline as RL
import qualified System.IO
import qualified System.Posix.Signals

import Debug.Trace (trace)

import Core


{----------------}
{- MAIN PROGRAM -}
{----------------}

program :: String
program = "lc"

prompt :: String
prompt = program ++ "> "

welcome :: String
welcome = "Yet Another Lambda Calculus Interpreter, v1.0.1\n" ++
          "Enter :help for a list of configuration commands\n" ++
          "To quit say :exit or ^D"


help :: Ctx -> String
help ctx =
       "\nControl commands:\n" ++
       "  :set color <on|off>            colorize reductions     [currently " ++ onOff (showColor ctx) ++ "]\n" ++
       "  :set trace <full|compact|off>  output during reduction [currently " ++ traceWord (showTrace ctx) ++ "]\n" ++
    -- It's a secret!
    -- "  :set parens <on|off>           require all parentheses [currently " ++ onOff (allParens ctx) ++ "]\n" ++
       "  :load <filename>               load macros from specified file (or just :l)\n" ++
       "  :reload                        load macros from last file (or just :r)\n" ++
       "  :help                          show this list\n" ++
       (currentMacros ctx) ++ "\n"

  where
    onOff True  = "on"
    onOff False = "off"

    traceWord FullTrace    = "full"
    traceWord CompactTrace = "compact"
    traceWord NoTrace      = "off"


currentMacros :: Ctx -> String
 -- Get info on the currently-defined macros
currentMacros ctx = "Current macros : " ++ show (M.keys (environment ctx))

mkCtx :: Env -> Ctx
 -- Creates a default context
mkCtx env = Ctx { environment = env
                , shortcuts = False
                , expandUnappliedNumerals = False
                , showTrace = FullTrace
                , lambdaChar = 'λ'
                , allParens = True
                , showColor = True
                , lastFilename = Nothing
                }

main :: IO ()
main =
  do
     System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering
     {- Wreaks havoc with rlwrap
        System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
      -}

     {- Incantation to prevent two ^c's from killing the program -}
     tid <- Control.Concurrent.myThreadId
     System.Posix.Signals.installHandler System.Posix.Signals.keyboardSignal (System.Posix.Signals.Catch (Control.Exception.throwTo tid Control.Exception.UserInterrupt)) Nothing

     {- Display the welcome message -}
     putStrLn welcome

     {- The wrapper read-eval-print loop -}
     repl' (mkCtx M.empty)

repl' :: Ctx -> IO ()
 -- Main read-eval-print loop that calls the read-eval-print
 -- part inside an exception handler.
repl' ctx =
  do (continue, ctx') <- Control.Exception.handle f (repl ctx)
     System.IO.hFlush System.IO.stdout
     when continue $ repl' ctx'
  where f :: Control.Exception.AsyncException -> IO (Bool, Ctx)
        f _ = do putStrLn "<interrupt>"
                 return (True, ctx)


myGetLine :: IO String
-- A version of getLine that handles ^D reasonably
myGetLine = do
        end <- System.IO.isEOF
        if end then
          return "\EOT"
        else
          getLine


repl :: Ctx -> IO (Bool, Ctx)
 -- Really, this function is poorly named because it only does read-eval-print.
 -- The looping is handled elsewhere, in repl'
 --
 -- Returns updated context and a "should we continue looping" flag.
repl ctx =
    do
      putStr prompt
      System.IO.hFlush System.IO.stdout
      minput <- myGetLine
      let trim = unwords . words
      case (trim minput) of
          s
            | s `elem` [":exit","exit","quit","\EOT"] -> do putStrLn "goodbye"
                                                            return (False, ctx)
          "<interrupt>" ->
              do putStrLn "<interrupt>"
                 return (True, ctx)
          "" -> return (True, ctx)
          s@(':':rest) ->
              -- Input starts with a colon, so it should be a special command.
              case words rest of
                ["set", "parens", "on"]     -> return (True, ctx { allParens = True })
                ["set", "parens", "off"]    -> return (True, ctx { allParens = False })
                ["set", "color", "on"]      -> return (True, ctx { showColor = True })
                ["set", "color", "off"]     -> return (True, ctx { showColor = False })
                ["set", "trace", "on"]      -> return (True, ctx { showTrace = FullTrace })
                ["set", "trace", "full"]    -> return (True, ctx { showTrace = FullTrace })
                ["set", "trace", "compact"] -> return (True, ctx { showTrace = CompactTrace })
                ["set", "trace", "off"]     -> return (True, ctx { showTrace = NoTrace })

                (cmd : names) | cmd `Data.List.isPrefixOf` "dumpmacros" ->
                  let env = environment ctx
                      doName n = case M.lookup n env of
                                     Just e -> printDefn ctx n e
                                     Nothing -> putStr $ "# " ++ n ++ " not defined\n\n"
                  in do if null names then printEnv ctx (environment ctx)
                                      else mapM_ doName names
                        return (True, ctx)

                [cmd, fname] | cmd `Data.List.isPrefixOf` "load" ->
                  do env <- parseFile ctx fname
                     let ctx' = ctx { environment = env, lastFilename = Just fname }
                     putStrLn (currentMacros ctx')
                     return (True, ctx')

                [cmd] | cmd `Data.List.isPrefixOf` "reload" ->
                  case (lastFilename ctx) of
                     Nothing -> do putStrLn "ERROR: No file loaded, so nothing to reload\n\n"
                                   return (True, ctx)
                     Just fname -> do env <- parseFile ctx fname
                                      let ctx' = ctx { environment = env }
                                      putStrLn (currentMacros ctx')
                                      return (True, ctx')

                [cmd] | cmd `Data.List.isPrefixOf` "help" || cmd == "?" ->
                  do putStrLn (help ctx)
                     return (True, ctx)
                     
                (cmd : macros@(_:_)) | cmd `Data.List.isPrefixOf` "undef" ->
                  let env' = foldr M.delete (environment ctx) macros
                      ctx' = ctx { environment = env' }
                  in  return (True, ctx')

                _ ->
                  do putStrLn $ "unrecognized command '" ++ s ++ "'"
                     return (True, ctx)
          line ->
              do {- putStrLn ""
                    putStrLn line -}
                 let parser = (defnOrExpr ctx) <* eof
                 case  parse parser "" line  of
                   Left err           -> do  print err
                                             return (True, ctx)
                   Right (Right e)    -> do  putStrLn (show' ctx e)
                                             run ctx e
                                             return (True, ctx)
                   Right (Left (n,e)) ->
                     let env' = M.insert n e (environment ctx)
                         ctx' = ctx { environment = env', lastFilename = Nothing }
                     in  case envOK env' of
                           Nothing -> return (True, ctx')
                           Just errMsg ->
                              do  putStrLn errMsg
                                  putStrLn "ERROR: Macro rejected."
                                  return (True, ctx)
