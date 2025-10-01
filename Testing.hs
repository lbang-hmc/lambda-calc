module Testing where

    import qualified System.Process
    import qualified System.Exit as Exit
    import qualified Control.Exception
    import qualified System.IO
    import qualified System.Info
    import Data.List (intercalate)


    -- linewidth
    --   How many columns should our output be?
    --
    linewidth :: Int
    linewidth = 72

    -- spacing
    --   If we want two strings left and right-justified respectively,
    --     produce the string of spaces to insert in-between
    --   If str1 and str3 don't fit nicely on one line, the
    --     returned string will have a linebreak (to keep str1
    --     left justified) and enough spaces to make str3
    --     right-justified on the following line.
    --   uses the linewidth constant above.
    --
    spacing :: String -> String -> String

    spacing left right =
      let nspaces = linewidth - length left - length right
      in if nspaces >= 2 then
           take nspaces (repeat ' ')
         else
           "\n" ++ take (linewidth - length right) (repeat ' ')

    -- underline
    --   Add an underline to a given string
    --     [NB: Change from earlier version that just returned the
    --      underline itself, not the combination of string + underline]
    underline :: String -> String

    underline s = s ++ "\n" ++ (map (\_ -> '=') s) ++ "\n"

    pshow p =
        if (abs(p - fromIntegral (round p)) < 0.1) then show (round p) else show p


    successPoints p = (p, if p==0 then "[ok]" else "+" ++ (pshow p)
                                                          ++ " points")

    failurePoints p = (0, if p==0 then "[oops]" else "0/" ++ (pshow p) ++ " points")

    ---------------------
    -- Testing
    ---------------------

    data Testcase a = Is a
                    | Among [a]
                    | Satisfies String (a -> Bool)
                    | Crashes

    instance (Show a) => Show (Testcase a) where
      show (Is x) = show x
      show (Among xs) = intercalate " or " (map show xs)
      show (Satisfies desc _) = desc
      show Crashes = "[error]"


    type Test a = (String, Points, a, Testcase a)
    type TestM a = (String, Points, IO a, Testcase a)

    type Points = Double

    handler :: Control.Exception.SomeException -> IO String
    handler h = return $ "[error]" -- "[error"++ (show h) ++ "]"

    doTests :: (Show a) => String -> [Test a] -> IO (Points, Points)

    doTests section tests =
      do putStrLn $ underline section
         points <- mapM doTest tests
         putStrLn "\n"
         return (sum (map fst points), sum (map snd points))


    doTestsM' :: (Show a) => String -> IO [Test a] -> IO (Points, Points)

    doTestsM' section testsM =
      do putStrLn $ underline section
         tests <- testsM
         points <- mapM doTest tests
         putStrLn "\n"
         return (sum (map fst points), sum (map snd points))

    doTest :: (Show a) => Test a -> IO (Points, Points)

    doTest (name, points, calc, expected) =
      do putStr name
         let arrow = " --> "
         System.IO.hFlush System.IO.stdout
         answer <- Control.Exception.catch (let c = (show calc) in (length c) `seq` return c) handler
         let str1 = arrow ++ answer
         let result =
              case expected of
                Crashes -> answer == "[error]"
                _ -> answer /= "[error]" &&
                        case expected of
                          Is x     -> answer == show x
                          Among xs -> elem answer (map show xs)
                          Satisfies _ p -> p calc
         let (p, str3) = if result then
                           successPoints points
                         else
                           failurePoints points
         let str2 = spacing (name ++ str1) str3
         putStrLn $ str1 ++ str2 ++ str3
         System.IO.hFlush System.IO.stdout
         if (not result) then putStrLn("  expected  " ++ show expected ++ "\n\n") else return ()
         System.IO.hFlush System.IO.stdout
         return (p,points)

    doTestsM :: (Show a,Read a) => String -> [TestM a] -> IO (Points, Points)

    doTestsM section tests =
      do putStrLn $ underline section
         points <- mapM doTestM tests
         putStrLn "\n"
         return (sum (map fst points), sum (map snd points))

    doTestsMM :: (Show a, Read a) => String -> IO [TestM a] -> IO (Points, Points)

    doTestsMM section testsM =
      do putStrLn $ underline section
         tests <- testsM
         points <- mapM doTestM tests
         putStrLn "\n"
         return (sum (map fst points), sum (map snd points))

    doTestM :: (Show a,Read a) => TestM a -> IO (Points, Points)

    doTestM (name, points, calcM, expected) =
      do putStr name
         let arrow = " --> "
         System.IO.hFlush System.IO.stdout
         {-answer <- (let c = (show calc) in (length c) `seq` return c)-}
         answer <- Control.Exception.catch (do calc <- calcM
                                               let c = show calc
                                               return $ (length c) `seq` c) handler
         let str1 = arrow ++ answer
         let result = answer /= "[error]" &&
                      case expected of
                        Is x     -> answer == show x
                        Among xs -> elem answer (map show xs)
                        Satisfies _ p -> p (read answer)
         let (p, str3) = if result then
                           successPoints points
                         else
                           failurePoints points
         let str2 = spacing (name ++ str1) str3
         putStrLn $ str1 ++ str2 ++ str3
         System.IO.hFlush System.IO.stdout
         if (not result) then putStrLn("  expected  " ++ show expected ++ "\n\n") else return ()
         System.IO.hFlush System.IO.stdout
         return (p,points)


    testRunner :: String -> [IO (Points, Points)] -> [String] -> IO ()

    testRunner header allTests handGraded =
      do putStrLn (underline header)

         ps <- sequence allTests
         let achieved = sum (map fst ps)
         let outOf    = sum (map snd ps)


         if (length handGraded > 0) then
           do
             putStrLn("AUTOGRADED POINTS: " ++ pshow achieved ++ "/" ++ pshow outOf ++ "\n")
             putStrLn("\nHand-Graded")
             putStrLn("===========\n")

             mapM_ putStrLn handGraded

             putStrLn("\n\nTOTAL POINTS:\nGraded by:   <@hmc.edu>\n\n")
          else
           do
             putStrLn("\n\nTOTAL POINTS: " ++ pshow achieved ++ "/" ++ pshow outOf ++ "\n(Autograded)\n\n")

    -------------
    -- Utilities
    -------------

    runCommand :: String -> [String] -> IO (Int, String,String)

    runCommand cmd args =
       do -- putStrLn ( intercalate " " (cmd:args))
          System.IO.hFlush System.IO.stdout
          let os = System.Info.os
          let timeout = if (os == "darwin") then "gtimeout" else "timeout"
          (code,stdoutput,stderror) <- System.Process.readProcessWithExitCode
                                           timeout ("5s" : cmd : args) ""
          --putStrLn stderror
          case code of
            Exit.ExitSuccess -> return (0, stdoutput, stderror)
            Exit.ExitFailure n -> return (n, stdoutput, stderror)

{-
    doCompare str1 points comparisonCommand comparisonArgs exitCodeOk =
       do (code,stdoutput,stderror) <- System.Process.readProcessWithExitCode comparisonCommand comparisonArgs
          let (points, str3) = case code of
                                          Exit.ExitSuccess   -> successPoints points
                                          Exit.ExitFailure _ -> failurePoints points
                                          Exit.ExitFailure _ -> failurePoints points
          putStrLn (str1 ++ spacing str1 str3 ++ str3);
          return points

    main =
      do
        putStrLn (underline header)

        putStrLn "\n"

        (p6, _) <- doTests "State returned by \"doElem ...\"" doElemTests

        let h3 = "PostScript.hs"
        putStrLn (underline h3)

        lineTestResult <- lineTest()
        writeFile ("line.ps") $ unlines $ lineTestResult
        p1 <- doCompare "line"

        circleTestResult <- circleTest()
        writeFile ("circle.ps") $ unlines $ circleTestResult
        p2 <- doCompare "circle"

        boxTestResult <- boxTest()
        writeFile ("box.ps") $ unlines $ boxTestResult
        p3 <- doCompare "box"

        arrowTestResult <- arrowTest()
        writeFile ("arrow.ps") $ unlines $ arrowTestResult
        p4 <- doCompare "arrow"


        (p5, _) <- picPSTests

        let psPoints = p1 + p2 + p3 + p4 + p5

        putStrLn ("\n\nSTATE POINTS: " ++ show p6 ++ " / 44")
        putStrLn ("POSTSCRIPT POINTS: " ++ show psPoints ++ " / 42\n")
        putStrLn ("AUTOGRADED POINTS: " ++ show (p6+psPoints) ++ "\n")

        putStrLn ("STYLE POINTS:   / 14\n")

        putStrLn "\n=============\n"
        putStrLn "TOTAL POINTS:\n\n"

        putStrLn "Graded by:    <@hmc.edu>"

        return ()
        -}


    {-


    type Test a = (String, Points, a, a)
    type Points = Int


    piclineExamples = [
       (Lt, (1,1), [], Lt, (1-Main.linewid,1)),
       (Rt, (1,1), [], Rt, (1+Main.linewid,1)),
       (Up, (1,1), [], Up, (1,1+Main.lineht)),
       (Dn, (1,1), [], Dn, (1, 1-Main.lineht)),
       (Lt, (1,2), [Dir Rt,Dir Rt], Rt, (1+2*linewid,2)),
       (Rt, (1,3), [Dir Rt,Dir Rt], Rt, (1+2*linewid,3)),
       (Lt, (1,4), [Dir Rt, Dir Rt, Dir Lt], Lt, (1+linewid,4)),
       (Lt, (1,5), [Label "A", Dir Up, Label "B", Dir Rt, Label "C"], Rt, (1+linewid, 5+lineht)),
       (Dn, (1,6), [Dir Dn, Label "D"], Dn, (1, 6-lineht)),
       (Up, (1,7), [Label "E"], Up, (1, 7+lineht))
       ]



    piclinePS =
        [PostScript.headerCode, headerCode, "grid"] ++
        (concatMap (\ (dir1, p1, attrs, dir2, p2) -> snd (doElem (dir1,p1) (Draw Line attrs)))
                    piclineExamples) ++
        [PostScript.trailerCode]

    piclineStateTests =
       map (\(dir1, p1, attrs, dir2, p2) ->
          (show (dir1, p1) ++ " (Draw Line " ++ show attrs ++ ")",
            1,
            fst (doElem (dir1,p1) (Draw Line attrs)),
            (dir2, p2))) piclineExamples

    picmoveStateTests =
       map (\(dir1, p1, attrs, dir2, p2) ->
          (show (dir1, p1) ++ " (Draw Move " ++ show attrs ++ ")",
            1,
            fst (doElem (dir1,p1) (Draw Move attrs)),
            (dir2, p2))) piclineExamples

    picarrowStateTests =
       map (\(dir1, p1, attrs, dir2, p2) ->
          (show (dir1, p1) ++ " (Draw Arrow " ++ show attrs ++ ")",
            1,
            fst (doElem (dir1,p1) (Draw Arrow attrs)),
            (dir2, p2))) piclineExamples

    boxExamples = [
       (Lt, (1,1), [], Lt, (1-Main.boxwid,1)),
       (Rt, (1,2), [], Rt, (1+Main.boxwid,2)),
       (Up, (1,3), [], Up, (1,3+Main.boxht)),
       (Dn, (1,4), [], Dn, (1, 4-Main.boxht)),
       (Lt, (1,5), [Dir Rt], Lt, (1-boxwid,5)),
       (Lt, (1,6), [Label "A"], Lt, (1-boxwid, 6)),
       (Dn, (1,7), [Label "D"], Dn, (1, 7-lineht))
       ]

    picboxStateTests =
       map (\(dir1, p1, attrs, dir2, p2) ->
          (show (dir1, p1) ++ " (Draw Box " ++ show attrs ++ ")",
            1,
            fst (doElem (dir1,p1) (Draw Box attrs)),
            (dir2, p2))) boxExamples

    picboxPS =
        [PostScript.headerCode, headerCode, "grid"] ++
        (concatMap (\ (dir1, p1, attrs, dir2, p2) -> snd (doElem (dir1,p1) (Draw Box attrs)))
                    boxExamples) ++
        [PostScript.trailerCode]

    circleExamples = [
       (Lt, (1,1), [], Lt, (1-2*Main.circlerad,1)),
       (Rt, (1,2), [], Rt, (1+2*Main.circlerad,2)),
       (Up, (1,3), [], Up, (1,3+2*Main.circlerad)),
       (Dn, (1,4), [], Dn, (1, 4-2*Main.circlerad)),
       (Lt, (1,5), [Dir Rt], Lt, (1-2*circlerad,5)),
       (Lt, (1,6), [Label "A"], Lt, (1-2*circlerad, 6)),
       (Dn, (1,7), [Label "D"], Dn, (1, 7-2*circlerad))
       ]

    piccircleStateTests =
       map (\(dir1, p1, attrs, dir2, p2) ->
          (show (dir1, p1) ++ " (Draw Circle " ++ show attrs ++ ")",
            1,
            fst (doElem (dir1,p1) (Draw Circle attrs)),
            (dir2, p2))) circleExamples

    piccirclePS =
        [PostScript.headerCode, headerCode, "grid"] ++
        (concatMap (\ (dir1, p1, attrs, dir2, p2) -> snd (doElem (dir1,p1) (Draw Circle attrs)))
                    circleExamples) ++
        [PostScript.trailerCode]

    picPSTests =
       do writeFile ("picLine.ps") $ unlines $ piclinePS
          p1 <- doCompare "picLine"

          writeFile ("picCircle.ps") $ unlines $ piccirclePS
          p2 <- doCompare "picCircle"

          writeFile ("picBox.ps") $ unlines $ picboxPS
          p3 <- doCompare "picBox"

          return $ (p1+p2+p3, 10)


    doElemTests = piclineStateTests ++ picmoveStateTests ++ picarrowStateTests ++
                       picboxStateTests ++ piccircleStateTests

    doTests :: (Show a) => String -> [Test a] -> IO (Points, Points)

    doTests section tests =
      do putStrLn $ underline section
         points <- mapM doTest tests
         putStrLn "\n"
         return (sum (map fst points), sum (map snd points))


    handler2 :: Control.Exception.SomeException -> IO String
    handler2 _ = return "[error]"

    doTest :: (Show a) => Test a -> IO (Points, Points)

    doTest (name, points, calc, expected) =
      do putStr name
         let arrow = " --> "
         System.IO.hFlush System.IO.stdout
         answer <- Control.Exception.catch (let c = (show calc) in (length c) `seq` return c) handler2
         let str1 = arrow ++ answer
         let result = (answer == show expected)
         let p = if result then points else 0
         let str3 = if result then ("+" ++ show p ++ " points") else (show p ++ " points")
         let str2 = spacing (name ++ str1) str3
         putStrLn $ str1 ++ str2 ++ str3
         if (not result) then putStrLn("  expected  " ++
                                           intercalate " or " (map show expected)) else return ()
         return (p,points)


    -}


