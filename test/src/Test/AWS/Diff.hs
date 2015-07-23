{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Test.AWS.Diff
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Diff where

import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process
import           Text.Groom
import           Text.Printf

-- | Display the difference between two Haskell values,
-- with control over the diff parameters.
diff :: (Show a, Show b) => a -> b -> IO String
diff e a =
    withSystemTempFile "diff_expect" $ \ep eh ->
        withSystemTempFile "diff_actual" $ \ap ah -> do
            hPutStrLn eh (groom e) >> hClose eh
            hPutStrLn ah (groom a) >> hClose ah
            (_, !out, !err)
                <- readProcessWithExitCode "colordiff" ["-U", "3", ep, ap] []
            return (out ++ "\n" ++ err)
