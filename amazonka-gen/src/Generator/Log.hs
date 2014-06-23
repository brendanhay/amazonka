{-# LANGUAGE OverloadedStrings #-}

-- Module      : Generator.Log
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Log
    ( say
    ) where

import           Data.Monoid
import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

say :: Text -> String -> IO ()
say lbl msg = Text.putStrLn $ title lbl <> Text.pack msg

title :: Text -> Text
title x = "[ " <> y <> "] "
  where
    y | n > 0     = x <> Text.replicate n " "
      | otherwise = x

    n = 17 - Text.length x

