-- |
-- Module      : Network.AWS.Internal.String
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.String where

import Data.Char  (isUpper, toLower)
import Data.List
import Data.Maybe

dropPrefix :: String -> String -> String
dropPrefix pre s = fromMaybe s $ pre `stripPrefix` s

lowercase :: String -> String
lowercase = map toLower

suffix :: String -> String
suffix str = map rep $ drop idx str
  where
    idx = (+ 1) $ reverse (elemIndices '.' str) !! 1

    rep '.' = '/'
    rep  c  = c

underscore :: String -> String
underscore (x:xs) | isUpper x = toLower x : underscore xs
underscore xs                 = concatMap f xs
  where
    f x = ['_' | isUpper x] ++ [toLower x]
