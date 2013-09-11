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

import Data.Char
import Data.Strings

sPack :: (Strings a, Strings b) => a -> b
sPack = sFromString . sToString

sStripPrefix :: Strings a => a -> a -> a
sStripPrefix pre s
    | s `sStartsWith` pre = sDrop (sLen pre) s
    | otherwise           = s

sEnsurePrefix :: Strings a => a -> a -> a
sEnsurePrefix pre s
    | s `sStartsWith` pre = s
    | otherwise           = sConcat [pre, s]

sStripSuffix :: Strings a => a -> a -> a
sStripSuffix suf s
    | s `sEndsWith` suf = sTake (sLen s - sLen suf) s
    | otherwise         = s

sEnsureSuffix :: Strings a => a -> a -> a
sEnsureSuffix suf s
    | s `sEndsWith` suf = s
    | otherwise         = sConcat [s, suf]

sWrap :: Strings a => a -> a -> a
sWrap delim = sEnsureSuffix delim . sEnsurePrefix delim

sStripChar :: Strings a => Char -> a -> a
sStripChar c s
    | sHead s == c = c `sStripChar` sTail s
    | sLast s == c = c `sStripChar` sInit s
    | otherwise    = s

sStripLower :: Strings a => a -> a
sStripLower = sDropWhile isLower

sLowerFirst :: Strings a => a -> a
sLowerFirst s
    | isUpper $ sHead s = toLower (sHead s) `sCons` sTail s
    | otherwise         = s

sJoin :: Strings a => a -> [a] -> a
sJoin delim = sConcat . map (sEnsureSuffix delim . sStripPrefix delim)
