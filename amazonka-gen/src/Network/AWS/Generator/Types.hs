-- Module      : Network.AWS.Generator.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Generator.Types where

import Data.List
import System.FilePath

data Service = Service
    { svcPath :: FilePath
    , svcVers :: String
    } deriving (Show)

serviceFromPath :: FilePath -> String -> Service
serviceFromPath d f = Service (d </> f) (fst $ break (== '.') f)
