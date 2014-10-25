{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.V2.Transform
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Transform where

import           Control.Applicative  ((<$>))
import           Control.Error
import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import           Data.Function        (on)
import           Data.Jason           (eitherDecode')
import           Data.Jason.Types     hiding (object)
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Gen.V2.Log
import           Gen.V2.Stage1        (Stage1)
import           Gen.V2.Stage2        (Stage2(..))
import           Gen.V2.Types
import           System.Directory
import           System.FilePath

transformS1ToS2 :: Stage1 -> Either String Stage2
transformS1ToS2 = const (Right Stage2)

trimS2 :: Stage2 -> Stage2
trimS2 = id
