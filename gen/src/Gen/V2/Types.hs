-- Module      : Gen.V2.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Types where

import           Control.Applicative  ((<$>))
import           Control.Error
import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import           Data.Function        (on)
import qualified Data.HashMap.Strict  as Map
import           Data.Jason           (eitherDecode')
import           Data.Jason.Types     hiding (object)
import           Data.List
import           Data.List            (unionBy)
import           Data.Monoid
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           System.Directory
import           System.FilePath

data Model = Model
    { _mName    :: String
    , _mVersion :: String
    , _mModel   :: Object
    } deriving (Show, Eq)

instance Ord Model where
    compare a b = comparing _mName a b <> comparing _mVersion a b

data Path
    = Const Text
    | Var   Text
      deriving (Eq, Show)

data URI = URI [Path] [(Text, Maybe Text)]
    deriving (Eq, Show)
