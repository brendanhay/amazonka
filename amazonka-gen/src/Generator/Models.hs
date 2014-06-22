-- Module      : Generator.Models
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Models where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson
import qualified Data.Foldable              as Fold
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           Data.String.CaseConversion
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as LText
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding    as LText
import qualified Data.Text.Lazy.IO          as LText
import           Data.Text.Util
import           System.Directory
import           System.Exit
import           System.FilePath
import qualified Text.EDE                   as EDE
import           Text.EDE.Filters

data Model = Model
    { modPath    :: FilePath
    , modVersion :: String
    } deriving (Show)

modelFromPath :: FilePath -> String -> Model
modelFromPath d f = Model (d </> f) (fst $ break (== '.') f)

models :: [FilePath] -> Script [Model]
models = fmap concat . mapM model
  where
    model d = sync $ map (modelFromPath d) . json <$> getDirectoryContents d

    sync = fmapLT show . syncIO
    json = filter (isSuffixOf ".json")
