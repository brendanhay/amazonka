-- Module      : Gen.Formatting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Formatting
  ( module Gen.Formatting,
    module Formatting,
    module Formatting.Time,
  )
where

import Control.Monad.Except
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Builder qualified as Build
import Formatting hiding (left, right)
import Formatting.Time hiding (fmt)
import Gen.Types

scomma :: Format a ([Text] -> a)
scomma = later (Build.fromText . Text.intercalate ",")

soriginal :: Format a (CI Text -> a)
soriginal = later (Build.fromText . CI.original)

iprimary :: Format a (Id -> a)
iprimary = later (Build.fromText . memberId)

itype :: Format a (Id -> a)
itype = later (Build.fromText . typeId)

path :: Format a (Path -> a)
path = later (Build.fromText . toTextIgnore)

partial :: Show b => Format a ((Id, Map.HashMap Id b) -> a)
partial = later (Build.fromString . show . Map.toList . prefix)
  where
    prefix (p, m) =
      let txt = Text.take 3 (memberId p)
       in Map.filterWithKey (const . Text.isPrefixOf txt . memberId) m

failure :: MonadError e m => Format LText.Text (a -> e) -> a -> m b
failure m = throwError . format m
