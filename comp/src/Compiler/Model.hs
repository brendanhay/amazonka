{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Compiler.Model
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Model where

import           Compiler.Types
import           Control.Error
import           Control.Lens              hiding ((<.>), (??))
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.Function             (on)
import           Data.List                 (nub)
import           Data.List.NonEmpty        (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Ord
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import           Data.Text.Lazy.Builder    (Builder, toLazyText)
import qualified Data.Text.Lazy.IO         as LText
import           Data.Time
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS
import           Formatting                hiding (left)

waiters, paginators, normal :: Text
waiters    = "waiters.json"
paginators = "paginators.json"
normal     = "normal.json"

data Ver = Ver
    { _verDate    :: UTCTime
    , _verNormal  :: Path
    , _verWaiters :: Path
    , _verPagers  :: Path
    } deriving (Eq, Show)

makeLenses ''Ver

versionFromFile :: Monad m => Path -> EitherT LazyText m Ver
versionFromFile f
    | not (hasExtension f "json")
                = failure ("Unexpected model version " % path) f
    | otherwise = do
        let dir  = directory f
            base = basename f
        t <- parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) (encodeString base)
            ?? format ("Unable to parse ISO8601 date " % path % " from " % path) base f
        return $! Ver t f
            (dir </> base <.> paginators)
            (dir </> base <.> waiters)

data Model = Model
    { _modName      :: Text
    , _modDirectory :: Dir
    , _modVersions  :: NonEmpty Ver
    } deriving (Show)

makeLenses ''Model

override :: Path -> Model -> Path
override dir m = dir </> fromText (_modName m) <.> "json"

latest :: Getter Model Ver
latest = to (NonEmpty.head . _modVersions)

unused :: Fold Model Ver
unused = folding (NonEmpty.tail . _modVersions)

modelFromDir :: Monad m => Dir -> EitherT LazyText m Model
modelFromDir d@(Dir f xs) = do
    let dir  = basename f
        name = toTextIgnore dir
        norm = filter (Text.isSuffixOf normal . toTextIgnore)
    ys <- nempty _verDate <$> mapM versionFromFile (norm xs)
    Model name d <$> ys
        ?? format ("Failed to find any model versions for " % stext) name

nempty :: (Eq a, Ord b) => (a -> b) -> [a] -> Maybe (NonEmpty a)
nempty f = fmap (NonEmpty.sortBy (on compare (Down . f))) . nonEmpty . nub
