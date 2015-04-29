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
import           Data.Function             (on)
import           Data.List                 (nub)
import           Data.List.NonEmpty        (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Ord
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Time
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

versionFromFile :: Monad m => Path -> Compiler m Ver
versionFromFile f
    | not (hasExtension f "json")
                = failure ("Unexpected model version " % path) f
    | otherwise = do
        let d = directory f
            b = basename f
        t <- parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) (encodeString b)
            ?? format ("Unable to parse ISO8601 date " % path % " from " % path) b f
        return $! Ver t f
            (d </> b <.> paginators)
            (d </> b <.> waiters)

data Model = Model
    { _modName      :: Text
    , _modDirectory :: Path
    , _modVersions  :: NonEmpty Ver
    } deriving (Show)

makeLenses ''Model

configFile, annexFile :: Getter Model Path
configFile = to (flip addExtension "json" . fromText . _modName)
annexFile  = configFile

latest :: Getter Model Ver
latest = to (NonEmpty.head . _modVersions)

unused :: Fold Model Ver
unused = folding (NonEmpty.tail . _modVersions)

modelFromDir :: Monad m => Path -> [Path] -> Compiler m Model
modelFromDir p xs = do
    let d = basename p
        n = toTextIgnore d
        f = filter (Text.isSuffixOf normal . toTextIgnore)
    ys <- ordNonEmpty _verDate <$> mapM versionFromFile (f xs)
    Model n p <$> ys
        ?? format ("Failed to find any model versions for " % stext) n

ordNonEmpty :: (Eq a, Ord b) => (a -> b) -> [a] -> Maybe (NonEmpty a)
ordNonEmpty f = fmap (NonEmpty.sortBy (on compare (Down . f))) . nonEmpty . nub
