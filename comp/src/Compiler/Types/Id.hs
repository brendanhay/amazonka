-- Module      : Compiler.Types.Id
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Id
    ( Id
    , textToId
    , keyCI
    , keyOriginal
    , keyActual
    , keyAppend
    ) where

import           Compiler.Text
import           Control.Lens
import           Data.Aeson           (ToJSON (..))
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Hashable
import           Data.Jason           hiding (ToJSON (..))
import           Data.String
import           Data.Text            (Text)

data Id = Id (CI Text) Text
    deriving (Show)

textToId :: Text -> Id
textToId t = Id (CI.mk t) t

keyCI :: Getter Id (CI Text)
keyCI = to (\(Id ci _) -> ci)

keyOriginal :: Getter Id Text
keyOriginal = keyCI . to CI.original

keyActual :: Lens' Id Text
keyActual = lens (\(Id _ t) -> upperAcronym t) (\(Id ci _) t -> Id ci t)

keyAppend :: Id -> Text -> Id
keyAppend i t = i & keyActual <>~ t

instance Eq Id where
    Id x _ == Id y _ = x == y

instance Hashable Id where
    hashWithSalt n (Id ci _) = hashWithSalt n ci

instance IsString Id where
    fromString = textToId . fromString

instance FromJSON Id where
    parseJSON = withText "id" (pure . textToId)

instance ToJSON Id where
    toJSON = toJSON . view keyActual

-- data Key = Key Id Text
--     deriving (Show)

-- textToKey :: Text -> Key
-- textToKey t = Key (textToId t) t

-- keyId :: Lens' Key Id
-- keyId = lens (\(Key i _) -> i) (\(Key _ t) i -> Key i t)

-- keyAppend :: Key -> Text -> Key
-- keyAppend (Key i t) = Key i . mappend t

-- instance Eq Key where
--     (==) = on (==) (view keyId)

-- instance Hashable Key where
--     hashWithSalt n = hashWithSalt n . view keyId

-- instance IsString Key where
--     fromString = textToKey . fromString

-- instance FromJSON Key where
--     parseJSON = withText "key" (pure . textToKey)

-- instance ToJSON Key where
--     toJSON (Key _ t) = toJSON (upperAcronym t)
