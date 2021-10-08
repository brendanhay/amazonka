-- Module      : Gen.Types.Id
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Id
  ( -- * Class
    HasId (..),

    -- * Identifier
    Id (..),
    mkId,

    -- * Lenses
    memberId,
    typeId,
    ctorId,
    branchId,
    smartCtorId,
    accessorId,
    lensId,

    -- * Modify representation
    prependId,
    appendId,
    replaceId,

    -- * Utilities
    partial,
  )
where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Lens
import Data.Aeson
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Gen.Text

-- | A class to extract identifiers from arbitrary products.
class HasId a where
  identifier :: a -> Id

instance HasId Id where
  identifier = id

instance (Functor f, HasId a) => HasId (Cofree f a) where
  identifier = identifier . extract

-- | A type where the actual identifier is immutable,
-- but the usable representation can be appended/modified.
data Id = Id Text Text
  deriving (Show)

instance Eq Id where
  Id x _ == Id y _ = x == y

instance Hashable Id where
  hashWithSalt n (Id x _) = hashWithSalt n x

instance FromJSONKey Id where
  fromJSONKey = mkId <$> fromJSONKey

instance FromJSON Id where
  parseJSON = withText "id" (pure . mkId)

instance ToJSON Id where
  toJSON = toJSON . view representation

mkId :: Text -> Id
mkId t = Id t (format t)

format :: Text -> Text
format = upperHead . Text.dropWhile (not . Char.isAlpha)

partial :: Show a => Id -> Map.HashMap Id a -> String
partial p m =
  let text = Text.take 3 (memberId p)
      matches = Map.filterWithKey (const . Text.isPrefixOf text . memberId) m
   in fromString (show (Map.toList matches))

representation :: Lens' Id Text
representation =
  lens
    (\(Id _ t) -> t)
    (\(Id x _) t -> Id x (format t))

memberId :: Id -> Text
memberId (Id x _) = x

typeId :: Id -> Text
typeId = view representation

ctorId :: Id -> Text
ctorId = (`Text.snoc` '\'') . typeId

branchId :: Maybe Text -> Id -> Text
branchId Nothing = typeId
branchId (Just p) = mappend p . mappend "_" . typeId

smartCtorId :: Id -> Text
smartCtorId = mappend "new" . typeId

accessorId :: Id -> Text
accessorId = renameReserved . lowerHead . memberId

lensId :: Maybe Text -> Id -> Text
lensId p = accessor p

accessor :: Maybe Text -> Id -> Text
accessor Nothing = lowerHead . view representation
accessor (Just p) = f . view representation
  where
    f
      | Text.null p = lowerHead
      | otherwise = mappend (lowerHead p) . mappend "_" . lowerHead

prependId :: Text -> Id -> Id
prependId t i = i & representation %~ mappend t

appendId :: Id -> Text -> Id
appendId i t = i & representation <>~ t

replaceId :: Id -> Id -> Id
replaceId x y = x & representation .~ y ^. representation
