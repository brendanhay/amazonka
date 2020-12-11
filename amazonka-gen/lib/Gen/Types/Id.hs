-- |
-- Module      : Gen.Types.Id
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.Id
  ( -- * Class
    HasId (..),

    -- * Identifier
    Id (UnsafeId),
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

    -- * Partial identifier matches
    partial,
  )
where

import qualified Control.Comonad as Comonad
import qualified Control.Comonad.Cofree as Comonad.Cofree
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Hashable as Hashable
import qualified Data.Text as Text
import qualified Data.Text.Manipulate as Manipulate
import Gen.Prelude
import Gen.Text

-- | A class to Comonad.extract identifiers from arbitrary products.
class HasId a where
  identifier :: a -> Id

instance HasId Id where
  identifier = id

instance (Functor f, HasId a) => HasId (Cofree f a) where
  identifier = identifier . Comonad.extract

-- | A type where the actual identifier is immutable,
-- but the usable representation can be appended/modified.
data Id = UnsafeId Text Text

instance Show Id where
  show (UnsafeId original assigned) =
    Text.unpack (original <> "<" <> assigned <> ">")

instance Eq Id where
  UnsafeId x _ == UnsafeId y _ = x == y

instance Ord Id where
  UnsafeId x _ `compare` UnsafeId y _ = x `compare` y

instance Hashable Id where
  hashWithSalt n (UnsafeId x _) = Hashable.hashWithSalt n x

instance FromJSONKey Id where
  fromJSONKey = mkId <$> Aeson.fromJSONKey

instance FromJSON Id where
  parseJSON = Aeson.withText "id" (pure . mkId)

instance ToJSON Id where
  toJSON = Aeson.toJSON . Lens.view representation

mkId :: Text -> Id
mkId t = UnsafeId t (format t)

format :: Text -> Text
format =
  Manipulate.upperHead
    . upperAcronym
    . Text.dropWhile (not . Char.isAlpha)

representation :: Lens' Id Text
representation =
  Lens.lens
    (\(UnsafeId _ t) -> t)
    (\(UnsafeId x _) t -> UnsafeId x (format t))

memberId :: Id -> Text
memberId (UnsafeId x _) = x

typeId :: Id -> Text
typeId = Lens.view representation

ctorId :: Id -> Text
ctorId = (`Text.snoc` '\'') . typeId

branchId :: Maybe Text -> Id -> Text
branchId p = f . typeId
  where
    f :: Text -> Text
    f
      | Just x <- p = mappend (Manipulate.upperHead x)
      | otherwise = id

smartCtorId :: Id -> Text
smartCtorId = mappend "mk" . typeId

accessorId :: Id -> Text
accessorId = renameReserved . lowerFirstAcronym . Lens.view representation

lensId :: Maybe Text -> Id -> Text
lensId p = renameReserved . accessor p
  where
    accessor Nothing = lowerFirstAcronym . Lens.view representation
    accessor (Just p) = f . Lens.view representation
      where
        f
          | Text.null p = Manipulate.lowerHead
          | otherwise = mappend (Text.toLower p) . Manipulate.upperHead

prependId :: Text -> Id -> Id
prependId t i = i & representation %~ mappend t

appendId :: Id -> Text -> Id
appendId i t = i & representation <>~ t

replaceId :: Id -> Id -> Id
replaceId x y = x & representation .~ y ^. representation

partial :: Id -> (HashMap.HashMap Id a) -> [(Id, a)]
partial p m =
  let txt = Text.take 3 (memberId p)
   in HashMap.toList $
        HashMap.filterWithKey (const . Text.isPrefixOf txt . memberId) m
