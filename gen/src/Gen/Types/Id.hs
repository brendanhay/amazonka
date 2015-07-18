{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- Module      : Gen.Types.Id
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Types.Id
    (
    -- * Class
      HasId (..)

    -- * Identifier
    , Id
    , mkId

    -- * Lenses
    , memberId
    , typeId
    , ctorId
    , branchId
    , smartCtorId
    , accessorId
    , lensId

    -- * Modify representation
    , prependId
    , appendId
    , replaceId
    ) where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Lens
import           Data.Aeson
import           Data.CaseInsensitive   (CI)
import qualified Data.CaseInsensitive   as CI
import           Data.Hashable
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Manipulate
import           Gen.Text

-- | A class to extract identifiers from arbitrary products.
class HasId a where
    identifier :: a -> Id

instance HasId Id where
    identifier = id

instance (Functor f, HasId a) => HasId (Cofree f a) where
    identifier = identifier . extract

-- | A type where the actual identifier is immutable,
-- but the usable representation can be appended/modified.
data Id = Id (CI Text) Text
    deriving (Show)

instance Eq Id where
    Id x _ == Id y _ = x == y

instance Hashable Id where
    hashWithSalt n (Id x _) = hashWithSalt n x

instance FromJSON Id where
    parseJSON = withText "id" (pure . mkId)

instance ToJSON Id where
    toJSON = toJSON . view representation

mkId :: Text -> Id
mkId t = Id (CI.mk t) (format t)

format :: Text -> Text
format = upperHead . upperAcronym

representation :: Lens' Id Text
representation =
    lens (\(Id _ t)   -> t)
         (\(Id x _) t -> Id x (format t))

memberId :: Getter Id Text
memberId = to $ \(Id x _) -> CI.original x

typeId :: Getter Id Text
typeId = representation

ctorId :: Getter Id Text
ctorId = typeId . to (`Text.snoc` '\'')

branchId :: Maybe Text -> Getter Id Text
branchId p = typeId . to f
  where
    f :: Text -> Text
    f | Just x <- p = mappend (upperHead x)
      | otherwise   = id

smartCtorId :: Getter Id Text
smartCtorId = typeId . to (renameReserved . lowerHead . lowerFirstAcronym)

accessorId :: Maybe Text -> Getter Id Text
accessorId p = accessor p . to (Text.cons '_')

lensId :: Maybe Text -> Getter Id Text
lensId p = accessor p . to renameReserved

accessor :: Maybe Text -> Getter Id Text
accessor Nothing  = representation . to lowerHead
accessor (Just p) = representation . to f
  where
    f | Text.null p = lowerHead
      | otherwise   = mappend (Text.toLower p) . upperHead

prependId :: Text -> Id -> Id
prependId t i = i & representation %~ mappend t

appendId :: Id -> Text -> Id
appendId i t = i & representation <>~ t

replaceId :: Id -> Id -> Id
replaceId x y = x & representation .~ y ^. representation
