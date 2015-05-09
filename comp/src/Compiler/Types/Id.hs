{-# LANGUAGE RankNTypes #-}

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
    , ciId
    , memberId
    , typeId
    , ctorId
    , accessorId
    , lensId
    , appendId
    ) where

import           Compiler.Text
import           Control.Lens
import           Data.Aeson           (ToJSON (..))
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Hashable
import           Data.Jason           hiding (ToJSON (..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Manipulate

-- | A type where the actual identifier is immutable,
-- but the usable representation can be appended/modified.
data Id = Id (CI Text) Text
    deriving (Show)

instance Eq Id where
    Id x _ == Id y _ = x == y

instance Hashable Id where
    hashWithSalt n (Id x _) = hashWithSalt n x

instance FromJSON Id where
    parseJSON = withText "id" (pure . textToId)

instance ToJSON Id where
    toJSON = toJSON . view representation

textToId :: Text -> Id
textToId t = Id (CI.mk t) (format t)

format :: Text -> Text
format = upperHead . upperAcronym

representation :: Lens' Id Text
representation =
    lens (\(Id _ t)   -> t)
         (\(Id x _) t -> Id x t)

ciId :: Getter Id (CI Text)
ciId = to (\(Id x _) -> x)

memberId :: Getter Id Text
memberId = ciId . to CI.original

-- FIXME: vPNStaticRoute :: VPNStaticRoute smart ctor name, note vPN
ctorId :: Getter Id Text
ctorId = typeId . to (lowerHead . lowerFirstAcronym)

typeId :: Getter Id Text
typeId = representation . to renameReserved

accessorId :: Text -> Getter Id Text
accessorId p = accessor p . to (Text.cons '_')

lensId :: Text -> Getter Id Text
lensId p = accessor p . to renameReserved

accessor :: Text -> Getter Id Text
accessor p = representation . to (mappend (Text.toLower p) . upperHead)

appendId :: Id -> Text -> Id
appendId i t = i & representation <>~ format t
