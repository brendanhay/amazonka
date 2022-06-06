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

import qualified Control.Comonad as Comonad
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Gen.Prelude
import Gen.Text

-- | A class to extract identifiers from arbitrary products.
class HasId a where
  identifier :: a -> Id

instance HasId Id where
  identifier = id

instance (Functor f, HasId a) => HasId (Cofree f a) where
  identifier = identifier . Comonad.extract

-- | A type where the actual identifier is immutable,
-- but the usable representation can be appended/modified.
data Id = Id Text Text
  deriving (Show)

instance Eq Id where
  Id x _ == Id y _ = x == y

instance Ord Id where
  compare (Id x _) (Id y _) = compare x y

instance Hashable Id where
  hashWithSalt n (Id x _) = hashWithSalt n x

instance FromJSONKey Id where
  fromJSONKey = mkId <$> Aeson.fromJSONKey

instance FromJSON Id where
  parseJSON = Aeson.withText "Id" (pure . mkId)

instance ToJSON Id where
  toJSON = Aeson.toJSON . Lens.view representation

mkId :: Text -> Id
mkId t = Id t (format t)

format :: Text -> Text
format = upperHead . Text.dropWhile (not . Char.isAlpha)

partial :: Show a => Id -> HashMap Id a -> String
partial p m =
  let text = Text.take 3 (memberId p)
      matches = HashMap.filterWithKey (const . Text.isPrefixOf text . memberId) m
   in fromString (show (HashMap.toList matches))

representation :: Lens' Id Text
representation =
  Lens.lens
    (\(Id _ t) -> t)
    (\(Id x _) t -> Id x (format t))

memberId :: Id -> Text
memberId (Id x _) = x

typeId :: Id -> Text
typeId = Lens.view representation

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
accessor Nothing = lowerHead . Lens.view representation
accessor (Just p) = f . Lens.view representation
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
