{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Functor.Barbie.Record
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.Functor.Barbie.Record where

import Control.Lens (Lens', (%~))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Barbie.Extended (TraversableB, bfor, bmap)
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Some (Some (..))
import Data.Text (Text)
import GHC.Generics ((:+:) (..))

-- | Class for Barbies which are records.
--
-- Laws:
--
-- * Every record field appears once in 'allFields'
-- * @'parseField' ('fieldName' f) == Just (Some f)@ for any field @f@
class (TraversableB b) => RecordB (b :: (Type -> Type) -> Type) where
  data Field b :: Type -> Type
  allFields :: [Some (Field b)]
  fieldName :: Field b a -> Text
  fieldLens :: Field b a -> Lens' (b f) (f a)

  parseField :: Text -> Maybe (Some (Field b))
  parseField field = Map.lookup field table
    where
      table = Map.fromList $ allFields @b <&> \(Some f) -> (fieldName f, Some f)

ibtraverse ::
  forall b f e g.
  (RecordB b, Applicative e) =>
  (forall a. Field b a -> f a -> e (g a)) ->
  b f ->
  e (b g)
ibtraverse f = go (allFields @b) . bmap L1
  where
    -- Because we have to walk the list of fields, we can't use
    -- 'btraverse' to walk the structure in the obvious way. So our
    -- accumulator is `b (f :+: Compose e g)`, we replace fields one
    -- at a time, and collect the results after updating each field.
    go :: [Some (Field b)] -> b (f :+: Compose e g) -> e (b g)
    go [] b = bfor b $ \case
      L1 _ -> error "RecordB law violation"
      R1 (Compose ega) -> ega
    go (Some field : fields) b =
      go fields $ b & fieldLens field %~ update
      where
        update = \case
          L1 fa -> R1 . Compose $ f field fa
          R1 _ -> error "RecordB law violation"

ibfor ::
  (RecordB b, Applicative e) =>
  b f ->
  (forall a. Field b a -> f a -> e (g a)) ->
  e (b g)
ibfor b f = ibtraverse f b
