-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.PutRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.PutRequest
  ( PutRequest (..),

    -- * Smart constructor
    mkPutRequest,

    -- * Lenses
    prItem,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a request to perform a @PutItem@ operation on an item.
--
-- /See:/ 'mkPutRequest' smart constructor.
newtype PutRequest = PutRequest'
  { item ::
      Lude.HashMap Lude.Text (AttributeValue)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRequest' with the minimum fields required to make a request.
--
-- * 'item' - A map of attribute name to attribute values, representing the primary key of an item to be processed by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
mkPutRequest ::
  PutRequest
mkPutRequest = PutRequest' {item = Lude.mempty}

-- | A map of attribute name to attribute values, representing the primary key of an item to be processed by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prItem :: Lens.Lens' PutRequest (Lude.HashMap Lude.Text (AttributeValue))
prItem = Lens.lens (item :: PutRequest -> Lude.HashMap Lude.Text (AttributeValue)) (\s a -> s {item = a} :: PutRequest)
{-# DEPRECATED prItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.FromJSON PutRequest where
  parseJSON =
    Lude.withObject
      "PutRequest"
      ( \x ->
          PutRequest' Lude.<$> (x Lude..:? "Item" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON PutRequest where
  toJSON PutRequest' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Item" Lude..= item)])
