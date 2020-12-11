-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteRequest
  ( DeleteRequest (..),

    -- * Smart constructor
    mkDeleteRequest,

    -- * Lenses
    drKey,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a request to perform a @DeleteItem@ operation on an item.
--
-- /See:/ 'mkDeleteRequest' smart constructor.
newtype DeleteRequest = DeleteRequest'
  { key ::
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

-- | Creates a value of 'DeleteRequest' with the minimum fields required to make a request.
--
-- * 'key' - A map of attribute name to attribute values, representing the primary key of the item to delete. All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema.
mkDeleteRequest ::
  DeleteRequest
mkDeleteRequest = DeleteRequest' {key = Lude.mempty}

-- | A map of attribute name to attribute values, representing the primary key of the item to delete. All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drKey :: Lens.Lens' DeleteRequest (Lude.HashMap Lude.Text (AttributeValue))
drKey = Lens.lens (key :: DeleteRequest -> Lude.HashMap Lude.Text (AttributeValue)) (\s a -> s {key = a} :: DeleteRequest)
{-# DEPRECATED drKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON DeleteRequest where
  parseJSON =
    Lude.withObject
      "DeleteRequest"
      ( \x ->
          DeleteRequest' Lude.<$> (x Lude..:? "Key" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON DeleteRequest where
  toJSON DeleteRequest' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Key" Lude..= key)])
