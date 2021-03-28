{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.PutRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.PutRequest
  ( PutRequest (..)
  -- * Smart constructor
  , mkPutRequest
  -- * Lenses
  , prItem
  ) where

import qualified Network.AWS.DynamoDB.Types.AttributeName as Types
import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a request to perform a @PutItem@ operation on an item.
--
-- /See:/ 'mkPutRequest' smart constructor.
newtype PutRequest = PutRequest'
  { item :: Core.HashMap Types.AttributeName Types.AttributeValue
    -- ^ A map of attribute name to attribute values, representing the primary key of an item to be processed by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutRequest' value with any optional fields omitted.
mkPutRequest
    :: PutRequest
mkPutRequest = PutRequest'{item = Core.mempty}

-- | A map of attribute name to attribute values, representing the primary key of an item to be processed by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prItem :: Lens.Lens' PutRequest (Core.HashMap Types.AttributeName Types.AttributeValue)
prItem = Lens.field @"item"
{-# INLINEABLE prItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

instance Core.FromJSON PutRequest where
        toJSON PutRequest{..}
          = Core.object (Core.catMaybes [Core.Just ("Item" Core..= item)])

instance Core.FromJSON PutRequest where
        parseJSON
          = Core.withObject "PutRequest" Core.$
              \ x ->
                PutRequest' Core.<$> (x Core..:? "Item" Core..!= Core.mempty)
