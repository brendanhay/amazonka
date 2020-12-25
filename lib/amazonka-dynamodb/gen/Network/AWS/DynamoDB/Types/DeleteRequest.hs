{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.DynamoDB.Types.AttributeName as Types
import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a request to perform a @DeleteItem@ operation on an item.
--
-- /See:/ 'mkDeleteRequest' smart constructor.
newtype DeleteRequest = DeleteRequest'
  { -- | A map of attribute name to attribute values, representing the primary key of the item to delete. All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema.
    key :: Core.HashMap Types.AttributeName Types.AttributeValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRequest' value with any optional fields omitted.
mkDeleteRequest ::
  DeleteRequest
mkDeleteRequest = DeleteRequest' {key = Core.mempty}

-- | A map of attribute name to attribute values, representing the primary key of the item to delete. All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drKey :: Lens.Lens' DeleteRequest (Core.HashMap Types.AttributeName Types.AttributeValue)
drKey = Lens.field @"key"
{-# DEPRECATED drKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Core.FromJSON DeleteRequest where
  toJSON DeleteRequest {..} =
    Core.object (Core.catMaybes [Core.Just ("Key" Core..= key)])

instance Core.FromJSON DeleteRequest where
  parseJSON =
    Core.withObject "DeleteRequest" Core.$
      \x ->
        DeleteRequest' Core.<$> (x Core..:? "Key" Core..!= Core.mempty)
