{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription
  ( LocalSecondaryIndexDescription (..),

    -- * Smart constructor
    mkLocalSecondaryIndexDescription,

    -- * Lenses
    lsidIndexArn,
    lsidIndexName,
    lsidIndexSizeBytes,
    lsidItemCount,
    lsidKeySchema,
    lsidProjection,
  )
where

import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.KeySchemaElement as Types
import qualified Network.AWS.DynamoDB.Types.Projection as Types
import qualified Network.AWS.DynamoDB.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a local secondary index.
--
-- /See:/ 'mkLocalSecondaryIndexDescription' smart constructor.
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies the index.
    indexArn :: Core.Maybe Types.String,
    -- | Represents the name of the local secondary index.
    indexName :: Core.Maybe Types.IndexName,
    -- | The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
    indexSizeBytes :: Core.Maybe Core.Integer,
    -- | The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
    itemCount :: Core.Maybe Core.Integer,
    -- | The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:
    --
    --
    --     * @HASH@ - partition key
    --
    --
    --     * @RANGE@ - sort key
    keySchema :: Core.Maybe (Core.NonEmpty Types.KeySchemaElement),
    -- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
    projection :: Core.Maybe Types.Projection
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalSecondaryIndexDescription' value with any optional fields omitted.
mkLocalSecondaryIndexDescription ::
  LocalSecondaryIndexDescription
mkLocalSecondaryIndexDescription =
  LocalSecondaryIndexDescription'
    { indexArn = Core.Nothing,
      indexName = Core.Nothing,
      indexSizeBytes = Core.Nothing,
      itemCount = Core.Nothing,
      keySchema = Core.Nothing,
      projection = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- /Note:/ Consider using 'indexArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidIndexArn :: Lens.Lens' LocalSecondaryIndexDescription (Core.Maybe Types.String)
lsidIndexArn = Lens.field @"indexArn"
{-# DEPRECATED lsidIndexArn "Use generic-lens or generic-optics with 'indexArn' instead." #-}

-- | Represents the name of the local secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidIndexName :: Lens.Lens' LocalSecondaryIndexDescription (Core.Maybe Types.IndexName)
lsidIndexName = Lens.field @"indexName"
{-# DEPRECATED lsidIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'indexSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidIndexSizeBytes :: Lens.Lens' LocalSecondaryIndexDescription (Core.Maybe Core.Integer)
lsidIndexSizeBytes = Lens.field @"indexSizeBytes"
{-# DEPRECATED lsidIndexSizeBytes "Use generic-lens or generic-optics with 'indexSizeBytes' instead." #-}

-- | The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidItemCount :: Lens.Lens' LocalSecondaryIndexDescription (Core.Maybe Core.Integer)
lsidItemCount = Lens.field @"itemCount"
{-# DEPRECATED lsidItemCount "Use generic-lens or generic-optics with 'itemCount' instead." #-}

-- | The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
--
-- /Note:/ Consider using 'keySchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidKeySchema :: Lens.Lens' LocalSecondaryIndexDescription (Core.Maybe (Core.NonEmpty Types.KeySchemaElement))
lsidKeySchema = Lens.field @"keySchema"
{-# DEPRECATED lsidKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidProjection :: Lens.Lens' LocalSecondaryIndexDescription (Core.Maybe Types.Projection)
lsidProjection = Lens.field @"projection"
{-# DEPRECATED lsidProjection "Use generic-lens or generic-optics with 'projection' instead." #-}

instance Core.FromJSON LocalSecondaryIndexDescription where
  parseJSON =
    Core.withObject "LocalSecondaryIndexDescription" Core.$
      \x ->
        LocalSecondaryIndexDescription'
          Core.<$> (x Core..:? "IndexArn")
          Core.<*> (x Core..:? "IndexName")
          Core.<*> (x Core..:? "IndexSizeBytes")
          Core.<*> (x Core..:? "ItemCount")
          Core.<*> (x Core..:? "KeySchema")
          Core.<*> (x Core..:? "Projection")
