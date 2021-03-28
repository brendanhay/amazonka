{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription
  ( GlobalSecondaryIndexDescription (..)
  -- * Smart constructor
  , mkGlobalSecondaryIndexDescription
  -- * Lenses
  , gsidBackfilling
  , gsidIndexArn
  , gsidIndexName
  , gsidIndexSizeBytes
  , gsidIndexStatus
  , gsidItemCount
  , gsidKeySchema
  , gsidProjection
  , gsidProvisionedThroughput
  ) where

import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.IndexStatus as Types
import qualified Network.AWS.DynamoDB.Types.KeySchemaElement as Types
import qualified Network.AWS.DynamoDB.Types.Projection as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'mkGlobalSecondaryIndexDescription' smart constructor.
data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription'
  { backfilling :: Core.Maybe Core.Bool
    -- ^ Indicates whether the index is currently backfilling. /Backfilling/ is the process of reading items from the table and determining whether they can be added to the index. (Not all items will qualify: For example, a partition key cannot have any duplicate values.) If an item can be added to the index, DynamoDB will do so. After all items have been processed, the backfilling operation is complete and @Backfilling@ is false.
--
-- You can delete an index that is being created during the @Backfilling@ phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true. You can't delete the index that is being created when @IndexStatus@ is set to CREATING and @Backfilling@ is false. 
  , indexArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) that uniquely identifies the index.
  , indexName :: Core.Maybe Types.IndexName
    -- ^ The name of the global secondary index.
  , indexSizeBytes :: Core.Maybe Core.Integer
    -- ^ The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
  , indexStatus :: Core.Maybe Types.IndexStatus
    -- ^ The current state of the global secondary index:
--
--
--     * @CREATING@ - The index is being created.
--
--
--     * @UPDATING@ - The index is being updated.
--
--
--     * @DELETING@ - The index is being deleted.
--
--
--     * @ACTIVE@ - The index is ready for use.
--
--
  , itemCount :: Core.Maybe Core.Integer
    -- ^ The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
  , keySchema :: Core.Maybe (Core.NonEmpty Types.KeySchemaElement)
    -- ^ The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
  , projection :: Core.Maybe Types.Projection
    -- ^ Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. 
  , provisionedThroughput :: Core.Maybe Types.ProvisionedThroughputDescription
    -- ^ Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GlobalSecondaryIndexDescription' value with any optional fields omitted.
mkGlobalSecondaryIndexDescription
    :: GlobalSecondaryIndexDescription
mkGlobalSecondaryIndexDescription
  = GlobalSecondaryIndexDescription'{backfilling = Core.Nothing,
                                     indexArn = Core.Nothing, indexName = Core.Nothing,
                                     indexSizeBytes = Core.Nothing, indexStatus = Core.Nothing,
                                     itemCount = Core.Nothing, keySchema = Core.Nothing,
                                     projection = Core.Nothing,
                                     provisionedThroughput = Core.Nothing}

-- | Indicates whether the index is currently backfilling. /Backfilling/ is the process of reading items from the table and determining whether they can be added to the index. (Not all items will qualify: For example, a partition key cannot have any duplicate values.) If an item can be added to the index, DynamoDB will do so. After all items have been processed, the backfilling operation is complete and @Backfilling@ is false.
--
-- You can delete an index that is being created during the @Backfilling@ phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true. You can't delete the index that is being created when @IndexStatus@ is set to CREATING and @Backfilling@ is false. 
--
-- /Note:/ Consider using 'backfilling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidBackfilling :: Lens.Lens' GlobalSecondaryIndexDescription (Core.Maybe Core.Bool)
gsidBackfilling = Lens.field @"backfilling"
{-# INLINEABLE gsidBackfilling #-}
{-# DEPRECATED backfilling "Use generic-lens or generic-optics with 'backfilling' instead"  #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- /Note:/ Consider using 'indexArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidIndexArn :: Lens.Lens' GlobalSecondaryIndexDescription (Core.Maybe Core.Text)
gsidIndexArn = Lens.field @"indexArn"
{-# INLINEABLE gsidIndexArn #-}
{-# DEPRECATED indexArn "Use generic-lens or generic-optics with 'indexArn' instead"  #-}

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidIndexName :: Lens.Lens' GlobalSecondaryIndexDescription (Core.Maybe Types.IndexName)
gsidIndexName = Lens.field @"indexName"
{-# INLINEABLE gsidIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'indexSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidIndexSizeBytes :: Lens.Lens' GlobalSecondaryIndexDescription (Core.Maybe Core.Integer)
gsidIndexSizeBytes = Lens.field @"indexSizeBytes"
{-# INLINEABLE gsidIndexSizeBytes #-}
{-# DEPRECATED indexSizeBytes "Use generic-lens or generic-optics with 'indexSizeBytes' instead"  #-}

-- | The current state of the global secondary index:
--
--
--     * @CREATING@ - The index is being created.
--
--
--     * @UPDATING@ - The index is being updated.
--
--
--     * @DELETING@ - The index is being deleted.
--
--
--     * @ACTIVE@ - The index is ready for use.
--
--
--
-- /Note:/ Consider using 'indexStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidIndexStatus :: Lens.Lens' GlobalSecondaryIndexDescription (Core.Maybe Types.IndexStatus)
gsidIndexStatus = Lens.field @"indexStatus"
{-# INLINEABLE gsidIndexStatus #-}
{-# DEPRECATED indexStatus "Use generic-lens or generic-optics with 'indexStatus' instead"  #-}

-- | The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidItemCount :: Lens.Lens' GlobalSecondaryIndexDescription (Core.Maybe Core.Integer)
gsidItemCount = Lens.field @"itemCount"
{-# INLINEABLE gsidItemCount #-}
{-# DEPRECATED itemCount "Use generic-lens or generic-optics with 'itemCount' instead"  #-}

-- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:
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
gsidKeySchema :: Lens.Lens' GlobalSecondaryIndexDescription (Core.Maybe (Core.NonEmpty Types.KeySchemaElement))
gsidKeySchema = Lens.field @"keySchema"
{-# INLINEABLE gsidKeySchema #-}
{-# DEPRECATED keySchema "Use generic-lens or generic-optics with 'keySchema' instead"  #-}

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. 
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidProjection :: Lens.Lens' GlobalSecondaryIndexDescription (Core.Maybe Types.Projection)
gsidProjection = Lens.field @"projection"
{-# INLINEABLE gsidProjection #-}
{-# DEPRECATED projection "Use generic-lens or generic-optics with 'projection' instead"  #-}

-- | Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidProvisionedThroughput :: Lens.Lens' GlobalSecondaryIndexDescription (Core.Maybe Types.ProvisionedThroughputDescription)
gsidProvisionedThroughput = Lens.field @"provisionedThroughput"
{-# INLINEABLE gsidProvisionedThroughput #-}
{-# DEPRECATED provisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead"  #-}

instance Core.FromJSON GlobalSecondaryIndexDescription where
        parseJSON
          = Core.withObject "GlobalSecondaryIndexDescription" Core.$
              \ x ->
                GlobalSecondaryIndexDescription' Core.<$>
                  (x Core..:? "Backfilling") Core.<*> x Core..:? "IndexArn" Core.<*>
                    x Core..:? "IndexName"
                    Core.<*> x Core..:? "IndexSizeBytes"
                    Core.<*> x Core..:? "IndexStatus"
                    Core.<*> x Core..:? "ItemCount"
                    Core.<*> x Core..:? "KeySchema"
                    Core.<*> x Core..:? "Projection"
                    Core.<*> x Core..:? "ProvisionedThroughput"
