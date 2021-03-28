{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ConsumedCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ConsumedCapacity
  ( ConsumedCapacity (..)
  -- * Smart constructor
  , mkConsumedCapacity
  -- * Lenses
  , ccfCapacityUnits
  , ccfGlobalSecondaryIndexes
  , ccfLocalSecondaryIndexes
  , ccfReadCapacityUnits
  , ccfTable
  , ccfTableName
  , ccfWriteCapacityUnits
  ) where

import qualified Network.AWS.DynamoDB.Types.Capacity as Types
import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The capacity units consumed by an operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the request asked for it. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
-- /See:/ 'mkConsumedCapacity' smart constructor.
data ConsumedCapacity = ConsumedCapacity'
  { capacityUnits :: Core.Maybe Core.Double
    -- ^ The total number of capacity units consumed by the operation.
  , globalSecondaryIndexes :: Core.Maybe (Core.HashMap Types.IndexName Types.Capacity)
    -- ^ The amount of throughput consumed on each global index affected by the operation.
  , localSecondaryIndexes :: Core.Maybe (Core.HashMap Types.IndexName Types.Capacity)
    -- ^ The amount of throughput consumed on each local index affected by the operation.
  , readCapacityUnits :: Core.Maybe Core.Double
    -- ^ The total number of read capacity units consumed by the operation.
  , table :: Core.Maybe Types.Capacity
    -- ^ The amount of throughput consumed on the table affected by the operation.
  , tableName :: Core.Maybe Types.TableName
    -- ^ The name of the table that was affected by the operation.
  , writeCapacityUnits :: Core.Maybe Core.Double
    -- ^ The total number of write capacity units consumed by the operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConsumedCapacity' value with any optional fields omitted.
mkConsumedCapacity
    :: ConsumedCapacity
mkConsumedCapacity
  = ConsumedCapacity'{capacityUnits = Core.Nothing,
                      globalSecondaryIndexes = Core.Nothing,
                      localSecondaryIndexes = Core.Nothing,
                      readCapacityUnits = Core.Nothing, table = Core.Nothing,
                      tableName = Core.Nothing, writeCapacityUnits = Core.Nothing}

-- | The total number of capacity units consumed by the operation.
--
-- /Note:/ Consider using 'capacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfCapacityUnits :: Lens.Lens' ConsumedCapacity (Core.Maybe Core.Double)
ccfCapacityUnits = Lens.field @"capacityUnits"
{-# INLINEABLE ccfCapacityUnits #-}
{-# DEPRECATED capacityUnits "Use generic-lens or generic-optics with 'capacityUnits' instead"  #-}

-- | The amount of throughput consumed on each global index affected by the operation.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfGlobalSecondaryIndexes :: Lens.Lens' ConsumedCapacity (Core.Maybe (Core.HashMap Types.IndexName Types.Capacity))
ccfGlobalSecondaryIndexes = Lens.field @"globalSecondaryIndexes"
{-# INLINEABLE ccfGlobalSecondaryIndexes #-}
{-# DEPRECATED globalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead"  #-}

-- | The amount of throughput consumed on each local index affected by the operation.
--
-- /Note:/ Consider using 'localSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfLocalSecondaryIndexes :: Lens.Lens' ConsumedCapacity (Core.Maybe (Core.HashMap Types.IndexName Types.Capacity))
ccfLocalSecondaryIndexes = Lens.field @"localSecondaryIndexes"
{-# INLINEABLE ccfLocalSecondaryIndexes #-}
{-# DEPRECATED localSecondaryIndexes "Use generic-lens or generic-optics with 'localSecondaryIndexes' instead"  #-}

-- | The total number of read capacity units consumed by the operation.
--
-- /Note:/ Consider using 'readCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfReadCapacityUnits :: Lens.Lens' ConsumedCapacity (Core.Maybe Core.Double)
ccfReadCapacityUnits = Lens.field @"readCapacityUnits"
{-# INLINEABLE ccfReadCapacityUnits #-}
{-# DEPRECATED readCapacityUnits "Use generic-lens or generic-optics with 'readCapacityUnits' instead"  #-}

-- | The amount of throughput consumed on the table affected by the operation.
--
-- /Note:/ Consider using 'table' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTable :: Lens.Lens' ConsumedCapacity (Core.Maybe Types.Capacity)
ccfTable = Lens.field @"table"
{-# INLINEABLE ccfTable #-}
{-# DEPRECATED table "Use generic-lens or generic-optics with 'table' instead"  #-}

-- | The name of the table that was affected by the operation.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTableName :: Lens.Lens' ConsumedCapacity (Core.Maybe Types.TableName)
ccfTableName = Lens.field @"tableName"
{-# INLINEABLE ccfTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The total number of write capacity units consumed by the operation.
--
-- /Note:/ Consider using 'writeCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfWriteCapacityUnits :: Lens.Lens' ConsumedCapacity (Core.Maybe Core.Double)
ccfWriteCapacityUnits = Lens.field @"writeCapacityUnits"
{-# INLINEABLE ccfWriteCapacityUnits #-}
{-# DEPRECATED writeCapacityUnits "Use generic-lens or generic-optics with 'writeCapacityUnits' instead"  #-}

instance Core.FromJSON ConsumedCapacity where
        parseJSON
          = Core.withObject "ConsumedCapacity" Core.$
              \ x ->
                ConsumedCapacity' Core.<$>
                  (x Core..:? "CapacityUnits") Core.<*>
                    x Core..:? "GlobalSecondaryIndexes"
                    Core.<*> x Core..:? "LocalSecondaryIndexes"
                    Core.<*> x Core..:? "ReadCapacityUnits"
                    Core.<*> x Core..:? "Table"
                    Core.<*> x Core..:? "TableName"
                    Core.<*> x Core..:? "WriteCapacityUnits"
