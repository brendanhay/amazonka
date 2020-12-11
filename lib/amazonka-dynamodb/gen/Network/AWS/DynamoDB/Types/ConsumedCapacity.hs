-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ConsumedCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ConsumedCapacity
  ( ConsumedCapacity (..),

    -- * Smart constructor
    mkConsumedCapacity,

    -- * Lenses
    cReadCapacityUnits,
    cGlobalSecondaryIndexes,
    cCapacityUnits,
    cWriteCapacityUnits,
    cLocalSecondaryIndexes,
    cTable,
    cTableName,
  )
where

import Network.AWS.DynamoDB.Types.Capacity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The capacity units consumed by an operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the request asked for it. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
-- /See:/ 'mkConsumedCapacity' smart constructor.
data ConsumedCapacity = ConsumedCapacity'
  { readCapacityUnits ::
      Lude.Maybe Lude.Double,
    globalSecondaryIndexes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Capacity)),
    capacityUnits :: Lude.Maybe Lude.Double,
    writeCapacityUnits :: Lude.Maybe Lude.Double,
    localSecondaryIndexes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Capacity)),
    table :: Lude.Maybe Capacity,
    tableName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConsumedCapacity' with the minimum fields required to make a request.
--
-- * 'capacityUnits' - The total number of capacity units consumed by the operation.
-- * 'globalSecondaryIndexes' - The amount of throughput consumed on each global index affected by the operation.
-- * 'localSecondaryIndexes' - The amount of throughput consumed on each local index affected by the operation.
-- * 'readCapacityUnits' - The total number of read capacity units consumed by the operation.
-- * 'table' - The amount of throughput consumed on the table affected by the operation.
-- * 'tableName' - The name of the table that was affected by the operation.
-- * 'writeCapacityUnits' - The total number of write capacity units consumed by the operation.
mkConsumedCapacity ::
  ConsumedCapacity
mkConsumedCapacity =
  ConsumedCapacity'
    { readCapacityUnits = Lude.Nothing,
      globalSecondaryIndexes = Lude.Nothing,
      capacityUnits = Lude.Nothing,
      writeCapacityUnits = Lude.Nothing,
      localSecondaryIndexes = Lude.Nothing,
      table = Lude.Nothing,
      tableName = Lude.Nothing
    }

-- | The total number of read capacity units consumed by the operation.
--
-- /Note:/ Consider using 'readCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReadCapacityUnits :: Lens.Lens' ConsumedCapacity (Lude.Maybe Lude.Double)
cReadCapacityUnits = Lens.lens (readCapacityUnits :: ConsumedCapacity -> Lude.Maybe Lude.Double) (\s a -> s {readCapacityUnits = a} :: ConsumedCapacity)
{-# DEPRECATED cReadCapacityUnits "Use generic-lens or generic-optics with 'readCapacityUnits' instead." #-}

-- | The amount of throughput consumed on each global index affected by the operation.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGlobalSecondaryIndexes :: Lens.Lens' ConsumedCapacity (Lude.Maybe (Lude.HashMap Lude.Text (Capacity)))
cGlobalSecondaryIndexes = Lens.lens (globalSecondaryIndexes :: ConsumedCapacity -> Lude.Maybe (Lude.HashMap Lude.Text (Capacity))) (\s a -> s {globalSecondaryIndexes = a} :: ConsumedCapacity)
{-# DEPRECATED cGlobalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead." #-}

-- | The total number of capacity units consumed by the operation.
--
-- /Note:/ Consider using 'capacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCapacityUnits :: Lens.Lens' ConsumedCapacity (Lude.Maybe Lude.Double)
cCapacityUnits = Lens.lens (capacityUnits :: ConsumedCapacity -> Lude.Maybe Lude.Double) (\s a -> s {capacityUnits = a} :: ConsumedCapacity)
{-# DEPRECATED cCapacityUnits "Use generic-lens or generic-optics with 'capacityUnits' instead." #-}

-- | The total number of write capacity units consumed by the operation.
--
-- /Note:/ Consider using 'writeCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cWriteCapacityUnits :: Lens.Lens' ConsumedCapacity (Lude.Maybe Lude.Double)
cWriteCapacityUnits = Lens.lens (writeCapacityUnits :: ConsumedCapacity -> Lude.Maybe Lude.Double) (\s a -> s {writeCapacityUnits = a} :: ConsumedCapacity)
{-# DEPRECATED cWriteCapacityUnits "Use generic-lens or generic-optics with 'writeCapacityUnits' instead." #-}

-- | The amount of throughput consumed on each local index affected by the operation.
--
-- /Note:/ Consider using 'localSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLocalSecondaryIndexes :: Lens.Lens' ConsumedCapacity (Lude.Maybe (Lude.HashMap Lude.Text (Capacity)))
cLocalSecondaryIndexes = Lens.lens (localSecondaryIndexes :: ConsumedCapacity -> Lude.Maybe (Lude.HashMap Lude.Text (Capacity))) (\s a -> s {localSecondaryIndexes = a} :: ConsumedCapacity)
{-# DEPRECATED cLocalSecondaryIndexes "Use generic-lens or generic-optics with 'localSecondaryIndexes' instead." #-}

-- | The amount of throughput consumed on the table affected by the operation.
--
-- /Note:/ Consider using 'table' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTable :: Lens.Lens' ConsumedCapacity (Lude.Maybe Capacity)
cTable = Lens.lens (table :: ConsumedCapacity -> Lude.Maybe Capacity) (\s a -> s {table = a} :: ConsumedCapacity)
{-# DEPRECATED cTable "Use generic-lens or generic-optics with 'table' instead." #-}

-- | The name of the table that was affected by the operation.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTableName :: Lens.Lens' ConsumedCapacity (Lude.Maybe Lude.Text)
cTableName = Lens.lens (tableName :: ConsumedCapacity -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: ConsumedCapacity)
{-# DEPRECATED cTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON ConsumedCapacity where
  parseJSON =
    Lude.withObject
      "ConsumedCapacity"
      ( \x ->
          ConsumedCapacity'
            Lude.<$> (x Lude..:? "ReadCapacityUnits")
            Lude.<*> (x Lude..:? "GlobalSecondaryIndexes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CapacityUnits")
            Lude.<*> (x Lude..:? "WriteCapacityUnits")
            Lude.<*> (x Lude..:? "LocalSecondaryIndexes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Table")
            Lude.<*> (x Lude..:? "TableName")
      )
