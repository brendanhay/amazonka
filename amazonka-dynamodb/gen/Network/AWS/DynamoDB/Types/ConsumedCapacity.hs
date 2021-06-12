{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ConsumedCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ConsumedCapacity where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.Capacity
import qualified Network.AWS.Lens as Lens

-- | The capacity units consumed by an operation. The data returned includes
-- the total provisioned throughput consumed, along with statistics for the
-- table and any indexes involved in the operation. @ConsumedCapacity@ is
-- only returned if the request asked for it. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /See:/ 'newConsumedCapacity' smart constructor.
data ConsumedCapacity = ConsumedCapacity'
  { -- | The amount of throughput consumed on each local index affected by the
    -- operation.
    localSecondaryIndexes :: Core.Maybe (Core.HashMap Core.Text Capacity),
    -- | The amount of throughput consumed on each global index affected by the
    -- operation.
    globalSecondaryIndexes :: Core.Maybe (Core.HashMap Core.Text Capacity),
    -- | The name of the table that was affected by the operation.
    tableName :: Core.Maybe Core.Text,
    -- | The total number of write capacity units consumed by the operation.
    writeCapacityUnits :: Core.Maybe Core.Double,
    -- | The total number of capacity units consumed by the operation.
    capacityUnits :: Core.Maybe Core.Double,
    -- | The amount of throughput consumed on the table affected by the
    -- operation.
    table :: Core.Maybe Capacity,
    -- | The total number of read capacity units consumed by the operation.
    readCapacityUnits :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConsumedCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localSecondaryIndexes', 'consumedCapacity_localSecondaryIndexes' - The amount of throughput consumed on each local index affected by the
-- operation.
--
-- 'globalSecondaryIndexes', 'consumedCapacity_globalSecondaryIndexes' - The amount of throughput consumed on each global index affected by the
-- operation.
--
-- 'tableName', 'consumedCapacity_tableName' - The name of the table that was affected by the operation.
--
-- 'writeCapacityUnits', 'consumedCapacity_writeCapacityUnits' - The total number of write capacity units consumed by the operation.
--
-- 'capacityUnits', 'consumedCapacity_capacityUnits' - The total number of capacity units consumed by the operation.
--
-- 'table', 'consumedCapacity_table' - The amount of throughput consumed on the table affected by the
-- operation.
--
-- 'readCapacityUnits', 'consumedCapacity_readCapacityUnits' - The total number of read capacity units consumed by the operation.
newConsumedCapacity ::
  ConsumedCapacity
newConsumedCapacity =
  ConsumedCapacity'
    { localSecondaryIndexes =
        Core.Nothing,
      globalSecondaryIndexes = Core.Nothing,
      tableName = Core.Nothing,
      writeCapacityUnits = Core.Nothing,
      capacityUnits = Core.Nothing,
      table = Core.Nothing,
      readCapacityUnits = Core.Nothing
    }

-- | The amount of throughput consumed on each local index affected by the
-- operation.
consumedCapacity_localSecondaryIndexes :: Lens.Lens' ConsumedCapacity (Core.Maybe (Core.HashMap Core.Text Capacity))
consumedCapacity_localSecondaryIndexes = Lens.lens (\ConsumedCapacity' {localSecondaryIndexes} -> localSecondaryIndexes) (\s@ConsumedCapacity' {} a -> s {localSecondaryIndexes = a} :: ConsumedCapacity) Core.. Lens.mapping Lens._Coerce

-- | The amount of throughput consumed on each global index affected by the
-- operation.
consumedCapacity_globalSecondaryIndexes :: Lens.Lens' ConsumedCapacity (Core.Maybe (Core.HashMap Core.Text Capacity))
consumedCapacity_globalSecondaryIndexes = Lens.lens (\ConsumedCapacity' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@ConsumedCapacity' {} a -> s {globalSecondaryIndexes = a} :: ConsumedCapacity) Core.. Lens.mapping Lens._Coerce

-- | The name of the table that was affected by the operation.
consumedCapacity_tableName :: Lens.Lens' ConsumedCapacity (Core.Maybe Core.Text)
consumedCapacity_tableName = Lens.lens (\ConsumedCapacity' {tableName} -> tableName) (\s@ConsumedCapacity' {} a -> s {tableName = a} :: ConsumedCapacity)

-- | The total number of write capacity units consumed by the operation.
consumedCapacity_writeCapacityUnits :: Lens.Lens' ConsumedCapacity (Core.Maybe Core.Double)
consumedCapacity_writeCapacityUnits = Lens.lens (\ConsumedCapacity' {writeCapacityUnits} -> writeCapacityUnits) (\s@ConsumedCapacity' {} a -> s {writeCapacityUnits = a} :: ConsumedCapacity)

-- | The total number of capacity units consumed by the operation.
consumedCapacity_capacityUnits :: Lens.Lens' ConsumedCapacity (Core.Maybe Core.Double)
consumedCapacity_capacityUnits = Lens.lens (\ConsumedCapacity' {capacityUnits} -> capacityUnits) (\s@ConsumedCapacity' {} a -> s {capacityUnits = a} :: ConsumedCapacity)

-- | The amount of throughput consumed on the table affected by the
-- operation.
consumedCapacity_table :: Lens.Lens' ConsumedCapacity (Core.Maybe Capacity)
consumedCapacity_table = Lens.lens (\ConsumedCapacity' {table} -> table) (\s@ConsumedCapacity' {} a -> s {table = a} :: ConsumedCapacity)

-- | The total number of read capacity units consumed by the operation.
consumedCapacity_readCapacityUnits :: Lens.Lens' ConsumedCapacity (Core.Maybe Core.Double)
consumedCapacity_readCapacityUnits = Lens.lens (\ConsumedCapacity' {readCapacityUnits} -> readCapacityUnits) (\s@ConsumedCapacity' {} a -> s {readCapacityUnits = a} :: ConsumedCapacity)

instance Core.FromJSON ConsumedCapacity where
  parseJSON =
    Core.withObject
      "ConsumedCapacity"
      ( \x ->
          ConsumedCapacity'
            Core.<$> ( x Core..:? "LocalSecondaryIndexes"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "GlobalSecondaryIndexes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "TableName")
            Core.<*> (x Core..:? "WriteCapacityUnits")
            Core.<*> (x Core..:? "CapacityUnits")
            Core.<*> (x Core..:? "Table")
            Core.<*> (x Core..:? "ReadCapacityUnits")
      )

instance Core.Hashable ConsumedCapacity

instance Core.NFData ConsumedCapacity
