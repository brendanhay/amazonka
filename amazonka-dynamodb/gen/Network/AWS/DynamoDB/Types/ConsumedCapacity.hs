{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DynamoDB.Types.Capacity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    localSecondaryIndexes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Capacity),
    -- | The amount of throughput consumed on each global index affected by the
    -- operation.
    globalSecondaryIndexes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Capacity),
    -- | The name of the table that was affected by the operation.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The total number of write capacity units consumed by the operation.
    writeCapacityUnits :: Prelude.Maybe Prelude.Double,
    -- | The total number of capacity units consumed by the operation.
    capacityUnits :: Prelude.Maybe Prelude.Double,
    -- | The amount of throughput consumed on the table affected by the
    -- operation.
    table :: Prelude.Maybe Capacity,
    -- | The total number of read capacity units consumed by the operation.
    readCapacityUnits :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      tableName = Prelude.Nothing,
      writeCapacityUnits = Prelude.Nothing,
      capacityUnits = Prelude.Nothing,
      table = Prelude.Nothing,
      readCapacityUnits = Prelude.Nothing
    }

-- | The amount of throughput consumed on each local index affected by the
-- operation.
consumedCapacity_localSecondaryIndexes :: Lens.Lens' ConsumedCapacity (Prelude.Maybe (Prelude.HashMap Prelude.Text Capacity))
consumedCapacity_localSecondaryIndexes = Lens.lens (\ConsumedCapacity' {localSecondaryIndexes} -> localSecondaryIndexes) (\s@ConsumedCapacity' {} a -> s {localSecondaryIndexes = a} :: ConsumedCapacity) Prelude.. Lens.mapping Prelude._Coerce

-- | The amount of throughput consumed on each global index affected by the
-- operation.
consumedCapacity_globalSecondaryIndexes :: Lens.Lens' ConsumedCapacity (Prelude.Maybe (Prelude.HashMap Prelude.Text Capacity))
consumedCapacity_globalSecondaryIndexes = Lens.lens (\ConsumedCapacity' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@ConsumedCapacity' {} a -> s {globalSecondaryIndexes = a} :: ConsumedCapacity) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the table that was affected by the operation.
consumedCapacity_tableName :: Lens.Lens' ConsumedCapacity (Prelude.Maybe Prelude.Text)
consumedCapacity_tableName = Lens.lens (\ConsumedCapacity' {tableName} -> tableName) (\s@ConsumedCapacity' {} a -> s {tableName = a} :: ConsumedCapacity)

-- | The total number of write capacity units consumed by the operation.
consumedCapacity_writeCapacityUnits :: Lens.Lens' ConsumedCapacity (Prelude.Maybe Prelude.Double)
consumedCapacity_writeCapacityUnits = Lens.lens (\ConsumedCapacity' {writeCapacityUnits} -> writeCapacityUnits) (\s@ConsumedCapacity' {} a -> s {writeCapacityUnits = a} :: ConsumedCapacity)

-- | The total number of capacity units consumed by the operation.
consumedCapacity_capacityUnits :: Lens.Lens' ConsumedCapacity (Prelude.Maybe Prelude.Double)
consumedCapacity_capacityUnits = Lens.lens (\ConsumedCapacity' {capacityUnits} -> capacityUnits) (\s@ConsumedCapacity' {} a -> s {capacityUnits = a} :: ConsumedCapacity)

-- | The amount of throughput consumed on the table affected by the
-- operation.
consumedCapacity_table :: Lens.Lens' ConsumedCapacity (Prelude.Maybe Capacity)
consumedCapacity_table = Lens.lens (\ConsumedCapacity' {table} -> table) (\s@ConsumedCapacity' {} a -> s {table = a} :: ConsumedCapacity)

-- | The total number of read capacity units consumed by the operation.
consumedCapacity_readCapacityUnits :: Lens.Lens' ConsumedCapacity (Prelude.Maybe Prelude.Double)
consumedCapacity_readCapacityUnits = Lens.lens (\ConsumedCapacity' {readCapacityUnits} -> readCapacityUnits) (\s@ConsumedCapacity' {} a -> s {readCapacityUnits = a} :: ConsumedCapacity)

instance Prelude.FromJSON ConsumedCapacity where
  parseJSON =
    Prelude.withObject
      "ConsumedCapacity"
      ( \x ->
          ConsumedCapacity'
            Prelude.<$> ( x Prelude..:? "LocalSecondaryIndexes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "GlobalSecondaryIndexes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "TableName")
            Prelude.<*> (x Prelude..:? "WriteCapacityUnits")
            Prelude.<*> (x Prelude..:? "CapacityUnits")
            Prelude.<*> (x Prelude..:? "Table")
            Prelude.<*> (x Prelude..:? "ReadCapacityUnits")
      )

instance Prelude.Hashable ConsumedCapacity

instance Prelude.NFData ConsumedCapacity
