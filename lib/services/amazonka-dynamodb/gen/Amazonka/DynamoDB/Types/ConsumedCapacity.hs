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
-- Module      : Amazonka.DynamoDB.Types.ConsumedCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ConsumedCapacity where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Internal
import Amazonka.DynamoDB.Types.Capacity
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The capacity units consumed by an operation. The data returned includes
-- the total provisioned throughput consumed, along with statistics for the
-- table and any indexes involved in the operation. @ConsumedCapacity@ is
-- only returned if the request asked for it. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /See:/ 'newConsumedCapacity' smart constructor.
data ConsumedCapacity = ConsumedCapacity'
  { -- | The total number of read capacity units consumed by the operation.
    readCapacityUnits :: Prelude.Maybe Prelude.Double,
    -- | The amount of throughput consumed on each global index affected by the
    -- operation.
    globalSecondaryIndexes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Capacity),
    -- | The total number of capacity units consumed by the operation.
    capacityUnits :: Prelude.Maybe Prelude.Double,
    -- | The total number of write capacity units consumed by the operation.
    writeCapacityUnits :: Prelude.Maybe Prelude.Double,
    -- | The amount of throughput consumed on each local index affected by the
    -- operation.
    localSecondaryIndexes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Capacity),
    -- | The amount of throughput consumed on the table affected by the
    -- operation.
    table :: Prelude.Maybe Capacity,
    -- | The name of the table that was affected by the operation.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConsumedCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readCapacityUnits', 'consumedCapacity_readCapacityUnits' - The total number of read capacity units consumed by the operation.
--
-- 'globalSecondaryIndexes', 'consumedCapacity_globalSecondaryIndexes' - The amount of throughput consumed on each global index affected by the
-- operation.
--
-- 'capacityUnits', 'consumedCapacity_capacityUnits' - The total number of capacity units consumed by the operation.
--
-- 'writeCapacityUnits', 'consumedCapacity_writeCapacityUnits' - The total number of write capacity units consumed by the operation.
--
-- 'localSecondaryIndexes', 'consumedCapacity_localSecondaryIndexes' - The amount of throughput consumed on each local index affected by the
-- operation.
--
-- 'table', 'consumedCapacity_table' - The amount of throughput consumed on the table affected by the
-- operation.
--
-- 'tableName', 'consumedCapacity_tableName' - The name of the table that was affected by the operation.
newConsumedCapacity ::
  ConsumedCapacity
newConsumedCapacity =
  ConsumedCapacity'
    { readCapacityUnits =
        Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      capacityUnits = Prelude.Nothing,
      writeCapacityUnits = Prelude.Nothing,
      localSecondaryIndexes = Prelude.Nothing,
      table = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | The total number of read capacity units consumed by the operation.
consumedCapacity_readCapacityUnits :: Lens.Lens' ConsumedCapacity (Prelude.Maybe Prelude.Double)
consumedCapacity_readCapacityUnits = Lens.lens (\ConsumedCapacity' {readCapacityUnits} -> readCapacityUnits) (\s@ConsumedCapacity' {} a -> s {readCapacityUnits = a} :: ConsumedCapacity)

-- | The amount of throughput consumed on each global index affected by the
-- operation.
consumedCapacity_globalSecondaryIndexes :: Lens.Lens' ConsumedCapacity (Prelude.Maybe (Prelude.HashMap Prelude.Text Capacity))
consumedCapacity_globalSecondaryIndexes = Lens.lens (\ConsumedCapacity' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@ConsumedCapacity' {} a -> s {globalSecondaryIndexes = a} :: ConsumedCapacity) Prelude.. Lens.mapping Lens.coerced

-- | The total number of capacity units consumed by the operation.
consumedCapacity_capacityUnits :: Lens.Lens' ConsumedCapacity (Prelude.Maybe Prelude.Double)
consumedCapacity_capacityUnits = Lens.lens (\ConsumedCapacity' {capacityUnits} -> capacityUnits) (\s@ConsumedCapacity' {} a -> s {capacityUnits = a} :: ConsumedCapacity)

-- | The total number of write capacity units consumed by the operation.
consumedCapacity_writeCapacityUnits :: Lens.Lens' ConsumedCapacity (Prelude.Maybe Prelude.Double)
consumedCapacity_writeCapacityUnits = Lens.lens (\ConsumedCapacity' {writeCapacityUnits} -> writeCapacityUnits) (\s@ConsumedCapacity' {} a -> s {writeCapacityUnits = a} :: ConsumedCapacity)

-- | The amount of throughput consumed on each local index affected by the
-- operation.
consumedCapacity_localSecondaryIndexes :: Lens.Lens' ConsumedCapacity (Prelude.Maybe (Prelude.HashMap Prelude.Text Capacity))
consumedCapacity_localSecondaryIndexes = Lens.lens (\ConsumedCapacity' {localSecondaryIndexes} -> localSecondaryIndexes) (\s@ConsumedCapacity' {} a -> s {localSecondaryIndexes = a} :: ConsumedCapacity) Prelude.. Lens.mapping Lens.coerced

-- | The amount of throughput consumed on the table affected by the
-- operation.
consumedCapacity_table :: Lens.Lens' ConsumedCapacity (Prelude.Maybe Capacity)
consumedCapacity_table = Lens.lens (\ConsumedCapacity' {table} -> table) (\s@ConsumedCapacity' {} a -> s {table = a} :: ConsumedCapacity)

-- | The name of the table that was affected by the operation.
consumedCapacity_tableName :: Lens.Lens' ConsumedCapacity (Prelude.Maybe Prelude.Text)
consumedCapacity_tableName = Lens.lens (\ConsumedCapacity' {tableName} -> tableName) (\s@ConsumedCapacity' {} a -> s {tableName = a} :: ConsumedCapacity)

instance Core.FromJSON ConsumedCapacity where
  parseJSON =
    Core.withObject
      "ConsumedCapacity"
      ( \x ->
          ConsumedCapacity'
            Prelude.<$> (x Core..:? "ReadCapacityUnits")
            Prelude.<*> ( x Core..:? "GlobalSecondaryIndexes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CapacityUnits")
            Prelude.<*> (x Core..:? "WriteCapacityUnits")
            Prelude.<*> ( x Core..:? "LocalSecondaryIndexes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Table")
            Prelude.<*> (x Core..:? "TableName")
      )

instance Prelude.Hashable ConsumedCapacity where
  hashWithSalt _salt ConsumedCapacity' {..} =
    _salt `Prelude.hashWithSalt` readCapacityUnits
      `Prelude.hashWithSalt` globalSecondaryIndexes
      `Prelude.hashWithSalt` capacityUnits
      `Prelude.hashWithSalt` writeCapacityUnits
      `Prelude.hashWithSalt` localSecondaryIndexes
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData ConsumedCapacity where
  rnf ConsumedCapacity' {..} =
    Prelude.rnf readCapacityUnits
      `Prelude.seq` Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf capacityUnits
      `Prelude.seq` Prelude.rnf writeCapacityUnits
      `Prelude.seq` Prelude.rnf localSecondaryIndexes
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf tableName
