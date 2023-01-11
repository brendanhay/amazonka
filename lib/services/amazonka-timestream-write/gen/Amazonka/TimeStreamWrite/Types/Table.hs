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
-- Module      : Amazonka.TimeStreamWrite.Types.Table
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.Table where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.MagneticStoreWriteProperties
import Amazonka.TimeStreamWrite.Types.RetentionProperties
import Amazonka.TimeStreamWrite.Types.TableStatus

-- | Table represents a database table in Timestream. Tables contain one or
-- more related time series. You can modify the retention duration of the
-- memory store and the magnetic store for a table.
--
-- /See:/ 'newTable' smart constructor.
data Table = Table'
  { -- | The Amazon Resource Name that uniquely identifies this table.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time when the Timestream table was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the Timestream database that contains this table.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The time when the Timestream table was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | Contains properties to set on the table when enabling magnetic store
    -- writes.
    magneticStoreWriteProperties :: Prelude.Maybe MagneticStoreWriteProperties,
    -- | The retention duration for the memory store and magnetic store.
    retentionProperties :: Prelude.Maybe RetentionProperties,
    -- | The name of the Timestream table.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The current state of the table:
    --
    -- -   @DELETING@ - The table is being deleted.
    --
    -- -   @ACTIVE@ - The table is ready for use.
    tableStatus :: Prelude.Maybe TableStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Table' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'table_arn' - The Amazon Resource Name that uniquely identifies this table.
--
-- 'creationTime', 'table_creationTime' - The time when the Timestream table was created.
--
-- 'databaseName', 'table_databaseName' - The name of the Timestream database that contains this table.
--
-- 'lastUpdatedTime', 'table_lastUpdatedTime' - The time when the Timestream table was last updated.
--
-- 'magneticStoreWriteProperties', 'table_magneticStoreWriteProperties' - Contains properties to set on the table when enabling magnetic store
-- writes.
--
-- 'retentionProperties', 'table_retentionProperties' - The retention duration for the memory store and magnetic store.
--
-- 'tableName', 'table_tableName' - The name of the Timestream table.
--
-- 'tableStatus', 'table_tableStatus' - The current state of the table:
--
-- -   @DELETING@ - The table is being deleted.
--
-- -   @ACTIVE@ - The table is ready for use.
newTable ::
  Table
newTable =
  Table'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      magneticStoreWriteProperties = Prelude.Nothing,
      retentionProperties = Prelude.Nothing,
      tableName = Prelude.Nothing,
      tableStatus = Prelude.Nothing
    }

-- | The Amazon Resource Name that uniquely identifies this table.
table_arn :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_arn = Lens.lens (\Table' {arn} -> arn) (\s@Table' {} a -> s {arn = a} :: Table)

-- | The time when the Timestream table was created.
table_creationTime :: Lens.Lens' Table (Prelude.Maybe Prelude.UTCTime)
table_creationTime = Lens.lens (\Table' {creationTime} -> creationTime) (\s@Table' {} a -> s {creationTime = a} :: Table) Prelude.. Lens.mapping Data._Time

-- | The name of the Timestream database that contains this table.
table_databaseName :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_databaseName = Lens.lens (\Table' {databaseName} -> databaseName) (\s@Table' {} a -> s {databaseName = a} :: Table)

-- | The time when the Timestream table was last updated.
table_lastUpdatedTime :: Lens.Lens' Table (Prelude.Maybe Prelude.UTCTime)
table_lastUpdatedTime = Lens.lens (\Table' {lastUpdatedTime} -> lastUpdatedTime) (\s@Table' {} a -> s {lastUpdatedTime = a} :: Table) Prelude.. Lens.mapping Data._Time

-- | Contains properties to set on the table when enabling magnetic store
-- writes.
table_magneticStoreWriteProperties :: Lens.Lens' Table (Prelude.Maybe MagneticStoreWriteProperties)
table_magneticStoreWriteProperties = Lens.lens (\Table' {magneticStoreWriteProperties} -> magneticStoreWriteProperties) (\s@Table' {} a -> s {magneticStoreWriteProperties = a} :: Table)

-- | The retention duration for the memory store and magnetic store.
table_retentionProperties :: Lens.Lens' Table (Prelude.Maybe RetentionProperties)
table_retentionProperties = Lens.lens (\Table' {retentionProperties} -> retentionProperties) (\s@Table' {} a -> s {retentionProperties = a} :: Table)

-- | The name of the Timestream table.
table_tableName :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_tableName = Lens.lens (\Table' {tableName} -> tableName) (\s@Table' {} a -> s {tableName = a} :: Table)

-- | The current state of the table:
--
-- -   @DELETING@ - The table is being deleted.
--
-- -   @ACTIVE@ - The table is ready for use.
table_tableStatus :: Lens.Lens' Table (Prelude.Maybe TableStatus)
table_tableStatus = Lens.lens (\Table' {tableStatus} -> tableStatus) (\s@Table' {} a -> s {tableStatus = a} :: Table)

instance Data.FromJSON Table where
  parseJSON =
    Data.withObject
      "Table"
      ( \x ->
          Table'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "MagneticStoreWriteProperties")
            Prelude.<*> (x Data..:? "RetentionProperties")
            Prelude.<*> (x Data..:? "TableName")
            Prelude.<*> (x Data..:? "TableStatus")
      )

instance Prelude.Hashable Table where
  hashWithSalt _salt Table' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` magneticStoreWriteProperties
      `Prelude.hashWithSalt` retentionProperties
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` tableStatus

instance Prelude.NFData Table where
  rnf Table' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf magneticStoreWriteProperties
      `Prelude.seq` Prelude.rnf retentionProperties
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf tableStatus
