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
-- Module      : Network.AWS.DMS.Types.TableStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.TableStatistics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides a collection of table statistics in response to a request by
-- the @DescribeTableStatistics@ operation.
--
-- /See:/ 'newTableStatistics' smart constructor.
data TableStatistics = TableStatistics'
  { -- | The number of rows that failed conditional checks during the full load
    -- operation (valid only for migrations where DynamoDB is the target).
    fullLoadCondtnlChkFailedRows :: Core.Maybe Core.Integer,
    -- | The number of rows added during the full load operation.
    fullLoadRows :: Core.Maybe Core.Integer,
    -- | The number of rows that failed to load during the full load operation
    -- (valid only for migrations where DynamoDB is the target).
    fullLoadErrorRows :: Core.Maybe Core.Integer,
    -- | The name of the table.
    tableName :: Core.Maybe Core.Text,
    -- | The state of the tables described.
    --
    -- Valid states: Table does not exist | Before load | Full load | Table
    -- completed | Table cancelled | Table error | Table all | Table updates |
    -- Table is being reloaded
    tableState :: Core.Maybe Core.Text,
    -- | The last time a table was updated.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | The number of records that failed validation.
    validationFailedRecords :: Core.Maybe Core.Integer,
    -- | The time when the full load operation started.
    fullLoadStartTime :: Core.Maybe Core.POSIX,
    -- | The number of update actions performed on a table.
    updates :: Core.Maybe Core.Integer,
    -- | The number of delete actions performed on a table.
    deletes :: Core.Maybe Core.Integer,
    -- | The data definition language (DDL) used to build and modify the
    -- structure of your tables.
    ddls :: Core.Maybe Core.Integer,
    -- | The time when the full load operation completed.
    fullLoadEndTime :: Core.Maybe Core.POSIX,
    -- | The validation state of the table.
    --
    -- This parameter can have the following values:
    --
    -- -   Not enabled – Validation isn\'t enabled for the table in the
    --     migration task.
    --
    -- -   Pending records – Some records in the table are waiting for
    --     validation.
    --
    -- -   Mismatched records – Some records in the table don\'t match between
    --     the source and target.
    --
    -- -   Suspended records – Some records in the table couldn\'t be
    --     validated.
    --
    -- -   No primary key –The table couldn\'t be validated because it has no
    --     primary key.
    --
    -- -   Table error – The table wasn\'t validated because it\'s in an error
    --     state and some data wasn\'t migrated.
    --
    -- -   Validated – All rows in the table are validated. If the table is
    --     updated, the status can change from Validated.
    --
    -- -   Error – The table couldn\'t be validated because of an unexpected
    --     error.
    --
    -- -   Pending validation – The table is waiting validation.
    --
    -- -   Preparing table – Preparing the table enabled in the migration task
    --     for validation.
    --
    -- -   Pending revalidation – All rows in the table are pending validation
    --     after the table was updated.
    validationState :: Core.Maybe Core.Text,
    -- | The number of insert actions performed on a table.
    inserts :: Core.Maybe Core.Integer,
    -- | The number of records that couldn\'t be validated.
    validationSuspendedRecords :: Core.Maybe Core.Integer,
    -- | The schema name.
    schemaName :: Core.Maybe Core.Text,
    -- | Additional details about the state of validation.
    validationStateDetails :: Core.Maybe Core.Text,
    -- | A value that indicates if the table was reloaded (@true@) or loaded as
    -- part of a new full load operation (@false@).
    fullLoadReloaded :: Core.Maybe Core.Bool,
    -- | The number of records that have yet to be validated.
    validationPendingRecords :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TableStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fullLoadCondtnlChkFailedRows', 'tableStatistics_fullLoadCondtnlChkFailedRows' - The number of rows that failed conditional checks during the full load
-- operation (valid only for migrations where DynamoDB is the target).
--
-- 'fullLoadRows', 'tableStatistics_fullLoadRows' - The number of rows added during the full load operation.
--
-- 'fullLoadErrorRows', 'tableStatistics_fullLoadErrorRows' - The number of rows that failed to load during the full load operation
-- (valid only for migrations where DynamoDB is the target).
--
-- 'tableName', 'tableStatistics_tableName' - The name of the table.
--
-- 'tableState', 'tableStatistics_tableState' - The state of the tables described.
--
-- Valid states: Table does not exist | Before load | Full load | Table
-- completed | Table cancelled | Table error | Table all | Table updates |
-- Table is being reloaded
--
-- 'lastUpdateTime', 'tableStatistics_lastUpdateTime' - The last time a table was updated.
--
-- 'validationFailedRecords', 'tableStatistics_validationFailedRecords' - The number of records that failed validation.
--
-- 'fullLoadStartTime', 'tableStatistics_fullLoadStartTime' - The time when the full load operation started.
--
-- 'updates', 'tableStatistics_updates' - The number of update actions performed on a table.
--
-- 'deletes', 'tableStatistics_deletes' - The number of delete actions performed on a table.
--
-- 'ddls', 'tableStatistics_ddls' - The data definition language (DDL) used to build and modify the
-- structure of your tables.
--
-- 'fullLoadEndTime', 'tableStatistics_fullLoadEndTime' - The time when the full load operation completed.
--
-- 'validationState', 'tableStatistics_validationState' - The validation state of the table.
--
-- This parameter can have the following values:
--
-- -   Not enabled – Validation isn\'t enabled for the table in the
--     migration task.
--
-- -   Pending records – Some records in the table are waiting for
--     validation.
--
-- -   Mismatched records – Some records in the table don\'t match between
--     the source and target.
--
-- -   Suspended records – Some records in the table couldn\'t be
--     validated.
--
-- -   No primary key –The table couldn\'t be validated because it has no
--     primary key.
--
-- -   Table error – The table wasn\'t validated because it\'s in an error
--     state and some data wasn\'t migrated.
--
-- -   Validated – All rows in the table are validated. If the table is
--     updated, the status can change from Validated.
--
-- -   Error – The table couldn\'t be validated because of an unexpected
--     error.
--
-- -   Pending validation – The table is waiting validation.
--
-- -   Preparing table – Preparing the table enabled in the migration task
--     for validation.
--
-- -   Pending revalidation – All rows in the table are pending validation
--     after the table was updated.
--
-- 'inserts', 'tableStatistics_inserts' - The number of insert actions performed on a table.
--
-- 'validationSuspendedRecords', 'tableStatistics_validationSuspendedRecords' - The number of records that couldn\'t be validated.
--
-- 'schemaName', 'tableStatistics_schemaName' - The schema name.
--
-- 'validationStateDetails', 'tableStatistics_validationStateDetails' - Additional details about the state of validation.
--
-- 'fullLoadReloaded', 'tableStatistics_fullLoadReloaded' - A value that indicates if the table was reloaded (@true@) or loaded as
-- part of a new full load operation (@false@).
--
-- 'validationPendingRecords', 'tableStatistics_validationPendingRecords' - The number of records that have yet to be validated.
newTableStatistics ::
  TableStatistics
newTableStatistics =
  TableStatistics'
    { fullLoadCondtnlChkFailedRows =
        Core.Nothing,
      fullLoadRows = Core.Nothing,
      fullLoadErrorRows = Core.Nothing,
      tableName = Core.Nothing,
      tableState = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      validationFailedRecords = Core.Nothing,
      fullLoadStartTime = Core.Nothing,
      updates = Core.Nothing,
      deletes = Core.Nothing,
      ddls = Core.Nothing,
      fullLoadEndTime = Core.Nothing,
      validationState = Core.Nothing,
      inserts = Core.Nothing,
      validationSuspendedRecords = Core.Nothing,
      schemaName = Core.Nothing,
      validationStateDetails = Core.Nothing,
      fullLoadReloaded = Core.Nothing,
      validationPendingRecords = Core.Nothing
    }

-- | The number of rows that failed conditional checks during the full load
-- operation (valid only for migrations where DynamoDB is the target).
tableStatistics_fullLoadCondtnlChkFailedRows :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tableStatistics_fullLoadCondtnlChkFailedRows = Lens.lens (\TableStatistics' {fullLoadCondtnlChkFailedRows} -> fullLoadCondtnlChkFailedRows) (\s@TableStatistics' {} a -> s {fullLoadCondtnlChkFailedRows = a} :: TableStatistics)

-- | The number of rows added during the full load operation.
tableStatistics_fullLoadRows :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tableStatistics_fullLoadRows = Lens.lens (\TableStatistics' {fullLoadRows} -> fullLoadRows) (\s@TableStatistics' {} a -> s {fullLoadRows = a} :: TableStatistics)

-- | The number of rows that failed to load during the full load operation
-- (valid only for migrations where DynamoDB is the target).
tableStatistics_fullLoadErrorRows :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tableStatistics_fullLoadErrorRows = Lens.lens (\TableStatistics' {fullLoadErrorRows} -> fullLoadErrorRows) (\s@TableStatistics' {} a -> s {fullLoadErrorRows = a} :: TableStatistics)

-- | The name of the table.
tableStatistics_tableName :: Lens.Lens' TableStatistics (Core.Maybe Core.Text)
tableStatistics_tableName = Lens.lens (\TableStatistics' {tableName} -> tableName) (\s@TableStatistics' {} a -> s {tableName = a} :: TableStatistics)

-- | The state of the tables described.
--
-- Valid states: Table does not exist | Before load | Full load | Table
-- completed | Table cancelled | Table error | Table all | Table updates |
-- Table is being reloaded
tableStatistics_tableState :: Lens.Lens' TableStatistics (Core.Maybe Core.Text)
tableStatistics_tableState = Lens.lens (\TableStatistics' {tableState} -> tableState) (\s@TableStatistics' {} a -> s {tableState = a} :: TableStatistics)

-- | The last time a table was updated.
tableStatistics_lastUpdateTime :: Lens.Lens' TableStatistics (Core.Maybe Core.UTCTime)
tableStatistics_lastUpdateTime = Lens.lens (\TableStatistics' {lastUpdateTime} -> lastUpdateTime) (\s@TableStatistics' {} a -> s {lastUpdateTime = a} :: TableStatistics) Core.. Lens.mapping Core._Time

-- | The number of records that failed validation.
tableStatistics_validationFailedRecords :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tableStatistics_validationFailedRecords = Lens.lens (\TableStatistics' {validationFailedRecords} -> validationFailedRecords) (\s@TableStatistics' {} a -> s {validationFailedRecords = a} :: TableStatistics)

-- | The time when the full load operation started.
tableStatistics_fullLoadStartTime :: Lens.Lens' TableStatistics (Core.Maybe Core.UTCTime)
tableStatistics_fullLoadStartTime = Lens.lens (\TableStatistics' {fullLoadStartTime} -> fullLoadStartTime) (\s@TableStatistics' {} a -> s {fullLoadStartTime = a} :: TableStatistics) Core.. Lens.mapping Core._Time

-- | The number of update actions performed on a table.
tableStatistics_updates :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tableStatistics_updates = Lens.lens (\TableStatistics' {updates} -> updates) (\s@TableStatistics' {} a -> s {updates = a} :: TableStatistics)

-- | The number of delete actions performed on a table.
tableStatistics_deletes :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tableStatistics_deletes = Lens.lens (\TableStatistics' {deletes} -> deletes) (\s@TableStatistics' {} a -> s {deletes = a} :: TableStatistics)

-- | The data definition language (DDL) used to build and modify the
-- structure of your tables.
tableStatistics_ddls :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tableStatistics_ddls = Lens.lens (\TableStatistics' {ddls} -> ddls) (\s@TableStatistics' {} a -> s {ddls = a} :: TableStatistics)

-- | The time when the full load operation completed.
tableStatistics_fullLoadEndTime :: Lens.Lens' TableStatistics (Core.Maybe Core.UTCTime)
tableStatistics_fullLoadEndTime = Lens.lens (\TableStatistics' {fullLoadEndTime} -> fullLoadEndTime) (\s@TableStatistics' {} a -> s {fullLoadEndTime = a} :: TableStatistics) Core.. Lens.mapping Core._Time

-- | The validation state of the table.
--
-- This parameter can have the following values:
--
-- -   Not enabled – Validation isn\'t enabled for the table in the
--     migration task.
--
-- -   Pending records – Some records in the table are waiting for
--     validation.
--
-- -   Mismatched records – Some records in the table don\'t match between
--     the source and target.
--
-- -   Suspended records – Some records in the table couldn\'t be
--     validated.
--
-- -   No primary key –The table couldn\'t be validated because it has no
--     primary key.
--
-- -   Table error – The table wasn\'t validated because it\'s in an error
--     state and some data wasn\'t migrated.
--
-- -   Validated – All rows in the table are validated. If the table is
--     updated, the status can change from Validated.
--
-- -   Error – The table couldn\'t be validated because of an unexpected
--     error.
--
-- -   Pending validation – The table is waiting validation.
--
-- -   Preparing table – Preparing the table enabled in the migration task
--     for validation.
--
-- -   Pending revalidation – All rows in the table are pending validation
--     after the table was updated.
tableStatistics_validationState :: Lens.Lens' TableStatistics (Core.Maybe Core.Text)
tableStatistics_validationState = Lens.lens (\TableStatistics' {validationState} -> validationState) (\s@TableStatistics' {} a -> s {validationState = a} :: TableStatistics)

-- | The number of insert actions performed on a table.
tableStatistics_inserts :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tableStatistics_inserts = Lens.lens (\TableStatistics' {inserts} -> inserts) (\s@TableStatistics' {} a -> s {inserts = a} :: TableStatistics)

-- | The number of records that couldn\'t be validated.
tableStatistics_validationSuspendedRecords :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tableStatistics_validationSuspendedRecords = Lens.lens (\TableStatistics' {validationSuspendedRecords} -> validationSuspendedRecords) (\s@TableStatistics' {} a -> s {validationSuspendedRecords = a} :: TableStatistics)

-- | The schema name.
tableStatistics_schemaName :: Lens.Lens' TableStatistics (Core.Maybe Core.Text)
tableStatistics_schemaName = Lens.lens (\TableStatistics' {schemaName} -> schemaName) (\s@TableStatistics' {} a -> s {schemaName = a} :: TableStatistics)

-- | Additional details about the state of validation.
tableStatistics_validationStateDetails :: Lens.Lens' TableStatistics (Core.Maybe Core.Text)
tableStatistics_validationStateDetails = Lens.lens (\TableStatistics' {validationStateDetails} -> validationStateDetails) (\s@TableStatistics' {} a -> s {validationStateDetails = a} :: TableStatistics)

-- | A value that indicates if the table was reloaded (@true@) or loaded as
-- part of a new full load operation (@false@).
tableStatistics_fullLoadReloaded :: Lens.Lens' TableStatistics (Core.Maybe Core.Bool)
tableStatistics_fullLoadReloaded = Lens.lens (\TableStatistics' {fullLoadReloaded} -> fullLoadReloaded) (\s@TableStatistics' {} a -> s {fullLoadReloaded = a} :: TableStatistics)

-- | The number of records that have yet to be validated.
tableStatistics_validationPendingRecords :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tableStatistics_validationPendingRecords = Lens.lens (\TableStatistics' {validationPendingRecords} -> validationPendingRecords) (\s@TableStatistics' {} a -> s {validationPendingRecords = a} :: TableStatistics)

instance Core.FromJSON TableStatistics where
  parseJSON =
    Core.withObject
      "TableStatistics"
      ( \x ->
          TableStatistics'
            Core.<$> (x Core..:? "FullLoadCondtnlChkFailedRows")
            Core.<*> (x Core..:? "FullLoadRows")
            Core.<*> (x Core..:? "FullLoadErrorRows")
            Core.<*> (x Core..:? "TableName")
            Core.<*> (x Core..:? "TableState")
            Core.<*> (x Core..:? "LastUpdateTime")
            Core.<*> (x Core..:? "ValidationFailedRecords")
            Core.<*> (x Core..:? "FullLoadStartTime")
            Core.<*> (x Core..:? "Updates")
            Core.<*> (x Core..:? "Deletes")
            Core.<*> (x Core..:? "Ddls")
            Core.<*> (x Core..:? "FullLoadEndTime")
            Core.<*> (x Core..:? "ValidationState")
            Core.<*> (x Core..:? "Inserts")
            Core.<*> (x Core..:? "ValidationSuspendedRecords")
            Core.<*> (x Core..:? "SchemaName")
            Core.<*> (x Core..:? "ValidationStateDetails")
            Core.<*> (x Core..:? "FullLoadReloaded")
            Core.<*> (x Core..:? "ValidationPendingRecords")
      )

instance Core.Hashable TableStatistics

instance Core.NFData TableStatistics
