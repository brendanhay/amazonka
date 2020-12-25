{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.TableStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.TableStatistics
  ( TableStatistics (..),

    -- * Smart constructor
    mkTableStatistics,

    -- * Lenses
    tsDdls,
    tsDeletes,
    tsFullLoadCondtnlChkFailedRows,
    tsFullLoadEndTime,
    tsFullLoadErrorRows,
    tsFullLoadReloaded,
    tsFullLoadRows,
    tsFullLoadStartTime,
    tsInserts,
    tsLastUpdateTime,
    tsSchemaName,
    tsTableName,
    tsTableState,
    tsUpdates,
    tsValidationFailedRecords,
    tsValidationPendingRecords,
    tsValidationState,
    tsValidationStateDetails,
    tsValidationSuspendedRecords,
  )
where

import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a collection of table statistics in response to a request by the @DescribeTableStatistics@ operation.
--
-- /See:/ 'mkTableStatistics' smart constructor.
data TableStatistics = TableStatistics'
  { -- | The data definition language (DDL) used to build and modify the structure of your tables.
    ddls :: Core.Maybe Core.Integer,
    -- | The number of delete actions performed on a table.
    deletes :: Core.Maybe Core.Integer,
    -- | The number of rows that failed conditional checks during the full load operation (valid only for migrations where DynamoDB is the target).
    fullLoadCondtnlChkFailedRows :: Core.Maybe Core.Integer,
    -- | The time when the full load operation completed.
    fullLoadEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The number of rows that failed to load during the full load operation (valid only for migrations where DynamoDB is the target).
    fullLoadErrorRows :: Core.Maybe Core.Integer,
    -- | A value that indicates if the table was reloaded (@true@ ) or loaded as part of a new full load operation (@false@ ).
    fullLoadReloaded :: Core.Maybe Core.Bool,
    -- | The number of rows added during the full load operation.
    fullLoadRows :: Core.Maybe Core.Integer,
    -- | The time when the full load operation started.
    fullLoadStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The number of insert actions performed on a table.
    inserts :: Core.Maybe Core.Integer,
    -- | The last time a table was updated.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The schema name.
    schemaName :: Core.Maybe Types.String,
    -- | The name of the table.
    tableName :: Core.Maybe Types.String,
    -- | The state of the tables described.
    --
    -- Valid states: Table does not exist | Before load | Full load | Table completed | Table cancelled | Table error | Table all | Table updates | Table is being reloaded
    tableState :: Core.Maybe Types.String,
    -- | The number of update actions performed on a table.
    updates :: Core.Maybe Core.Integer,
    -- | The number of records that failed validation.
    validationFailedRecords :: Core.Maybe Core.Integer,
    -- | The number of records that have yet to be validated.
    validationPendingRecords :: Core.Maybe Core.Integer,
    -- | The validation state of the table.
    --
    -- This parameter can have the following values:
    --
    --     * Not enabled – Validation isn't enabled for the table in the migration task.
    --
    --
    --     * Pending records – Some records in the table are waiting for validation.
    --
    --
    --     * Mismatched records – Some records in the table don't match between the source and target.
    --
    --
    --     * Suspended records – Some records in the table couldn't be validated.
    --
    --
    --     * No primary key –The table couldn't be validated because it has no primary key.
    --
    --
    --     * Table error – The table wasn't validated because it's in an error state and some data wasn't migrated.
    --
    --
    --     * Validated – All rows in the table are validated. If the table is updated, the status can change from Validated.
    --
    --
    --     * Error – The table couldn't be validated because of an unexpected error.
    --
    --
    --     * Pending validation – The table is waiting validation.
    --
    --
    --     * Preparing table – Preparing the table enabled in the migration task for validation.
    --
    --
    --     * Pending revalidation – All rows in the table are pending validation after the table was updated.
    validationState :: Core.Maybe Types.String,
    -- | Additional details about the state of validation.
    validationStateDetails :: Core.Maybe Types.String,
    -- | The number of records that couldn't be validated.
    validationSuspendedRecords :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TableStatistics' value with any optional fields omitted.
mkTableStatistics ::
  TableStatistics
mkTableStatistics =
  TableStatistics'
    { ddls = Core.Nothing,
      deletes = Core.Nothing,
      fullLoadCondtnlChkFailedRows = Core.Nothing,
      fullLoadEndTime = Core.Nothing,
      fullLoadErrorRows = Core.Nothing,
      fullLoadReloaded = Core.Nothing,
      fullLoadRows = Core.Nothing,
      fullLoadStartTime = Core.Nothing,
      inserts = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      schemaName = Core.Nothing,
      tableName = Core.Nothing,
      tableState = Core.Nothing,
      updates = Core.Nothing,
      validationFailedRecords = Core.Nothing,
      validationPendingRecords = Core.Nothing,
      validationState = Core.Nothing,
      validationStateDetails = Core.Nothing,
      validationSuspendedRecords = Core.Nothing
    }

-- | The data definition language (DDL) used to build and modify the structure of your tables.
--
-- /Note:/ Consider using 'ddls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDdls :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tsDdls = Lens.field @"ddls"
{-# DEPRECATED tsDdls "Use generic-lens or generic-optics with 'ddls' instead." #-}

-- | The number of delete actions performed on a table.
--
-- /Note:/ Consider using 'deletes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDeletes :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tsDeletes = Lens.field @"deletes"
{-# DEPRECATED tsDeletes "Use generic-lens or generic-optics with 'deletes' instead." #-}

-- | The number of rows that failed conditional checks during the full load operation (valid only for migrations where DynamoDB is the target).
--
-- /Note:/ Consider using 'fullLoadCondtnlChkFailedRows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadCondtnlChkFailedRows :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tsFullLoadCondtnlChkFailedRows = Lens.field @"fullLoadCondtnlChkFailedRows"
{-# DEPRECATED tsFullLoadCondtnlChkFailedRows "Use generic-lens or generic-optics with 'fullLoadCondtnlChkFailedRows' instead." #-}

-- | The time when the full load operation completed.
--
-- /Note:/ Consider using 'fullLoadEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadEndTime :: Lens.Lens' TableStatistics (Core.Maybe Core.NominalDiffTime)
tsFullLoadEndTime = Lens.field @"fullLoadEndTime"
{-# DEPRECATED tsFullLoadEndTime "Use generic-lens or generic-optics with 'fullLoadEndTime' instead." #-}

-- | The number of rows that failed to load during the full load operation (valid only for migrations where DynamoDB is the target).
--
-- /Note:/ Consider using 'fullLoadErrorRows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadErrorRows :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tsFullLoadErrorRows = Lens.field @"fullLoadErrorRows"
{-# DEPRECATED tsFullLoadErrorRows "Use generic-lens or generic-optics with 'fullLoadErrorRows' instead." #-}

-- | A value that indicates if the table was reloaded (@true@ ) or loaded as part of a new full load operation (@false@ ).
--
-- /Note:/ Consider using 'fullLoadReloaded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadReloaded :: Lens.Lens' TableStatistics (Core.Maybe Core.Bool)
tsFullLoadReloaded = Lens.field @"fullLoadReloaded"
{-# DEPRECATED tsFullLoadReloaded "Use generic-lens or generic-optics with 'fullLoadReloaded' instead." #-}

-- | The number of rows added during the full load operation.
--
-- /Note:/ Consider using 'fullLoadRows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadRows :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tsFullLoadRows = Lens.field @"fullLoadRows"
{-# DEPRECATED tsFullLoadRows "Use generic-lens or generic-optics with 'fullLoadRows' instead." #-}

-- | The time when the full load operation started.
--
-- /Note:/ Consider using 'fullLoadStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadStartTime :: Lens.Lens' TableStatistics (Core.Maybe Core.NominalDiffTime)
tsFullLoadStartTime = Lens.field @"fullLoadStartTime"
{-# DEPRECATED tsFullLoadStartTime "Use generic-lens or generic-optics with 'fullLoadStartTime' instead." #-}

-- | The number of insert actions performed on a table.
--
-- /Note:/ Consider using 'inserts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsInserts :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tsInserts = Lens.field @"inserts"
{-# DEPRECATED tsInserts "Use generic-lens or generic-optics with 'inserts' instead." #-}

-- | The last time a table was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLastUpdateTime :: Lens.Lens' TableStatistics (Core.Maybe Core.NominalDiffTime)
tsLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED tsLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The schema name.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsSchemaName :: Lens.Lens' TableStatistics (Core.Maybe Types.String)
tsSchemaName = Lens.field @"schemaName"
{-# DEPRECATED tsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTableName :: Lens.Lens' TableStatistics (Core.Maybe Types.String)
tsTableName = Lens.field @"tableName"
{-# DEPRECATED tsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The state of the tables described.
--
-- Valid states: Table does not exist | Before load | Full load | Table completed | Table cancelled | Table error | Table all | Table updates | Table is being reloaded
--
-- /Note:/ Consider using 'tableState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTableState :: Lens.Lens' TableStatistics (Core.Maybe Types.String)
tsTableState = Lens.field @"tableState"
{-# DEPRECATED tsTableState "Use generic-lens or generic-optics with 'tableState' instead." #-}

-- | The number of update actions performed on a table.
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsUpdates :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tsUpdates = Lens.field @"updates"
{-# DEPRECATED tsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The number of records that failed validation.
--
-- /Note:/ Consider using 'validationFailedRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsValidationFailedRecords :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tsValidationFailedRecords = Lens.field @"validationFailedRecords"
{-# DEPRECATED tsValidationFailedRecords "Use generic-lens or generic-optics with 'validationFailedRecords' instead." #-}

-- | The number of records that have yet to be validated.
--
-- /Note:/ Consider using 'validationPendingRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsValidationPendingRecords :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tsValidationPendingRecords = Lens.field @"validationPendingRecords"
{-# DEPRECATED tsValidationPendingRecords "Use generic-lens or generic-optics with 'validationPendingRecords' instead." #-}

-- | The validation state of the table.
--
-- This parameter can have the following values:
--
--     * Not enabled – Validation isn't enabled for the table in the migration task.
--
--
--     * Pending records – Some records in the table are waiting for validation.
--
--
--     * Mismatched records – Some records in the table don't match between the source and target.
--
--
--     * Suspended records – Some records in the table couldn't be validated.
--
--
--     * No primary key –The table couldn't be validated because it has no primary key.
--
--
--     * Table error – The table wasn't validated because it's in an error state and some data wasn't migrated.
--
--
--     * Validated – All rows in the table are validated. If the table is updated, the status can change from Validated.
--
--
--     * Error – The table couldn't be validated because of an unexpected error.
--
--
--     * Pending validation – The table is waiting validation.
--
--
--     * Preparing table – Preparing the table enabled in the migration task for validation.
--
--
--     * Pending revalidation – All rows in the table are pending validation after the table was updated.
--
--
--
-- /Note:/ Consider using 'validationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsValidationState :: Lens.Lens' TableStatistics (Core.Maybe Types.String)
tsValidationState = Lens.field @"validationState"
{-# DEPRECATED tsValidationState "Use generic-lens or generic-optics with 'validationState' instead." #-}

-- | Additional details about the state of validation.
--
-- /Note:/ Consider using 'validationStateDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsValidationStateDetails :: Lens.Lens' TableStatistics (Core.Maybe Types.String)
tsValidationStateDetails = Lens.field @"validationStateDetails"
{-# DEPRECATED tsValidationStateDetails "Use generic-lens or generic-optics with 'validationStateDetails' instead." #-}

-- | The number of records that couldn't be validated.
--
-- /Note:/ Consider using 'validationSuspendedRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsValidationSuspendedRecords :: Lens.Lens' TableStatistics (Core.Maybe Core.Integer)
tsValidationSuspendedRecords = Lens.field @"validationSuspendedRecords"
{-# DEPRECATED tsValidationSuspendedRecords "Use generic-lens or generic-optics with 'validationSuspendedRecords' instead." #-}

instance Core.FromJSON TableStatistics where
  parseJSON =
    Core.withObject "TableStatistics" Core.$
      \x ->
        TableStatistics'
          Core.<$> (x Core..:? "Ddls")
          Core.<*> (x Core..:? "Deletes")
          Core.<*> (x Core..:? "FullLoadCondtnlChkFailedRows")
          Core.<*> (x Core..:? "FullLoadEndTime")
          Core.<*> (x Core..:? "FullLoadErrorRows")
          Core.<*> (x Core..:? "FullLoadReloaded")
          Core.<*> (x Core..:? "FullLoadRows")
          Core.<*> (x Core..:? "FullLoadStartTime")
          Core.<*> (x Core..:? "Inserts")
          Core.<*> (x Core..:? "LastUpdateTime")
          Core.<*> (x Core..:? "SchemaName")
          Core.<*> (x Core..:? "TableName")
          Core.<*> (x Core..:? "TableState")
          Core.<*> (x Core..:? "Updates")
          Core.<*> (x Core..:? "ValidationFailedRecords")
          Core.<*> (x Core..:? "ValidationPendingRecords")
          Core.<*> (x Core..:? "ValidationState")
          Core.<*> (x Core..:? "ValidationStateDetails")
          Core.<*> (x Core..:? "ValidationSuspendedRecords")
