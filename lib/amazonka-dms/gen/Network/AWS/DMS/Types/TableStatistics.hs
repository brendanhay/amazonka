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
    tsValidationState,
    tsFullLoadRows,
    tsInserts,
    tsFullLoadEndTime,
    tsFullLoadCondtnlChkFailedRows,
    tsFullLoadReloaded,
    tsValidationFailedRecords,
    tsValidationSuspendedRecords,
    tsSchemaName,
    tsValidationStateDetails,
    tsTableState,
    tsFullLoadErrorRows,
    tsDdls,
    tsDeletes,
    tsUpdates,
    tsValidationPendingRecords,
    tsFullLoadStartTime,
    tsLastUpdateTime,
    tsTableName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a collection of table statistics in response to a request by the @DescribeTableStatistics@ operation.
--
-- /See:/ 'mkTableStatistics' smart constructor.
data TableStatistics = TableStatistics'
  { -- | The validation state of the table.
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
    validationState :: Lude.Maybe Lude.Text,
    -- | The number of rows added during the full load operation.
    fullLoadRows :: Lude.Maybe Lude.Integer,
    -- | The number of insert actions performed on a table.
    inserts :: Lude.Maybe Lude.Integer,
    -- | The time when the full load operation completed.
    fullLoadEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The number of rows that failed conditional checks during the full load operation (valid only for migrations where DynamoDB is the target).
    fullLoadCondtnlChkFailedRows :: Lude.Maybe Lude.Integer,
    -- | A value that indicates if the table was reloaded (@true@ ) or loaded as part of a new full load operation (@false@ ).
    fullLoadReloaded :: Lude.Maybe Lude.Bool,
    -- | The number of records that failed validation.
    validationFailedRecords :: Lude.Maybe Lude.Integer,
    -- | The number of records that couldn't be validated.
    validationSuspendedRecords :: Lude.Maybe Lude.Integer,
    -- | The schema name.
    schemaName :: Lude.Maybe Lude.Text,
    -- | Additional details about the state of validation.
    validationStateDetails :: Lude.Maybe Lude.Text,
    -- | The state of the tables described.
    --
    -- Valid states: Table does not exist | Before load | Full load | Table completed | Table cancelled | Table error | Table all | Table updates | Table is being reloaded
    tableState :: Lude.Maybe Lude.Text,
    -- | The number of rows that failed to load during the full load operation (valid only for migrations where DynamoDB is the target).
    fullLoadErrorRows :: Lude.Maybe Lude.Integer,
    -- | The data definition language (DDL) used to build and modify the structure of your tables.
    ddls :: Lude.Maybe Lude.Integer,
    -- | The number of delete actions performed on a table.
    deletes :: Lude.Maybe Lude.Integer,
    -- | The number of update actions performed on a table.
    updates :: Lude.Maybe Lude.Integer,
    -- | The number of records that have yet to be validated.
    validationPendingRecords :: Lude.Maybe Lude.Integer,
    -- | The time when the full load operation started.
    fullLoadStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The last time a table was updated.
    lastUpdateTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the table.
    tableName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableStatistics' with the minimum fields required to make a request.
--
-- * 'validationState' - The validation state of the table.
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
-- * 'fullLoadRows' - The number of rows added during the full load operation.
-- * 'inserts' - The number of insert actions performed on a table.
-- * 'fullLoadEndTime' - The time when the full load operation completed.
-- * 'fullLoadCondtnlChkFailedRows' - The number of rows that failed conditional checks during the full load operation (valid only for migrations where DynamoDB is the target).
-- * 'fullLoadReloaded' - A value that indicates if the table was reloaded (@true@ ) or loaded as part of a new full load operation (@false@ ).
-- * 'validationFailedRecords' - The number of records that failed validation.
-- * 'validationSuspendedRecords' - The number of records that couldn't be validated.
-- * 'schemaName' - The schema name.
-- * 'validationStateDetails' - Additional details about the state of validation.
-- * 'tableState' - The state of the tables described.
--
-- Valid states: Table does not exist | Before load | Full load | Table completed | Table cancelled | Table error | Table all | Table updates | Table is being reloaded
-- * 'fullLoadErrorRows' - The number of rows that failed to load during the full load operation (valid only for migrations where DynamoDB is the target).
-- * 'ddls' - The data definition language (DDL) used to build and modify the structure of your tables.
-- * 'deletes' - The number of delete actions performed on a table.
-- * 'updates' - The number of update actions performed on a table.
-- * 'validationPendingRecords' - The number of records that have yet to be validated.
-- * 'fullLoadStartTime' - The time when the full load operation started.
-- * 'lastUpdateTime' - The last time a table was updated.
-- * 'tableName' - The name of the table.
mkTableStatistics ::
  TableStatistics
mkTableStatistics =
  TableStatistics'
    { validationState = Lude.Nothing,
      fullLoadRows = Lude.Nothing,
      inserts = Lude.Nothing,
      fullLoadEndTime = Lude.Nothing,
      fullLoadCondtnlChkFailedRows = Lude.Nothing,
      fullLoadReloaded = Lude.Nothing,
      validationFailedRecords = Lude.Nothing,
      validationSuspendedRecords = Lude.Nothing,
      schemaName = Lude.Nothing,
      validationStateDetails = Lude.Nothing,
      tableState = Lude.Nothing,
      fullLoadErrorRows = Lude.Nothing,
      ddls = Lude.Nothing,
      deletes = Lude.Nothing,
      updates = Lude.Nothing,
      validationPendingRecords = Lude.Nothing,
      fullLoadStartTime = Lude.Nothing,
      lastUpdateTime = Lude.Nothing,
      tableName = Lude.Nothing
    }

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
tsValidationState :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Text)
tsValidationState = Lens.lens (validationState :: TableStatistics -> Lude.Maybe Lude.Text) (\s a -> s {validationState = a} :: TableStatistics)
{-# DEPRECATED tsValidationState "Use generic-lens or generic-optics with 'validationState' instead." #-}

-- | The number of rows added during the full load operation.
--
-- /Note:/ Consider using 'fullLoadRows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadRows :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Integer)
tsFullLoadRows = Lens.lens (fullLoadRows :: TableStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {fullLoadRows = a} :: TableStatistics)
{-# DEPRECATED tsFullLoadRows "Use generic-lens or generic-optics with 'fullLoadRows' instead." #-}

-- | The number of insert actions performed on a table.
--
-- /Note:/ Consider using 'inserts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsInserts :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Integer)
tsInserts = Lens.lens (inserts :: TableStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {inserts = a} :: TableStatistics)
{-# DEPRECATED tsInserts "Use generic-lens or generic-optics with 'inserts' instead." #-}

-- | The time when the full load operation completed.
--
-- /Note:/ Consider using 'fullLoadEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadEndTime :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Timestamp)
tsFullLoadEndTime = Lens.lens (fullLoadEndTime :: TableStatistics -> Lude.Maybe Lude.Timestamp) (\s a -> s {fullLoadEndTime = a} :: TableStatistics)
{-# DEPRECATED tsFullLoadEndTime "Use generic-lens or generic-optics with 'fullLoadEndTime' instead." #-}

-- | The number of rows that failed conditional checks during the full load operation (valid only for migrations where DynamoDB is the target).
--
-- /Note:/ Consider using 'fullLoadCondtnlChkFailedRows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadCondtnlChkFailedRows :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Integer)
tsFullLoadCondtnlChkFailedRows = Lens.lens (fullLoadCondtnlChkFailedRows :: TableStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {fullLoadCondtnlChkFailedRows = a} :: TableStatistics)
{-# DEPRECATED tsFullLoadCondtnlChkFailedRows "Use generic-lens or generic-optics with 'fullLoadCondtnlChkFailedRows' instead." #-}

-- | A value that indicates if the table was reloaded (@true@ ) or loaded as part of a new full load operation (@false@ ).
--
-- /Note:/ Consider using 'fullLoadReloaded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadReloaded :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Bool)
tsFullLoadReloaded = Lens.lens (fullLoadReloaded :: TableStatistics -> Lude.Maybe Lude.Bool) (\s a -> s {fullLoadReloaded = a} :: TableStatistics)
{-# DEPRECATED tsFullLoadReloaded "Use generic-lens or generic-optics with 'fullLoadReloaded' instead." #-}

-- | The number of records that failed validation.
--
-- /Note:/ Consider using 'validationFailedRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsValidationFailedRecords :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Integer)
tsValidationFailedRecords = Lens.lens (validationFailedRecords :: TableStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {validationFailedRecords = a} :: TableStatistics)
{-# DEPRECATED tsValidationFailedRecords "Use generic-lens or generic-optics with 'validationFailedRecords' instead." #-}

-- | The number of records that couldn't be validated.
--
-- /Note:/ Consider using 'validationSuspendedRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsValidationSuspendedRecords :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Integer)
tsValidationSuspendedRecords = Lens.lens (validationSuspendedRecords :: TableStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {validationSuspendedRecords = a} :: TableStatistics)
{-# DEPRECATED tsValidationSuspendedRecords "Use generic-lens or generic-optics with 'validationSuspendedRecords' instead." #-}

-- | The schema name.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsSchemaName :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Text)
tsSchemaName = Lens.lens (schemaName :: TableStatistics -> Lude.Maybe Lude.Text) (\s a -> s {schemaName = a} :: TableStatistics)
{-# DEPRECATED tsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | Additional details about the state of validation.
--
-- /Note:/ Consider using 'validationStateDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsValidationStateDetails :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Text)
tsValidationStateDetails = Lens.lens (validationStateDetails :: TableStatistics -> Lude.Maybe Lude.Text) (\s a -> s {validationStateDetails = a} :: TableStatistics)
{-# DEPRECATED tsValidationStateDetails "Use generic-lens or generic-optics with 'validationStateDetails' instead." #-}

-- | The state of the tables described.
--
-- Valid states: Table does not exist | Before load | Full load | Table completed | Table cancelled | Table error | Table all | Table updates | Table is being reloaded
--
-- /Note:/ Consider using 'tableState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTableState :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Text)
tsTableState = Lens.lens (tableState :: TableStatistics -> Lude.Maybe Lude.Text) (\s a -> s {tableState = a} :: TableStatistics)
{-# DEPRECATED tsTableState "Use generic-lens or generic-optics with 'tableState' instead." #-}

-- | The number of rows that failed to load during the full load operation (valid only for migrations where DynamoDB is the target).
--
-- /Note:/ Consider using 'fullLoadErrorRows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadErrorRows :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Integer)
tsFullLoadErrorRows = Lens.lens (fullLoadErrorRows :: TableStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {fullLoadErrorRows = a} :: TableStatistics)
{-# DEPRECATED tsFullLoadErrorRows "Use generic-lens or generic-optics with 'fullLoadErrorRows' instead." #-}

-- | The data definition language (DDL) used to build and modify the structure of your tables.
--
-- /Note:/ Consider using 'ddls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDdls :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Integer)
tsDdls = Lens.lens (ddls :: TableStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {ddls = a} :: TableStatistics)
{-# DEPRECATED tsDdls "Use generic-lens or generic-optics with 'ddls' instead." #-}

-- | The number of delete actions performed on a table.
--
-- /Note:/ Consider using 'deletes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDeletes :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Integer)
tsDeletes = Lens.lens (deletes :: TableStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {deletes = a} :: TableStatistics)
{-# DEPRECATED tsDeletes "Use generic-lens or generic-optics with 'deletes' instead." #-}

-- | The number of update actions performed on a table.
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsUpdates :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Integer)
tsUpdates = Lens.lens (updates :: TableStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {updates = a} :: TableStatistics)
{-# DEPRECATED tsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The number of records that have yet to be validated.
--
-- /Note:/ Consider using 'validationPendingRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsValidationPendingRecords :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Integer)
tsValidationPendingRecords = Lens.lens (validationPendingRecords :: TableStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {validationPendingRecords = a} :: TableStatistics)
{-# DEPRECATED tsValidationPendingRecords "Use generic-lens or generic-optics with 'validationPendingRecords' instead." #-}

-- | The time when the full load operation started.
--
-- /Note:/ Consider using 'fullLoadStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFullLoadStartTime :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Timestamp)
tsFullLoadStartTime = Lens.lens (fullLoadStartTime :: TableStatistics -> Lude.Maybe Lude.Timestamp) (\s a -> s {fullLoadStartTime = a} :: TableStatistics)
{-# DEPRECATED tsFullLoadStartTime "Use generic-lens or generic-optics with 'fullLoadStartTime' instead." #-}

-- | The last time a table was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLastUpdateTime :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Timestamp)
tsLastUpdateTime = Lens.lens (lastUpdateTime :: TableStatistics -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: TableStatistics)
{-# DEPRECATED tsLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTableName :: Lens.Lens' TableStatistics (Lude.Maybe Lude.Text)
tsTableName = Lens.lens (tableName :: TableStatistics -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: TableStatistics)
{-# DEPRECATED tsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON TableStatistics where
  parseJSON =
    Lude.withObject
      "TableStatistics"
      ( \x ->
          TableStatistics'
            Lude.<$> (x Lude..:? "ValidationState")
            Lude.<*> (x Lude..:? "FullLoadRows")
            Lude.<*> (x Lude..:? "Inserts")
            Lude.<*> (x Lude..:? "FullLoadEndTime")
            Lude.<*> (x Lude..:? "FullLoadCondtnlChkFailedRows")
            Lude.<*> (x Lude..:? "FullLoadReloaded")
            Lude.<*> (x Lude..:? "ValidationFailedRecords")
            Lude.<*> (x Lude..:? "ValidationSuspendedRecords")
            Lude.<*> (x Lude..:? "SchemaName")
            Lude.<*> (x Lude..:? "ValidationStateDetails")
            Lude.<*> (x Lude..:? "TableState")
            Lude.<*> (x Lude..:? "FullLoadErrorRows")
            Lude.<*> (x Lude..:? "Ddls")
            Lude.<*> (x Lude..:? "Deletes")
            Lude.<*> (x Lude..:? "Updates")
            Lude.<*> (x Lude..:? "ValidationPendingRecords")
            Lude.<*> (x Lude..:? "FullLoadStartTime")
            Lude.<*> (x Lude..:? "LastUpdateTime")
            Lude.<*> (x Lude..:? "TableName")
      )
