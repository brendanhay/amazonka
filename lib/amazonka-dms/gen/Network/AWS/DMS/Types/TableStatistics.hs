{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.TableStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.TableStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a collection of table statistics in response to a request by the @DescribeTableStatistics@ operation.
--
--
--
-- /See:/ 'tableStatistics' smart constructor.
data TableStatistics = TableStatistics'
  { _tsValidationState ::
      !(Maybe Text),
    _tsFullLoadRows :: !(Maybe Integer),
    _tsInserts :: !(Maybe Integer),
    _tsFullLoadEndTime :: !(Maybe POSIX),
    _tsFullLoadCondtnlChkFailedRows :: !(Maybe Integer),
    _tsFullLoadReloaded :: !(Maybe Bool),
    _tsValidationFailedRecords :: !(Maybe Integer),
    _tsValidationSuspendedRecords :: !(Maybe Integer),
    _tsSchemaName :: !(Maybe Text),
    _tsValidationStateDetails :: !(Maybe Text),
    _tsTableState :: !(Maybe Text),
    _tsFullLoadErrorRows :: !(Maybe Integer),
    _tsDdls :: !(Maybe Integer),
    _tsDeletes :: !(Maybe Integer),
    _tsUpdates :: !(Maybe Integer),
    _tsValidationPendingRecords :: !(Maybe Integer),
    _tsFullLoadStartTime :: !(Maybe POSIX),
    _tsLastUpdateTime :: !(Maybe POSIX),
    _tsTableName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TableStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsValidationState' - The validation state of the table. This parameter can have the following values:     * Not enabled – Validation isn't enabled for the table in the migration task.     * Pending records – Some records in the table are waiting for validation.     * Mismatched records – Some records in the table don't match between the source and target.     * Suspended records – Some records in the table couldn't be validated.     * No primary key –The table couldn't be validated because it has no primary key.     * Table error – The table wasn't validated because it's in an error state and some data wasn't migrated.     * Validated – All rows in the table are validated. If the table is updated, the status can change from Validated.     * Error – The table couldn't be validated because of an unexpected error.     * Pending validation – The table is waiting validation.     * Preparing table – Preparing the table enabled in the migration task for validation.     * Pending revalidation – All rows in the table are pending validation after the table was updated.
--
-- * 'tsFullLoadRows' - The number of rows added during the full load operation.
--
-- * 'tsInserts' - The number of insert actions performed on a table.
--
-- * 'tsFullLoadEndTime' - The time when the full load operation completed.
--
-- * 'tsFullLoadCondtnlChkFailedRows' - The number of rows that failed conditional checks during the full load operation (valid only for migrations where DynamoDB is the target).
--
-- * 'tsFullLoadReloaded' - A value that indicates if the table was reloaded (@true@ ) or loaded as part of a new full load operation (@false@ ).
--
-- * 'tsValidationFailedRecords' - The number of records that failed validation.
--
-- * 'tsValidationSuspendedRecords' - The number of records that couldn't be validated.
--
-- * 'tsSchemaName' - The schema name.
--
-- * 'tsValidationStateDetails' - Additional details about the state of validation.
--
-- * 'tsTableState' - The state of the tables described. Valid states: Table does not exist | Before load | Full load | Table completed | Table cancelled | Table error | Table all | Table updates | Table is being reloaded
--
-- * 'tsFullLoadErrorRows' - The number of rows that failed to load during the full load operation (valid only for migrations where DynamoDB is the target).
--
-- * 'tsDdls' - The data definition language (DDL) used to build and modify the structure of your tables.
--
-- * 'tsDeletes' - The number of delete actions performed on a table.
--
-- * 'tsUpdates' - The number of update actions performed on a table.
--
-- * 'tsValidationPendingRecords' - The number of records that have yet to be validated.
--
-- * 'tsFullLoadStartTime' - The time when the full load operation started.
--
-- * 'tsLastUpdateTime' - The last time a table was updated.
--
-- * 'tsTableName' - The name of the table.
tableStatistics ::
  TableStatistics
tableStatistics =
  TableStatistics'
    { _tsValidationState = Nothing,
      _tsFullLoadRows = Nothing,
      _tsInserts = Nothing,
      _tsFullLoadEndTime = Nothing,
      _tsFullLoadCondtnlChkFailedRows = Nothing,
      _tsFullLoadReloaded = Nothing,
      _tsValidationFailedRecords = Nothing,
      _tsValidationSuspendedRecords = Nothing,
      _tsSchemaName = Nothing,
      _tsValidationStateDetails = Nothing,
      _tsTableState = Nothing,
      _tsFullLoadErrorRows = Nothing,
      _tsDdls = Nothing,
      _tsDeletes = Nothing,
      _tsUpdates = Nothing,
      _tsValidationPendingRecords = Nothing,
      _tsFullLoadStartTime = Nothing,
      _tsLastUpdateTime = Nothing,
      _tsTableName = Nothing
    }

-- | The validation state of the table. This parameter can have the following values:     * Not enabled – Validation isn't enabled for the table in the migration task.     * Pending records – Some records in the table are waiting for validation.     * Mismatched records – Some records in the table don't match between the source and target.     * Suspended records – Some records in the table couldn't be validated.     * No primary key –The table couldn't be validated because it has no primary key.     * Table error – The table wasn't validated because it's in an error state and some data wasn't migrated.     * Validated – All rows in the table are validated. If the table is updated, the status can change from Validated.     * Error – The table couldn't be validated because of an unexpected error.     * Pending validation – The table is waiting validation.     * Preparing table – Preparing the table enabled in the migration task for validation.     * Pending revalidation – All rows in the table are pending validation after the table was updated.
tsValidationState :: Lens' TableStatistics (Maybe Text)
tsValidationState = lens _tsValidationState (\s a -> s {_tsValidationState = a})

-- | The number of rows added during the full load operation.
tsFullLoadRows :: Lens' TableStatistics (Maybe Integer)
tsFullLoadRows = lens _tsFullLoadRows (\s a -> s {_tsFullLoadRows = a})

-- | The number of insert actions performed on a table.
tsInserts :: Lens' TableStatistics (Maybe Integer)
tsInserts = lens _tsInserts (\s a -> s {_tsInserts = a})

-- | The time when the full load operation completed.
tsFullLoadEndTime :: Lens' TableStatistics (Maybe UTCTime)
tsFullLoadEndTime = lens _tsFullLoadEndTime (\s a -> s {_tsFullLoadEndTime = a}) . mapping _Time

-- | The number of rows that failed conditional checks during the full load operation (valid only for migrations where DynamoDB is the target).
tsFullLoadCondtnlChkFailedRows :: Lens' TableStatistics (Maybe Integer)
tsFullLoadCondtnlChkFailedRows = lens _tsFullLoadCondtnlChkFailedRows (\s a -> s {_tsFullLoadCondtnlChkFailedRows = a})

-- | A value that indicates if the table was reloaded (@true@ ) or loaded as part of a new full load operation (@false@ ).
tsFullLoadReloaded :: Lens' TableStatistics (Maybe Bool)
tsFullLoadReloaded = lens _tsFullLoadReloaded (\s a -> s {_tsFullLoadReloaded = a})

-- | The number of records that failed validation.
tsValidationFailedRecords :: Lens' TableStatistics (Maybe Integer)
tsValidationFailedRecords = lens _tsValidationFailedRecords (\s a -> s {_tsValidationFailedRecords = a})

-- | The number of records that couldn't be validated.
tsValidationSuspendedRecords :: Lens' TableStatistics (Maybe Integer)
tsValidationSuspendedRecords = lens _tsValidationSuspendedRecords (\s a -> s {_tsValidationSuspendedRecords = a})

-- | The schema name.
tsSchemaName :: Lens' TableStatistics (Maybe Text)
tsSchemaName = lens _tsSchemaName (\s a -> s {_tsSchemaName = a})

-- | Additional details about the state of validation.
tsValidationStateDetails :: Lens' TableStatistics (Maybe Text)
tsValidationStateDetails = lens _tsValidationStateDetails (\s a -> s {_tsValidationStateDetails = a})

-- | The state of the tables described. Valid states: Table does not exist | Before load | Full load | Table completed | Table cancelled | Table error | Table all | Table updates | Table is being reloaded
tsTableState :: Lens' TableStatistics (Maybe Text)
tsTableState = lens _tsTableState (\s a -> s {_tsTableState = a})

-- | The number of rows that failed to load during the full load operation (valid only for migrations where DynamoDB is the target).
tsFullLoadErrorRows :: Lens' TableStatistics (Maybe Integer)
tsFullLoadErrorRows = lens _tsFullLoadErrorRows (\s a -> s {_tsFullLoadErrorRows = a})

-- | The data definition language (DDL) used to build and modify the structure of your tables.
tsDdls :: Lens' TableStatistics (Maybe Integer)
tsDdls = lens _tsDdls (\s a -> s {_tsDdls = a})

-- | The number of delete actions performed on a table.
tsDeletes :: Lens' TableStatistics (Maybe Integer)
tsDeletes = lens _tsDeletes (\s a -> s {_tsDeletes = a})

-- | The number of update actions performed on a table.
tsUpdates :: Lens' TableStatistics (Maybe Integer)
tsUpdates = lens _tsUpdates (\s a -> s {_tsUpdates = a})

-- | The number of records that have yet to be validated.
tsValidationPendingRecords :: Lens' TableStatistics (Maybe Integer)
tsValidationPendingRecords = lens _tsValidationPendingRecords (\s a -> s {_tsValidationPendingRecords = a})

-- | The time when the full load operation started.
tsFullLoadStartTime :: Lens' TableStatistics (Maybe UTCTime)
tsFullLoadStartTime = lens _tsFullLoadStartTime (\s a -> s {_tsFullLoadStartTime = a}) . mapping _Time

-- | The last time a table was updated.
tsLastUpdateTime :: Lens' TableStatistics (Maybe UTCTime)
tsLastUpdateTime = lens _tsLastUpdateTime (\s a -> s {_tsLastUpdateTime = a}) . mapping _Time

-- | The name of the table.
tsTableName :: Lens' TableStatistics (Maybe Text)
tsTableName = lens _tsTableName (\s a -> s {_tsTableName = a})

instance FromJSON TableStatistics where
  parseJSON =
    withObject
      "TableStatistics"
      ( \x ->
          TableStatistics'
            <$> (x .:? "ValidationState")
            <*> (x .:? "FullLoadRows")
            <*> (x .:? "Inserts")
            <*> (x .:? "FullLoadEndTime")
            <*> (x .:? "FullLoadCondtnlChkFailedRows")
            <*> (x .:? "FullLoadReloaded")
            <*> (x .:? "ValidationFailedRecords")
            <*> (x .:? "ValidationSuspendedRecords")
            <*> (x .:? "SchemaName")
            <*> (x .:? "ValidationStateDetails")
            <*> (x .:? "TableState")
            <*> (x .:? "FullLoadErrorRows")
            <*> (x .:? "Ddls")
            <*> (x .:? "Deletes")
            <*> (x .:? "Updates")
            <*> (x .:? "ValidationPendingRecords")
            <*> (x .:? "FullLoadStartTime")
            <*> (x .:? "LastUpdateTime")
            <*> (x .:? "TableName")
      )

instance Hashable TableStatistics

instance NFData TableStatistics
