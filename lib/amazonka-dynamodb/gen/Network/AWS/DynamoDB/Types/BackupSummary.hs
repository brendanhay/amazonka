{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupSummary where

import Network.AWS.DynamoDB.Types.BackupStatus
import Network.AWS.DynamoDB.Types.BackupType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details for the backup.
--
--
--
-- /See:/ 'backupSummary' smart constructor.
data BackupSummary = BackupSummary'
  { _bsBackupExpiryDateTime ::
      !(Maybe POSIX),
    _bsTableARN :: !(Maybe Text),
    _bsBackupName :: !(Maybe Text),
    _bsBackupStatus :: !(Maybe BackupStatus),
    _bsBackupSizeBytes :: !(Maybe Nat),
    _bsBackupARN :: !(Maybe Text),
    _bsTableId :: !(Maybe Text),
    _bsBackupCreationDateTime :: !(Maybe POSIX),
    _bsBackupType :: !(Maybe BackupType),
    _bsTableName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BackupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsBackupExpiryDateTime' - Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
--
-- * 'bsTableARN' - ARN associated with the table.
--
-- * 'bsBackupName' - Name of the specified backup.
--
-- * 'bsBackupStatus' - Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
--
-- * 'bsBackupSizeBytes' - Size of the backup in bytes.
--
-- * 'bsBackupARN' - ARN associated with the backup.
--
-- * 'bsTableId' - Unique identifier for the table.
--
-- * 'bsBackupCreationDateTime' - Time at which the backup was created.
--
-- * 'bsBackupType' - BackupType:     * @USER@ - You create and manage these using the on-demand backup feature.     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.      * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
--
-- * 'bsTableName' - Name of the table.
backupSummary ::
  BackupSummary
backupSummary =
  BackupSummary'
    { _bsBackupExpiryDateTime = Nothing,
      _bsTableARN = Nothing,
      _bsBackupName = Nothing,
      _bsBackupStatus = Nothing,
      _bsBackupSizeBytes = Nothing,
      _bsBackupARN = Nothing,
      _bsTableId = Nothing,
      _bsBackupCreationDateTime = Nothing,
      _bsBackupType = Nothing,
      _bsTableName = Nothing
    }

-- | Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
bsBackupExpiryDateTime :: Lens' BackupSummary (Maybe UTCTime)
bsBackupExpiryDateTime = lens _bsBackupExpiryDateTime (\s a -> s {_bsBackupExpiryDateTime = a}) . mapping _Time

-- | ARN associated with the table.
bsTableARN :: Lens' BackupSummary (Maybe Text)
bsTableARN = lens _bsTableARN (\s a -> s {_bsTableARN = a})

-- | Name of the specified backup.
bsBackupName :: Lens' BackupSummary (Maybe Text)
bsBackupName = lens _bsBackupName (\s a -> s {_bsBackupName = a})

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
bsBackupStatus :: Lens' BackupSummary (Maybe BackupStatus)
bsBackupStatus = lens _bsBackupStatus (\s a -> s {_bsBackupStatus = a})

-- | Size of the backup in bytes.
bsBackupSizeBytes :: Lens' BackupSummary (Maybe Natural)
bsBackupSizeBytes = lens _bsBackupSizeBytes (\s a -> s {_bsBackupSizeBytes = a}) . mapping _Nat

-- | ARN associated with the backup.
bsBackupARN :: Lens' BackupSummary (Maybe Text)
bsBackupARN = lens _bsBackupARN (\s a -> s {_bsBackupARN = a})

-- | Unique identifier for the table.
bsTableId :: Lens' BackupSummary (Maybe Text)
bsTableId = lens _bsTableId (\s a -> s {_bsTableId = a})

-- | Time at which the backup was created.
bsBackupCreationDateTime :: Lens' BackupSummary (Maybe UTCTime)
bsBackupCreationDateTime = lens _bsBackupCreationDateTime (\s a -> s {_bsBackupCreationDateTime = a}) . mapping _Time

-- | BackupType:     * @USER@ - You create and manage these using the on-demand backup feature.     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.      * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
bsBackupType :: Lens' BackupSummary (Maybe BackupType)
bsBackupType = lens _bsBackupType (\s a -> s {_bsBackupType = a})

-- | Name of the table.
bsTableName :: Lens' BackupSummary (Maybe Text)
bsTableName = lens _bsTableName (\s a -> s {_bsTableName = a})

instance FromJSON BackupSummary where
  parseJSON =
    withObject
      "BackupSummary"
      ( \x ->
          BackupSummary'
            <$> (x .:? "BackupExpiryDateTime")
            <*> (x .:? "TableArn")
            <*> (x .:? "BackupName")
            <*> (x .:? "BackupStatus")
            <*> (x .:? "BackupSizeBytes")
            <*> (x .:? "BackupArn")
            <*> (x .:? "TableId")
            <*> (x .:? "BackupCreationDateTime")
            <*> (x .:? "BackupType")
            <*> (x .:? "TableName")
      )

instance Hashable BackupSummary

instance NFData BackupSummary
