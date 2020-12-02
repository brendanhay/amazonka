{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupDetails where

import Network.AWS.DynamoDB.Types.BackupStatus
import Network.AWS.DynamoDB.Types.BackupType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the details of the backup created for the table.
--
--
--
-- /See:/ 'backupDetails' smart constructor.
data BackupDetails = BackupDetails'
  { _bdBackupExpiryDateTime ::
      !(Maybe POSIX),
    _bdBackupSizeBytes :: !(Maybe Nat),
    _bdBackupARN :: !Text,
    _bdBackupName :: !Text,
    _bdBackupStatus :: !BackupStatus,
    _bdBackupType :: !BackupType,
    _bdBackupCreationDateTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BackupDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdBackupExpiryDateTime' - Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
--
-- * 'bdBackupSizeBytes' - Size of the backup in bytes.
--
-- * 'bdBackupARN' - ARN associated with the backup.
--
-- * 'bdBackupName' - Name of the requested backup.
--
-- * 'bdBackupStatus' - Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
--
-- * 'bdBackupType' - BackupType:     * @USER@ - You create and manage these using the on-demand backup feature.     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.      * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
--
-- * 'bdBackupCreationDateTime' - Time at which the backup was created. This is the request time of the backup.
backupDetails ::
  -- | 'bdBackupARN'
  Text ->
  -- | 'bdBackupName'
  Text ->
  -- | 'bdBackupStatus'
  BackupStatus ->
  -- | 'bdBackupType'
  BackupType ->
  -- | 'bdBackupCreationDateTime'
  UTCTime ->
  BackupDetails
backupDetails
  pBackupARN_
  pBackupName_
  pBackupStatus_
  pBackupType_
  pBackupCreationDateTime_ =
    BackupDetails'
      { _bdBackupExpiryDateTime = Nothing,
        _bdBackupSizeBytes = Nothing,
        _bdBackupARN = pBackupARN_,
        _bdBackupName = pBackupName_,
        _bdBackupStatus = pBackupStatus_,
        _bdBackupType = pBackupType_,
        _bdBackupCreationDateTime = _Time # pBackupCreationDateTime_
      }

-- | Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
bdBackupExpiryDateTime :: Lens' BackupDetails (Maybe UTCTime)
bdBackupExpiryDateTime = lens _bdBackupExpiryDateTime (\s a -> s {_bdBackupExpiryDateTime = a}) . mapping _Time

-- | Size of the backup in bytes.
bdBackupSizeBytes :: Lens' BackupDetails (Maybe Natural)
bdBackupSizeBytes = lens _bdBackupSizeBytes (\s a -> s {_bdBackupSizeBytes = a}) . mapping _Nat

-- | ARN associated with the backup.
bdBackupARN :: Lens' BackupDetails Text
bdBackupARN = lens _bdBackupARN (\s a -> s {_bdBackupARN = a})

-- | Name of the requested backup.
bdBackupName :: Lens' BackupDetails Text
bdBackupName = lens _bdBackupName (\s a -> s {_bdBackupName = a})

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
bdBackupStatus :: Lens' BackupDetails BackupStatus
bdBackupStatus = lens _bdBackupStatus (\s a -> s {_bdBackupStatus = a})

-- | BackupType:     * @USER@ - You create and manage these using the on-demand backup feature.     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.      * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
bdBackupType :: Lens' BackupDetails BackupType
bdBackupType = lens _bdBackupType (\s a -> s {_bdBackupType = a})

-- | Time at which the backup was created. This is the request time of the backup.
bdBackupCreationDateTime :: Lens' BackupDetails UTCTime
bdBackupCreationDateTime = lens _bdBackupCreationDateTime (\s a -> s {_bdBackupCreationDateTime = a}) . _Time

instance FromJSON BackupDetails where
  parseJSON =
    withObject
      "BackupDetails"
      ( \x ->
          BackupDetails'
            <$> (x .:? "BackupExpiryDateTime")
            <*> (x .:? "BackupSizeBytes")
            <*> (x .: "BackupArn")
            <*> (x .: "BackupName")
            <*> (x .: "BackupStatus")
            <*> (x .: "BackupType")
            <*> (x .: "BackupCreationDateTime")
      )

instance Hashable BackupDetails

instance NFData BackupDetails
