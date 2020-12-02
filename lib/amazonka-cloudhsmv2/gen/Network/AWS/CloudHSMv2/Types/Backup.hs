{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Backup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.Backup where

import Network.AWS.CloudHSMv2.Types.BackupState
import Network.AWS.CloudHSMv2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a backup of an AWS CloudHSM cluster. All backup objects contain the @BackupId@ , @BackupState@ , @ClusterId@ , and @CreateTimestamp@ parameters. Backups that were copied into a destination region additionally contain the @CopyTimestamp@ , @SourceBackup@ , @SourceCluster@ , and @SourceRegion@ parameters. A backup that is pending deletion will include the @DeleteTimestamp@ parameter.
--
--
--
-- /See:/ 'backup' smart constructor.
data Backup = Backup'
  { _bDeleteTimestamp :: !(Maybe POSIX),
    _bSourceCluster :: !(Maybe Text),
    _bNeverExpires :: !(Maybe Bool),
    _bSourceRegion :: !(Maybe Text),
    _bTagList :: !(Maybe [Tag]),
    _bSourceBackup :: !(Maybe Text),
    _bClusterId :: !(Maybe Text),
    _bCreateTimestamp :: !(Maybe POSIX),
    _bCopyTimestamp :: !(Maybe POSIX),
    _bBackupState :: !(Maybe BackupState),
    _bBackupId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Backup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bDeleteTimestamp' - The date and time when the backup will be permanently deleted.
--
-- * 'bSourceCluster' - The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
--
-- * 'bNeverExpires' - Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
--
-- * 'bSourceRegion' - The AWS Region that contains the source backup from which the new backup was copied.
--
-- * 'bTagList' - The list of tags for the backup.
--
-- * 'bSourceBackup' - The identifier (ID) of the source backup from which the new backup was copied.
--
-- * 'bClusterId' - The identifier (ID) of the cluster that was backed up.
--
-- * 'bCreateTimestamp' - The date and time when the backup was created.
--
-- * 'bCopyTimestamp' - The date and time when the backup was copied from a source backup.
--
-- * 'bBackupState' - The state of the backup.
--
-- * 'bBackupId' - The identifier (ID) of the backup.
backup ::
  -- | 'bBackupId'
  Text ->
  Backup
backup pBackupId_ =
  Backup'
    { _bDeleteTimestamp = Nothing,
      _bSourceCluster = Nothing,
      _bNeverExpires = Nothing,
      _bSourceRegion = Nothing,
      _bTagList = Nothing,
      _bSourceBackup = Nothing,
      _bClusterId = Nothing,
      _bCreateTimestamp = Nothing,
      _bCopyTimestamp = Nothing,
      _bBackupState = Nothing,
      _bBackupId = pBackupId_
    }

-- | The date and time when the backup will be permanently deleted.
bDeleteTimestamp :: Lens' Backup (Maybe UTCTime)
bDeleteTimestamp = lens _bDeleteTimestamp (\s a -> s {_bDeleteTimestamp = a}) . mapping _Time

-- | The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
bSourceCluster :: Lens' Backup (Maybe Text)
bSourceCluster = lens _bSourceCluster (\s a -> s {_bSourceCluster = a})

-- | Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
bNeverExpires :: Lens' Backup (Maybe Bool)
bNeverExpires = lens _bNeverExpires (\s a -> s {_bNeverExpires = a})

-- | The AWS Region that contains the source backup from which the new backup was copied.
bSourceRegion :: Lens' Backup (Maybe Text)
bSourceRegion = lens _bSourceRegion (\s a -> s {_bSourceRegion = a})

-- | The list of tags for the backup.
bTagList :: Lens' Backup [Tag]
bTagList = lens _bTagList (\s a -> s {_bTagList = a}) . _Default . _Coerce

-- | The identifier (ID) of the source backup from which the new backup was copied.
bSourceBackup :: Lens' Backup (Maybe Text)
bSourceBackup = lens _bSourceBackup (\s a -> s {_bSourceBackup = a})

-- | The identifier (ID) of the cluster that was backed up.
bClusterId :: Lens' Backup (Maybe Text)
bClusterId = lens _bClusterId (\s a -> s {_bClusterId = a})

-- | The date and time when the backup was created.
bCreateTimestamp :: Lens' Backup (Maybe UTCTime)
bCreateTimestamp = lens _bCreateTimestamp (\s a -> s {_bCreateTimestamp = a}) . mapping _Time

-- | The date and time when the backup was copied from a source backup.
bCopyTimestamp :: Lens' Backup (Maybe UTCTime)
bCopyTimestamp = lens _bCopyTimestamp (\s a -> s {_bCopyTimestamp = a}) . mapping _Time

-- | The state of the backup.
bBackupState :: Lens' Backup (Maybe BackupState)
bBackupState = lens _bBackupState (\s a -> s {_bBackupState = a})

-- | The identifier (ID) of the backup.
bBackupId :: Lens' Backup Text
bBackupId = lens _bBackupId (\s a -> s {_bBackupId = a})

instance FromJSON Backup where
  parseJSON =
    withObject
      "Backup"
      ( \x ->
          Backup'
            <$> (x .:? "DeleteTimestamp")
            <*> (x .:? "SourceCluster")
            <*> (x .:? "NeverExpires")
            <*> (x .:? "SourceRegion")
            <*> (x .:? "TagList" .!= mempty)
            <*> (x .:? "SourceBackup")
            <*> (x .:? "ClusterId")
            <*> (x .:? "CreateTimestamp")
            <*> (x .:? "CopyTimestamp")
            <*> (x .:? "BackupState")
            <*> (x .: "BackupId")
      )

instance Hashable Backup

instance NFData Backup
