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
-- Module      : Network.AWS.CloudHSMv2.Types.Backup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.Backup where

import Network.AWS.CloudHSMv2.Types.BackupState
import Network.AWS.CloudHSMv2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a backup of an AWS CloudHSM cluster. All
-- backup objects contain the @BackupId@, @BackupState@, @ClusterId@, and
-- @CreateTimestamp@ parameters. Backups that were copied into a
-- destination region additionally contain the @CopyTimestamp@,
-- @SourceBackup@, @SourceCluster@, and @SourceRegion@ parameters. A backup
-- that is pending deletion will include the @DeleteTimestamp@ parameter.
--
-- /See:/ 'newBackup' smart constructor.
data Backup = Backup'
  { -- | The identifier (ID) of the cluster that was backed up.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The state of the backup.
    backupState :: Prelude.Maybe BackupState,
    -- | The identifier (ID) of the source backup from which the new backup was
    -- copied.
    sourceBackup :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the backup was copied from a source backup.
    copyTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time when the backup was created.
    createTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies whether the service should exempt a backup from the retention
    -- policy for the cluster. @True@ exempts a backup from the retention
    -- policy. @False@ means the service applies the backup retention policy
    -- defined at the cluster.
    neverExpires :: Prelude.Maybe Prelude.Bool,
    -- | The identifier (ID) of the cluster containing the source backup from
    -- which the new backup was copied.
    sourceCluster :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the backup will be permanently deleted.
    deleteTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The list of tags for the backup.
    tagList :: Prelude.Maybe [Tag],
    -- | The AWS Region that contains the source backup from which the new backup
    -- was copied.
    sourceRegion :: Prelude.Maybe Prelude.Text,
    -- | The identifier (ID) of the backup.
    backupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Backup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'backup_clusterId' - The identifier (ID) of the cluster that was backed up.
--
-- 'backupState', 'backup_backupState' - The state of the backup.
--
-- 'sourceBackup', 'backup_sourceBackup' - The identifier (ID) of the source backup from which the new backup was
-- copied.
--
-- 'copyTimestamp', 'backup_copyTimestamp' - The date and time when the backup was copied from a source backup.
--
-- 'createTimestamp', 'backup_createTimestamp' - The date and time when the backup was created.
--
-- 'neverExpires', 'backup_neverExpires' - Specifies whether the service should exempt a backup from the retention
-- policy for the cluster. @True@ exempts a backup from the retention
-- policy. @False@ means the service applies the backup retention policy
-- defined at the cluster.
--
-- 'sourceCluster', 'backup_sourceCluster' - The identifier (ID) of the cluster containing the source backup from
-- which the new backup was copied.
--
-- 'deleteTimestamp', 'backup_deleteTimestamp' - The date and time when the backup will be permanently deleted.
--
-- 'tagList', 'backup_tagList' - The list of tags for the backup.
--
-- 'sourceRegion', 'backup_sourceRegion' - The AWS Region that contains the source backup from which the new backup
-- was copied.
--
-- 'backupId', 'backup_backupId' - The identifier (ID) of the backup.
newBackup ::
  -- | 'backupId'
  Prelude.Text ->
  Backup
newBackup pBackupId_ =
  Backup'
    { clusterId = Prelude.Nothing,
      backupState = Prelude.Nothing,
      sourceBackup = Prelude.Nothing,
      copyTimestamp = Prelude.Nothing,
      createTimestamp = Prelude.Nothing,
      neverExpires = Prelude.Nothing,
      sourceCluster = Prelude.Nothing,
      deleteTimestamp = Prelude.Nothing,
      tagList = Prelude.Nothing,
      sourceRegion = Prelude.Nothing,
      backupId = pBackupId_
    }

-- | The identifier (ID) of the cluster that was backed up.
backup_clusterId :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_clusterId = Lens.lens (\Backup' {clusterId} -> clusterId) (\s@Backup' {} a -> s {clusterId = a} :: Backup)

-- | The state of the backup.
backup_backupState :: Lens.Lens' Backup (Prelude.Maybe BackupState)
backup_backupState = Lens.lens (\Backup' {backupState} -> backupState) (\s@Backup' {} a -> s {backupState = a} :: Backup)

-- | The identifier (ID) of the source backup from which the new backup was
-- copied.
backup_sourceBackup :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_sourceBackup = Lens.lens (\Backup' {sourceBackup} -> sourceBackup) (\s@Backup' {} a -> s {sourceBackup = a} :: Backup)

-- | The date and time when the backup was copied from a source backup.
backup_copyTimestamp :: Lens.Lens' Backup (Prelude.Maybe Prelude.UTCTime)
backup_copyTimestamp = Lens.lens (\Backup' {copyTimestamp} -> copyTimestamp) (\s@Backup' {} a -> s {copyTimestamp = a} :: Backup) Prelude.. Lens.mapping Prelude._Time

-- | The date and time when the backup was created.
backup_createTimestamp :: Lens.Lens' Backup (Prelude.Maybe Prelude.UTCTime)
backup_createTimestamp = Lens.lens (\Backup' {createTimestamp} -> createTimestamp) (\s@Backup' {} a -> s {createTimestamp = a} :: Backup) Prelude.. Lens.mapping Prelude._Time

-- | Specifies whether the service should exempt a backup from the retention
-- policy for the cluster. @True@ exempts a backup from the retention
-- policy. @False@ means the service applies the backup retention policy
-- defined at the cluster.
backup_neverExpires :: Lens.Lens' Backup (Prelude.Maybe Prelude.Bool)
backup_neverExpires = Lens.lens (\Backup' {neverExpires} -> neverExpires) (\s@Backup' {} a -> s {neverExpires = a} :: Backup)

-- | The identifier (ID) of the cluster containing the source backup from
-- which the new backup was copied.
backup_sourceCluster :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_sourceCluster = Lens.lens (\Backup' {sourceCluster} -> sourceCluster) (\s@Backup' {} a -> s {sourceCluster = a} :: Backup)

-- | The date and time when the backup will be permanently deleted.
backup_deleteTimestamp :: Lens.Lens' Backup (Prelude.Maybe Prelude.UTCTime)
backup_deleteTimestamp = Lens.lens (\Backup' {deleteTimestamp} -> deleteTimestamp) (\s@Backup' {} a -> s {deleteTimestamp = a} :: Backup) Prelude.. Lens.mapping Prelude._Time

-- | The list of tags for the backup.
backup_tagList :: Lens.Lens' Backup (Prelude.Maybe [Tag])
backup_tagList = Lens.lens (\Backup' {tagList} -> tagList) (\s@Backup' {} a -> s {tagList = a} :: Backup) Prelude.. Lens.mapping Prelude._Coerce

-- | The AWS Region that contains the source backup from which the new backup
-- was copied.
backup_sourceRegion :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_sourceRegion = Lens.lens (\Backup' {sourceRegion} -> sourceRegion) (\s@Backup' {} a -> s {sourceRegion = a} :: Backup)

-- | The identifier (ID) of the backup.
backup_backupId :: Lens.Lens' Backup Prelude.Text
backup_backupId = Lens.lens (\Backup' {backupId} -> backupId) (\s@Backup' {} a -> s {backupId = a} :: Backup)

instance Prelude.FromJSON Backup where
  parseJSON =
    Prelude.withObject
      "Backup"
      ( \x ->
          Backup'
            Prelude.<$> (x Prelude..:? "ClusterId")
            Prelude.<*> (x Prelude..:? "BackupState")
            Prelude.<*> (x Prelude..:? "SourceBackup")
            Prelude.<*> (x Prelude..:? "CopyTimestamp")
            Prelude.<*> (x Prelude..:? "CreateTimestamp")
            Prelude.<*> (x Prelude..:? "NeverExpires")
            Prelude.<*> (x Prelude..:? "SourceCluster")
            Prelude.<*> (x Prelude..:? "DeleteTimestamp")
            Prelude.<*> (x Prelude..:? "TagList" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "SourceRegion")
            Prelude.<*> (x Prelude..: "BackupId")
      )

instance Prelude.Hashable Backup

instance Prelude.NFData Backup
