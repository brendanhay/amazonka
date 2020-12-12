{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Backup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.Backup
  ( Backup (..),

    -- * Smart constructor
    mkBackup,

    -- * Lenses
    bDeleteTimestamp,
    bSourceCluster,
    bNeverExpires,
    bSourceRegion,
    bTagList,
    bSourceBackup,
    bClusterId,
    bCreateTimestamp,
    bCopyTimestamp,
    bBackupState,
    bBackupId,
  )
where

import Network.AWS.CloudHSMv2.Types.BackupState
import Network.AWS.CloudHSMv2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a backup of an AWS CloudHSM cluster. All backup objects contain the @BackupId@ , @BackupState@ , @ClusterId@ , and @CreateTimestamp@ parameters. Backups that were copied into a destination region additionally contain the @CopyTimestamp@ , @SourceBackup@ , @SourceCluster@ , and @SourceRegion@ parameters. A backup that is pending deletion will include the @DeleteTimestamp@ parameter.
--
-- /See:/ 'mkBackup' smart constructor.
data Backup = Backup'
  { deleteTimestamp :: Lude.Maybe Lude.Timestamp,
    sourceCluster :: Lude.Maybe Lude.Text,
    neverExpires :: Lude.Maybe Lude.Bool,
    sourceRegion :: Lude.Maybe Lude.Text,
    tagList :: Lude.Maybe [Tag],
    sourceBackup :: Lude.Maybe Lude.Text,
    clusterId :: Lude.Maybe Lude.Text,
    createTimestamp :: Lude.Maybe Lude.Timestamp,
    copyTimestamp :: Lude.Maybe Lude.Timestamp,
    backupState :: Lude.Maybe BackupState,
    backupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Backup' with the minimum fields required to make a request.
--
-- * 'backupId' - The identifier (ID) of the backup.
-- * 'backupState' - The state of the backup.
-- * 'clusterId' - The identifier (ID) of the cluster that was backed up.
-- * 'copyTimestamp' - The date and time when the backup was copied from a source backup.
-- * 'createTimestamp' - The date and time when the backup was created.
-- * 'deleteTimestamp' - The date and time when the backup will be permanently deleted.
-- * 'neverExpires' - Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
-- * 'sourceBackup' - The identifier (ID) of the source backup from which the new backup was copied.
-- * 'sourceCluster' - The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
-- * 'sourceRegion' - The AWS Region that contains the source backup from which the new backup was copied.
-- * 'tagList' - The list of tags for the backup.
mkBackup ::
  -- | 'backupId'
  Lude.Text ->
  Backup
mkBackup pBackupId_ =
  Backup'
    { deleteTimestamp = Lude.Nothing,
      sourceCluster = Lude.Nothing,
      neverExpires = Lude.Nothing,
      sourceRegion = Lude.Nothing,
      tagList = Lude.Nothing,
      sourceBackup = Lude.Nothing,
      clusterId = Lude.Nothing,
      createTimestamp = Lude.Nothing,
      copyTimestamp = Lude.Nothing,
      backupState = Lude.Nothing,
      backupId = pBackupId_
    }

-- | The date and time when the backup will be permanently deleted.
--
-- /Note:/ Consider using 'deleteTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDeleteTimestamp :: Lens.Lens' Backup (Lude.Maybe Lude.Timestamp)
bDeleteTimestamp = Lens.lens (deleteTimestamp :: Backup -> Lude.Maybe Lude.Timestamp) (\s a -> s {deleteTimestamp = a} :: Backup)
{-# DEPRECATED bDeleteTimestamp "Use generic-lens or generic-optics with 'deleteTimestamp' instead." #-}

-- | The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSourceCluster :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bSourceCluster = Lens.lens (sourceCluster :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {sourceCluster = a} :: Backup)
{-# DEPRECATED bSourceCluster "Use generic-lens or generic-optics with 'sourceCluster' instead." #-}

-- | Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
--
-- /Note:/ Consider using 'neverExpires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNeverExpires :: Lens.Lens' Backup (Lude.Maybe Lude.Bool)
bNeverExpires = Lens.lens (neverExpires :: Backup -> Lude.Maybe Lude.Bool) (\s a -> s {neverExpires = a} :: Backup)
{-# DEPRECATED bNeverExpires "Use generic-lens or generic-optics with 'neverExpires' instead." #-}

-- | The AWS Region that contains the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSourceRegion :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bSourceRegion = Lens.lens (sourceRegion :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {sourceRegion = a} :: Backup)
{-# DEPRECATED bSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | The list of tags for the backup.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTagList :: Lens.Lens' Backup (Lude.Maybe [Tag])
bTagList = Lens.lens (tagList :: Backup -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: Backup)
{-# DEPRECATED bTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The identifier (ID) of the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSourceBackup :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bSourceBackup = Lens.lens (sourceBackup :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {sourceBackup = a} :: Backup)
{-# DEPRECATED bSourceBackup "Use generic-lens or generic-optics with 'sourceBackup' instead." #-}

-- | The identifier (ID) of the cluster that was backed up.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bClusterId :: Lens.Lens' Backup (Lude.Maybe Lude.Text)
bClusterId = Lens.lens (clusterId :: Backup -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: Backup)
{-# DEPRECATED bClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The date and time when the backup was created.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreateTimestamp :: Lens.Lens' Backup (Lude.Maybe Lude.Timestamp)
bCreateTimestamp = Lens.lens (createTimestamp :: Backup -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTimestamp = a} :: Backup)
{-# DEPRECATED bCreateTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead." #-}

-- | The date and time when the backup was copied from a source backup.
--
-- /Note:/ Consider using 'copyTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCopyTimestamp :: Lens.Lens' Backup (Lude.Maybe Lude.Timestamp)
bCopyTimestamp = Lens.lens (copyTimestamp :: Backup -> Lude.Maybe Lude.Timestamp) (\s a -> s {copyTimestamp = a} :: Backup)
{-# DEPRECATED bCopyTimestamp "Use generic-lens or generic-optics with 'copyTimestamp' instead." #-}

-- | The state of the backup.
--
-- /Note:/ Consider using 'backupState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupState :: Lens.Lens' Backup (Lude.Maybe BackupState)
bBackupState = Lens.lens (backupState :: Backup -> Lude.Maybe BackupState) (\s a -> s {backupState = a} :: Backup)
{-# DEPRECATED bBackupState "Use generic-lens or generic-optics with 'backupState' instead." #-}

-- | The identifier (ID) of the backup.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupId :: Lens.Lens' Backup Lude.Text
bBackupId = Lens.lens (backupId :: Backup -> Lude.Text) (\s a -> s {backupId = a} :: Backup)
{-# DEPRECATED bBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

instance Lude.FromJSON Backup where
  parseJSON =
    Lude.withObject
      "Backup"
      ( \x ->
          Backup'
            Lude.<$> (x Lude..:? "DeleteTimestamp")
            Lude.<*> (x Lude..:? "SourceCluster")
            Lude.<*> (x Lude..:? "NeverExpires")
            Lude.<*> (x Lude..:? "SourceRegion")
            Lude.<*> (x Lude..:? "TagList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SourceBackup")
            Lude.<*> (x Lude..:? "ClusterId")
            Lude.<*> (x Lude..:? "CreateTimestamp")
            Lude.<*> (x Lude..:? "CopyTimestamp")
            Lude.<*> (x Lude..:? "BackupState")
            Lude.<*> (x Lude..: "BackupId")
      )
