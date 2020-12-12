{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupDetails
  ( BackupDetails (..),

    -- * Smart constructor
    mkBackupDetails,

    -- * Lenses
    bdBackupExpiryDateTime,
    bdBackupSizeBytes,
    bdBackupARN,
    bdBackupName,
    bdBackupStatus,
    bdBackupType,
    bdBackupCreationDateTime,
  )
where

import Network.AWS.DynamoDB.Types.BackupStatus
import Network.AWS.DynamoDB.Types.BackupType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the details of the backup created for the table.
--
-- /See:/ 'mkBackupDetails' smart constructor.
data BackupDetails = BackupDetails'
  { backupExpiryDateTime ::
      Lude.Maybe Lude.Timestamp,
    backupSizeBytes :: Lude.Maybe Lude.Natural,
    backupARN :: Lude.Text,
    backupName :: Lude.Text,
    backupStatus :: BackupStatus,
    backupType :: BackupType,
    backupCreationDateTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BackupDetails' with the minimum fields required to make a request.
--
-- * 'backupARN' - ARN associated with the backup.
-- * 'backupCreationDateTime' - Time at which the backup was created. This is the request time of the backup.
-- * 'backupExpiryDateTime' - Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
-- * 'backupName' - Name of the requested backup.
-- * 'backupSizeBytes' - Size of the backup in bytes.
-- * 'backupStatus' - Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
-- * 'backupType' - BackupType:
--
--
--     * @USER@ - You create and manage these using the on-demand backup feature.
--
--
--     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.
--
--
--     * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
mkBackupDetails ::
  -- | 'backupARN'
  Lude.Text ->
  -- | 'backupName'
  Lude.Text ->
  -- | 'backupStatus'
  BackupStatus ->
  -- | 'backupType'
  BackupType ->
  -- | 'backupCreationDateTime'
  Lude.Timestamp ->
  BackupDetails
mkBackupDetails
  pBackupARN_
  pBackupName_
  pBackupStatus_
  pBackupType_
  pBackupCreationDateTime_ =
    BackupDetails'
      { backupExpiryDateTime = Lude.Nothing,
        backupSizeBytes = Lude.Nothing,
        backupARN = pBackupARN_,
        backupName = pBackupName_,
        backupStatus = pBackupStatus_,
        backupType = pBackupType_,
        backupCreationDateTime = pBackupCreationDateTime_
      }

-- | Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
--
-- /Note:/ Consider using 'backupExpiryDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupExpiryDateTime :: Lens.Lens' BackupDetails (Lude.Maybe Lude.Timestamp)
bdBackupExpiryDateTime = Lens.lens (backupExpiryDateTime :: BackupDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {backupExpiryDateTime = a} :: BackupDetails)
{-# DEPRECATED bdBackupExpiryDateTime "Use generic-lens or generic-optics with 'backupExpiryDateTime' instead." #-}

-- | Size of the backup in bytes.
--
-- /Note:/ Consider using 'backupSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupSizeBytes :: Lens.Lens' BackupDetails (Lude.Maybe Lude.Natural)
bdBackupSizeBytes = Lens.lens (backupSizeBytes :: BackupDetails -> Lude.Maybe Lude.Natural) (\s a -> s {backupSizeBytes = a} :: BackupDetails)
{-# DEPRECATED bdBackupSizeBytes "Use generic-lens or generic-optics with 'backupSizeBytes' instead." #-}

-- | ARN associated with the backup.
--
-- /Note:/ Consider using 'backupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupARN :: Lens.Lens' BackupDetails Lude.Text
bdBackupARN = Lens.lens (backupARN :: BackupDetails -> Lude.Text) (\s a -> s {backupARN = a} :: BackupDetails)
{-# DEPRECATED bdBackupARN "Use generic-lens or generic-optics with 'backupARN' instead." #-}

-- | Name of the requested backup.
--
-- /Note:/ Consider using 'backupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupName :: Lens.Lens' BackupDetails Lude.Text
bdBackupName = Lens.lens (backupName :: BackupDetails -> Lude.Text) (\s a -> s {backupName = a} :: BackupDetails)
{-# DEPRECATED bdBackupName "Use generic-lens or generic-optics with 'backupName' instead." #-}

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
--
-- /Note:/ Consider using 'backupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupStatus :: Lens.Lens' BackupDetails BackupStatus
bdBackupStatus = Lens.lens (backupStatus :: BackupDetails -> BackupStatus) (\s a -> s {backupStatus = a} :: BackupDetails)
{-# DEPRECATED bdBackupStatus "Use generic-lens or generic-optics with 'backupStatus' instead." #-}

-- | BackupType:
--
--
--     * @USER@ - You create and manage these using the on-demand backup feature.
--
--
--     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.
--
--
--     * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
--
--
--
-- /Note:/ Consider using 'backupType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupType :: Lens.Lens' BackupDetails BackupType
bdBackupType = Lens.lens (backupType :: BackupDetails -> BackupType) (\s a -> s {backupType = a} :: BackupDetails)
{-# DEPRECATED bdBackupType "Use generic-lens or generic-optics with 'backupType' instead." #-}

-- | Time at which the backup was created. This is the request time of the backup.
--
-- /Note:/ Consider using 'backupCreationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupCreationDateTime :: Lens.Lens' BackupDetails Lude.Timestamp
bdBackupCreationDateTime = Lens.lens (backupCreationDateTime :: BackupDetails -> Lude.Timestamp) (\s a -> s {backupCreationDateTime = a} :: BackupDetails)
{-# DEPRECATED bdBackupCreationDateTime "Use generic-lens or generic-optics with 'backupCreationDateTime' instead." #-}

instance Lude.FromJSON BackupDetails where
  parseJSON =
    Lude.withObject
      "BackupDetails"
      ( \x ->
          BackupDetails'
            Lude.<$> (x Lude..:? "BackupExpiryDateTime")
            Lude.<*> (x Lude..:? "BackupSizeBytes")
            Lude.<*> (x Lude..: "BackupArn")
            Lude.<*> (x Lude..: "BackupName")
            Lude.<*> (x Lude..: "BackupStatus")
            Lude.<*> (x Lude..: "BackupType")
            Lude.<*> (x Lude..: "BackupCreationDateTime")
      )
