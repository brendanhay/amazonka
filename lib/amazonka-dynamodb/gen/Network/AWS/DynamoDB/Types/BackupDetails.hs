{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.BackupDetails
  ( BackupDetails (..)
  -- * Smart constructor
  , mkBackupDetails
  -- * Lenses
  , bdBackupArn
  , bdBackupName
  , bdBackupStatus
  , bdBackupType
  , bdBackupCreationDateTime
  , bdBackupExpiryDateTime
  , bdBackupSizeBytes
  ) where

import qualified Network.AWS.DynamoDB.Types.BackupArn as Types
import qualified Network.AWS.DynamoDB.Types.BackupName as Types
import qualified Network.AWS.DynamoDB.Types.BackupStatus as Types
import qualified Network.AWS.DynamoDB.Types.BackupType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the details of the backup created for the table.
--
-- /See:/ 'mkBackupDetails' smart constructor.
data BackupDetails = BackupDetails'
  { backupArn :: Types.BackupArn
    -- ^ ARN associated with the backup.
  , backupName :: Types.BackupName
    -- ^ Name of the requested backup.
  , backupStatus :: Types.BackupStatus
    -- ^ Backup can be in one of the following states: CREATING, ACTIVE, DELETED. 
  , backupType :: Types.BackupType
    -- ^ BackupType:
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
  , backupCreationDateTime :: Core.NominalDiffTime
    -- ^ Time at which the backup was created. This is the request time of the backup. 
  , backupExpiryDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
  , backupSizeBytes :: Core.Maybe Core.Natural
    -- ^ Size of the backup in bytes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BackupDetails' value with any optional fields omitted.
mkBackupDetails
    :: Types.BackupArn -- ^ 'backupArn'
    -> Types.BackupName -- ^ 'backupName'
    -> Types.BackupStatus -- ^ 'backupStatus'
    -> Types.BackupType -- ^ 'backupType'
    -> Core.NominalDiffTime -- ^ 'backupCreationDateTime'
    -> BackupDetails
mkBackupDetails backupArn backupName backupStatus backupType
  backupCreationDateTime
  = BackupDetails'{backupArn, backupName, backupStatus, backupType,
                   backupCreationDateTime, backupExpiryDateTime = Core.Nothing,
                   backupSizeBytes = Core.Nothing}

-- | ARN associated with the backup.
--
-- /Note:/ Consider using 'backupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupArn :: Lens.Lens' BackupDetails Types.BackupArn
bdBackupArn = Lens.field @"backupArn"
{-# INLINEABLE bdBackupArn #-}
{-# DEPRECATED backupArn "Use generic-lens or generic-optics with 'backupArn' instead"  #-}

-- | Name of the requested backup.
--
-- /Note:/ Consider using 'backupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupName :: Lens.Lens' BackupDetails Types.BackupName
bdBackupName = Lens.field @"backupName"
{-# INLINEABLE bdBackupName #-}
{-# DEPRECATED backupName "Use generic-lens or generic-optics with 'backupName' instead"  #-}

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED. 
--
-- /Note:/ Consider using 'backupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupStatus :: Lens.Lens' BackupDetails Types.BackupStatus
bdBackupStatus = Lens.field @"backupStatus"
{-# INLINEABLE bdBackupStatus #-}
{-# DEPRECATED backupStatus "Use generic-lens or generic-optics with 'backupStatus' instead"  #-}

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
bdBackupType :: Lens.Lens' BackupDetails Types.BackupType
bdBackupType = Lens.field @"backupType"
{-# INLINEABLE bdBackupType #-}
{-# DEPRECATED backupType "Use generic-lens or generic-optics with 'backupType' instead"  #-}

-- | Time at which the backup was created. This is the request time of the backup. 
--
-- /Note:/ Consider using 'backupCreationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupCreationDateTime :: Lens.Lens' BackupDetails Core.NominalDiffTime
bdBackupCreationDateTime = Lens.field @"backupCreationDateTime"
{-# INLINEABLE bdBackupCreationDateTime #-}
{-# DEPRECATED backupCreationDateTime "Use generic-lens or generic-optics with 'backupCreationDateTime' instead"  #-}

-- | Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
--
-- /Note:/ Consider using 'backupExpiryDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupExpiryDateTime :: Lens.Lens' BackupDetails (Core.Maybe Core.NominalDiffTime)
bdBackupExpiryDateTime = Lens.field @"backupExpiryDateTime"
{-# INLINEABLE bdBackupExpiryDateTime #-}
{-# DEPRECATED backupExpiryDateTime "Use generic-lens or generic-optics with 'backupExpiryDateTime' instead"  #-}

-- | Size of the backup in bytes.
--
-- /Note:/ Consider using 'backupSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupSizeBytes :: Lens.Lens' BackupDetails (Core.Maybe Core.Natural)
bdBackupSizeBytes = Lens.field @"backupSizeBytes"
{-# INLINEABLE bdBackupSizeBytes #-}
{-# DEPRECATED backupSizeBytes "Use generic-lens or generic-optics with 'backupSizeBytes' instead"  #-}

instance Core.FromJSON BackupDetails where
        parseJSON
          = Core.withObject "BackupDetails" Core.$
              \ x ->
                BackupDetails' Core.<$>
                  (x Core..: "BackupArn") Core.<*> x Core..: "BackupName" Core.<*>
                    x Core..: "BackupStatus"
                    Core.<*> x Core..: "BackupType"
                    Core.<*> x Core..: "BackupCreationDateTime"
                    Core.<*> x Core..:? "BackupExpiryDateTime"
                    Core.<*> x Core..:? "BackupSizeBytes"
