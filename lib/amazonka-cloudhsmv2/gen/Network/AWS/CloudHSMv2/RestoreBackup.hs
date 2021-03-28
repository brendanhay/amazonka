{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.RestoreBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a specified AWS CloudHSM backup that is in the @PENDING_DELETION@ state. For mor information on deleting a backup, see 'DeleteBackup' .
module Network.AWS.CloudHSMv2.RestoreBackup
    (
    -- * Creating a request
      RestoreBackup (..)
    , mkRestoreBackup
    -- ** Request lenses
    , rbBackupId

    -- * Destructuring the response
    , RestoreBackupResponse (..)
    , mkRestoreBackupResponse
    -- ** Response lenses
    , rbrrsBackup
    , rbrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreBackup' smart constructor.
newtype RestoreBackup = RestoreBackup'
  { backupId :: Types.BackupId
    -- ^ The ID of the backup to be restored. To find the ID of a backup, use the 'DescribeBackups' operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreBackup' value with any optional fields omitted.
mkRestoreBackup
    :: Types.BackupId -- ^ 'backupId'
    -> RestoreBackup
mkRestoreBackup backupId = RestoreBackup'{backupId}

-- | The ID of the backup to be restored. To find the ID of a backup, use the 'DescribeBackups' operation.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbBackupId :: Lens.Lens' RestoreBackup Types.BackupId
rbBackupId = Lens.field @"backupId"
{-# INLINEABLE rbBackupId #-}
{-# DEPRECATED backupId "Use generic-lens or generic-optics with 'backupId' instead"  #-}

instance Core.ToQuery RestoreBackup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RestoreBackup where
        toHeaders RestoreBackup{..}
          = Core.pure ("X-Amz-Target", "BaldrApiService.RestoreBackup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RestoreBackup where
        toJSON RestoreBackup{..}
          = Core.object
              (Core.catMaybes [Core.Just ("BackupId" Core..= backupId)])

instance Core.AWSRequest RestoreBackup where
        type Rs RestoreBackup = RestoreBackupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RestoreBackupResponse' Core.<$>
                   (x Core..:? "Backup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreBackupResponse' smart constructor.
data RestoreBackupResponse = RestoreBackupResponse'
  { backup :: Core.Maybe Types.Backup
    -- ^ Information on the @Backup@ object created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreBackupResponse' value with any optional fields omitted.
mkRestoreBackupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreBackupResponse
mkRestoreBackupResponse responseStatus
  = RestoreBackupResponse'{backup = Core.Nothing, responseStatus}

-- | Information on the @Backup@ object created.
--
-- /Note:/ Consider using 'backup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrrsBackup :: Lens.Lens' RestoreBackupResponse (Core.Maybe Types.Backup)
rbrrsBackup = Lens.field @"backup"
{-# INLINEABLE rbrrsBackup #-}
{-# DEPRECATED backup "Use generic-lens or generic-optics with 'backup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrrsResponseStatus :: Lens.Lens' RestoreBackupResponse Core.Int
rbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
