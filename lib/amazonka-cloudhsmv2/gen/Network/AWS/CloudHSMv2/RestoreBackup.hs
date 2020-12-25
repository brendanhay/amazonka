{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RestoreBackup (..),
    mkRestoreBackup,

    -- ** Request lenses
    rbBackupId,

    -- * Destructuring the response
    RestoreBackupResponse (..),
    mkRestoreBackupResponse,

    -- ** Response lenses
    rbrrsBackup,
    rbrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreBackup' smart constructor.
newtype RestoreBackup = RestoreBackup'
  { -- | The ID of the backup to be restored. To find the ID of a backup, use the 'DescribeBackups' operation.
    backupId :: Types.BackupId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreBackup' value with any optional fields omitted.
mkRestoreBackup ::
  -- | 'backupId'
  Types.BackupId ->
  RestoreBackup
mkRestoreBackup backupId = RestoreBackup' {backupId}

-- | The ID of the backup to be restored. To find the ID of a backup, use the 'DescribeBackups' operation.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbBackupId :: Lens.Lens' RestoreBackup Types.BackupId
rbBackupId = Lens.field @"backupId"
{-# DEPRECATED rbBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

instance Core.FromJSON RestoreBackup where
  toJSON RestoreBackup {..} =
    Core.object
      (Core.catMaybes [Core.Just ("BackupId" Core..= backupId)])

instance Core.AWSRequest RestoreBackup where
  type Rs RestoreBackup = RestoreBackupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "BaldrApiService.RestoreBackup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreBackupResponse'
            Core.<$> (x Core..:? "Backup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreBackupResponse' smart constructor.
data RestoreBackupResponse = RestoreBackupResponse'
  { -- | Information on the @Backup@ object created.
    backup :: Core.Maybe Types.Backup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RestoreBackupResponse' value with any optional fields omitted.
mkRestoreBackupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreBackupResponse
mkRestoreBackupResponse responseStatus =
  RestoreBackupResponse' {backup = Core.Nothing, responseStatus}

-- | Information on the @Backup@ object created.
--
-- /Note:/ Consider using 'backup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrrsBackup :: Lens.Lens' RestoreBackupResponse (Core.Maybe Types.Backup)
rbrrsBackup = Lens.field @"backup"
{-# DEPRECATED rbrrsBackup "Use generic-lens or generic-optics with 'backup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrrsResponseStatus :: Lens.Lens' RestoreBackupResponse Core.Int
rbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
