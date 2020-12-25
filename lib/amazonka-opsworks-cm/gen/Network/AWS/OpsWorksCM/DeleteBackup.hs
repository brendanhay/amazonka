{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DeleteBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a backup. You can delete both manual and automated backups. This operation is asynchronous.
--
-- An @InvalidStateException@ is thrown when a backup deletion is already in progress. A @ResourceNotFoundException@ is thrown when the backup does not exist. A @ValidationException@ is thrown when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.DeleteBackup
  ( -- * Creating a request
    DeleteBackup (..),
    mkDeleteBackup,

    -- ** Request lenses
    dbBackupId,

    -- * Destructuring the response
    DeleteBackupResponse (..),
    mkDeleteBackupResponse,

    -- ** Response lenses
    dbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBackup' smart constructor.
newtype DeleteBackup = DeleteBackup'
  { -- | The ID of the backup to delete. Run the DescribeBackups command to get a list of backup IDs. Backup IDs are in the format @ServerName-yyyyMMddHHmmssSSS@ .
    backupId :: Types.BackupId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBackup' value with any optional fields omitted.
mkDeleteBackup ::
  -- | 'backupId'
  Types.BackupId ->
  DeleteBackup
mkDeleteBackup backupId = DeleteBackup' {backupId}

-- | The ID of the backup to delete. Run the DescribeBackups command to get a list of backup IDs. Backup IDs are in the format @ServerName-yyyyMMddHHmmssSSS@ .
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBackupId :: Lens.Lens' DeleteBackup Types.BackupId
dbBackupId = Lens.field @"backupId"
{-# DEPRECATED dbBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

instance Core.FromJSON DeleteBackup where
  toJSON DeleteBackup {..} =
    Core.object
      (Core.catMaybes [Core.Just ("BackupId" Core..= backupId)])

instance Core.AWSRequest DeleteBackup where
  type Rs DeleteBackup = DeleteBackupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorksCM_V2016_11_01.DeleteBackup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBackupResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteBackupResponse' smart constructor.
newtype DeleteBackupResponse = DeleteBackupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBackupResponse' value with any optional fields omitted.
mkDeleteBackupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteBackupResponse
mkDeleteBackupResponse responseStatus =
  DeleteBackupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsResponseStatus :: Lens.Lens' DeleteBackupResponse Core.Int
dbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
