{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteResourceDataSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Resource Data Sync configuration. After the configuration is deleted, changes to data on managed instances are no longer synced to or from the target. Deleting a sync configuration does not delete data.
module Network.AWS.SSM.DeleteResourceDataSync
  ( -- * Creating a request
    DeleteResourceDataSync (..),
    mkDeleteResourceDataSync,

    -- ** Request lenses
    drdsSyncName,
    drdsSyncType,

    -- * Destructuring the response
    DeleteResourceDataSyncResponse (..),
    mkDeleteResourceDataSyncResponse,

    -- ** Response lenses
    drdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeleteResourceDataSync' smart constructor.
data DeleteResourceDataSync = DeleteResourceDataSync'
  { -- | The name of the configuration to delete.
    syncName :: Types.SyncName,
    -- | Specify the type of resource data sync to delete.
    syncType :: Core.Maybe Types.SyncType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceDataSync' value with any optional fields omitted.
mkDeleteResourceDataSync ::
  -- | 'syncName'
  Types.SyncName ->
  DeleteResourceDataSync
mkDeleteResourceDataSync syncName =
  DeleteResourceDataSync' {syncName, syncType = Core.Nothing}

-- | The name of the configuration to delete.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsSyncName :: Lens.Lens' DeleteResourceDataSync Types.SyncName
drdsSyncName = Lens.field @"syncName"
{-# DEPRECATED drdsSyncName "Use generic-lens or generic-optics with 'syncName' instead." #-}

-- | Specify the type of resource data sync to delete.
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsSyncType :: Lens.Lens' DeleteResourceDataSync (Core.Maybe Types.SyncType)
drdsSyncType = Lens.field @"syncType"
{-# DEPRECATED drdsSyncType "Use generic-lens or generic-optics with 'syncType' instead." #-}

instance Core.FromJSON DeleteResourceDataSync where
  toJSON DeleteResourceDataSync {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SyncName" Core..= syncName),
            ("SyncType" Core..=) Core.<$> syncType
          ]
      )

instance Core.AWSRequest DeleteResourceDataSync where
  type Rs DeleteResourceDataSync = DeleteResourceDataSyncResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DeleteResourceDataSync")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourceDataSyncResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteResourceDataSyncResponse' smart constructor.
newtype DeleteResourceDataSyncResponse = DeleteResourceDataSyncResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceDataSyncResponse' value with any optional fields omitted.
mkDeleteResourceDataSyncResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteResourceDataSyncResponse
mkDeleteResourceDataSyncResponse responseStatus =
  DeleteResourceDataSyncResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsrrsResponseStatus :: Lens.Lens' DeleteResourceDataSyncResponse Core.Int
drdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
