{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteResourceDataSync (..)
    , mkDeleteResourceDataSync
    -- ** Request lenses
    , drdsSyncName
    , drdsSyncType

    -- * Destructuring the response
    , DeleteResourceDataSyncResponse (..)
    , mkDeleteResourceDataSyncResponse
    -- ** Response lenses
    , drdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeleteResourceDataSync' smart constructor.
data DeleteResourceDataSync = DeleteResourceDataSync'
  { syncName :: Types.SyncName
    -- ^ The name of the configuration to delete.
  , syncType :: Core.Maybe Types.SyncType
    -- ^ Specify the type of resource data sync to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceDataSync' value with any optional fields omitted.
mkDeleteResourceDataSync
    :: Types.SyncName -- ^ 'syncName'
    -> DeleteResourceDataSync
mkDeleteResourceDataSync syncName
  = DeleteResourceDataSync'{syncName, syncType = Core.Nothing}

-- | The name of the configuration to delete.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsSyncName :: Lens.Lens' DeleteResourceDataSync Types.SyncName
drdsSyncName = Lens.field @"syncName"
{-# INLINEABLE drdsSyncName #-}
{-# DEPRECATED syncName "Use generic-lens or generic-optics with 'syncName' instead"  #-}

-- | Specify the type of resource data sync to delete.
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsSyncType :: Lens.Lens' DeleteResourceDataSync (Core.Maybe Types.SyncType)
drdsSyncType = Lens.field @"syncType"
{-# INLINEABLE drdsSyncType #-}
{-# DEPRECATED syncType "Use generic-lens or generic-optics with 'syncType' instead"  #-}

instance Core.ToQuery DeleteResourceDataSync where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteResourceDataSync where
        toHeaders DeleteResourceDataSync{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DeleteResourceDataSync")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteResourceDataSync where
        toJSON DeleteResourceDataSync{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SyncName" Core..= syncName),
                  ("SyncType" Core..=) Core.<$> syncType])

instance Core.AWSRequest DeleteResourceDataSync where
        type Rs DeleteResourceDataSync = DeleteResourceDataSyncResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteResourceDataSyncResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteResourceDataSyncResponse' smart constructor.
newtype DeleteResourceDataSyncResponse = DeleteResourceDataSyncResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceDataSyncResponse' value with any optional fields omitted.
mkDeleteResourceDataSyncResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteResourceDataSyncResponse
mkDeleteResourceDataSyncResponse responseStatus
  = DeleteResourceDataSyncResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsrrsResponseStatus :: Lens.Lens' DeleteResourceDataSyncResponse Core.Int
drdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
