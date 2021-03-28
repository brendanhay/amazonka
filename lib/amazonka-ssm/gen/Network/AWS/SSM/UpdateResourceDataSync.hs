{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateResourceDataSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a resource data sync. After you create a resource data sync for a Region, you can't change the account options for that sync. For example, if you create a sync in the us-east-2 (Ohio) Region and you choose the Include only the current account option, you can't edit that sync later and choose the Include all accounts from my AWS Organizations configuration option. Instead, you must delete the first resource data sync, and create a new one.
module Network.AWS.SSM.UpdateResourceDataSync
    (
    -- * Creating a request
      UpdateResourceDataSync (..)
    , mkUpdateResourceDataSync
    -- ** Request lenses
    , urdsSyncName
    , urdsSyncType
    , urdsSyncSource

    -- * Destructuring the response
    , UpdateResourceDataSyncResponse (..)
    , mkUpdateResourceDataSyncResponse
    -- ** Response lenses
    , urdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdateResourceDataSync' smart constructor.
data UpdateResourceDataSync = UpdateResourceDataSync'
  { syncName :: Types.SyncName
    -- ^ The name of the resource data sync you want to update.
  , syncType :: Types.SyncType
    -- ^ The type of resource data sync. The supported @SyncType@ is SyncFromSource.
  , syncSource :: Types.ResourceDataSyncSource
    -- ^ Specify information about the data sources to synchronize.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResourceDataSync' value with any optional fields omitted.
mkUpdateResourceDataSync
    :: Types.SyncName -- ^ 'syncName'
    -> Types.SyncType -- ^ 'syncType'
    -> Types.ResourceDataSyncSource -- ^ 'syncSource'
    -> UpdateResourceDataSync
mkUpdateResourceDataSync syncName syncType syncSource
  = UpdateResourceDataSync'{syncName, syncType, syncSource}

-- | The name of the resource data sync you want to update.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdsSyncName :: Lens.Lens' UpdateResourceDataSync Types.SyncName
urdsSyncName = Lens.field @"syncName"
{-# INLINEABLE urdsSyncName #-}
{-# DEPRECATED syncName "Use generic-lens or generic-optics with 'syncName' instead"  #-}

-- | The type of resource data sync. The supported @SyncType@ is SyncFromSource.
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdsSyncType :: Lens.Lens' UpdateResourceDataSync Types.SyncType
urdsSyncType = Lens.field @"syncType"
{-# INLINEABLE urdsSyncType #-}
{-# DEPRECATED syncType "Use generic-lens or generic-optics with 'syncType' instead"  #-}

-- | Specify information about the data sources to synchronize.
--
-- /Note:/ Consider using 'syncSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdsSyncSource :: Lens.Lens' UpdateResourceDataSync Types.ResourceDataSyncSource
urdsSyncSource = Lens.field @"syncSource"
{-# INLINEABLE urdsSyncSource #-}
{-# DEPRECATED syncSource "Use generic-lens or generic-optics with 'syncSource' instead"  #-}

instance Core.ToQuery UpdateResourceDataSync where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateResourceDataSync where
        toHeaders UpdateResourceDataSync{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.UpdateResourceDataSync")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateResourceDataSync where
        toJSON UpdateResourceDataSync{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SyncName" Core..= syncName),
                  Core.Just ("SyncType" Core..= syncType),
                  Core.Just ("SyncSource" Core..= syncSource)])

instance Core.AWSRequest UpdateResourceDataSync where
        type Rs UpdateResourceDataSync = UpdateResourceDataSyncResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateResourceDataSyncResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateResourceDataSyncResponse' smart constructor.
newtype UpdateResourceDataSyncResponse = UpdateResourceDataSyncResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResourceDataSyncResponse' value with any optional fields omitted.
mkUpdateResourceDataSyncResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateResourceDataSyncResponse
mkUpdateResourceDataSyncResponse responseStatus
  = UpdateResourceDataSyncResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdsrrsResponseStatus :: Lens.Lens' UpdateResourceDataSyncResponse Core.Int
urdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
