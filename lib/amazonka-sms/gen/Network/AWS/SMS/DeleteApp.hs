{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Optionally deletes the launched stack associated with the application and all AWS SMS replication jobs for servers in the application.
module Network.AWS.SMS.DeleteApp
    (
    -- * Creating a request
      DeleteApp (..)
    , mkDeleteApp
    -- ** Request lenses
    , daAppId
    , daForceStopAppReplication
    , daForceTerminateApp

    -- * Destructuring the response
    , DeleteAppResponse (..)
    , mkDeleteAppResponse
    -- ** Response lenses
    , darrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { appId :: Core.Maybe Types.AppId
    -- ^ The ID of the application.
  , forceStopAppReplication :: Core.Maybe Core.Bool
    -- ^ Indicates whether to stop all replication jobs corresponding to the servers in the application while deleting the application.
  , forceTerminateApp :: Core.Maybe Core.Bool
    -- ^ Indicates whether to terminate the stack corresponding to the application while deleting the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApp' value with any optional fields omitted.
mkDeleteApp
    :: DeleteApp
mkDeleteApp
  = DeleteApp'{appId = Core.Nothing,
               forceStopAppReplication = Core.Nothing,
               forceTerminateApp = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppId :: Lens.Lens' DeleteApp (Core.Maybe Types.AppId)
daAppId = Lens.field @"appId"
{-# INLINEABLE daAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | Indicates whether to stop all replication jobs corresponding to the servers in the application while deleting the application.
--
-- /Note:/ Consider using 'forceStopAppReplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daForceStopAppReplication :: Lens.Lens' DeleteApp (Core.Maybe Core.Bool)
daForceStopAppReplication = Lens.field @"forceStopAppReplication"
{-# INLINEABLE daForceStopAppReplication #-}
{-# DEPRECATED forceStopAppReplication "Use generic-lens or generic-optics with 'forceStopAppReplication' instead"  #-}

-- | Indicates whether to terminate the stack corresponding to the application while deleting the application.
--
-- /Note:/ Consider using 'forceTerminateApp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daForceTerminateApp :: Lens.Lens' DeleteApp (Core.Maybe Core.Bool)
daForceTerminateApp = Lens.field @"forceTerminateApp"
{-# INLINEABLE daForceTerminateApp #-}
{-# DEPRECATED forceTerminateApp "Use generic-lens or generic-optics with 'forceTerminateApp' instead"  #-}

instance Core.ToQuery DeleteApp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApp where
        toHeaders DeleteApp{..}
          = Core.pure
              ("X-Amz-Target", "AWSServerMigrationService_V2016_10_24.DeleteApp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteApp where
        toJSON DeleteApp{..}
          = Core.object
              (Core.catMaybes
                 [("appId" Core..=) Core.<$> appId,
                  ("forceStopAppReplication" Core..=) Core.<$>
                    forceStopAppReplication,
                  ("forceTerminateApp" Core..=) Core.<$> forceTerminateApp])

instance Core.AWSRequest DeleteApp where
        type Rs DeleteApp = DeleteAppResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteAppResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAppResponse' smart constructor.
newtype DeleteAppResponse = DeleteAppResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppResponse' value with any optional fields omitted.
mkDeleteAppResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteAppResponse
mkDeleteAppResponse responseStatus
  = DeleteAppResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DeleteAppResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
