{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about the specified application.
module Network.AWS.SMS.GetApp
    (
    -- * Creating a request
      GetApp (..)
    , mkGetApp
    -- ** Request lenses
    , gaAppId

    -- * Destructuring the response
    , GetAppResponse (..)
    , mkGetAppResponse
    -- ** Response lenses
    , garrsAppSummary
    , garrsServerGroups
    , garrsTags
    , garrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetApp' smart constructor.
newtype GetApp = GetApp'
  { appId :: Core.Maybe Types.AppId
    -- ^ The ID of the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApp' value with any optional fields omitted.
mkGetApp
    :: GetApp
mkGetApp = GetApp'{appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAppId :: Lens.Lens' GetApp (Core.Maybe Types.AppId)
gaAppId = Lens.field @"appId"
{-# INLINEABLE gaAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

instance Core.ToQuery GetApp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApp where
        toHeaders GetApp{..}
          = Core.pure
              ("X-Amz-Target", "AWSServerMigrationService_V2016_10_24.GetApp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetApp where
        toJSON GetApp{..}
          = Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest GetApp where
        type Rs GetApp = GetAppResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAppResponse' Core.<$>
                   (x Core..:? "appSummary") Core.<*> x Core..:? "serverGroups"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
  { appSummary :: Core.Maybe Types.AppSummary
    -- ^ Information about the application.
  , serverGroups :: Core.Maybe [Types.ServerGroup]
    -- ^ The server groups that belong to the application.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags associated with the application.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetAppResponse' value with any optional fields omitted.
mkGetAppResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAppResponse
mkGetAppResponse responseStatus
  = GetAppResponse'{appSummary = Core.Nothing,
                    serverGroups = Core.Nothing, tags = Core.Nothing, responseStatus}

-- | Information about the application.
--
-- /Note:/ Consider using 'appSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsAppSummary :: Lens.Lens' GetAppResponse (Core.Maybe Types.AppSummary)
garrsAppSummary = Lens.field @"appSummary"
{-# INLINEABLE garrsAppSummary #-}
{-# DEPRECATED appSummary "Use generic-lens or generic-optics with 'appSummary' instead"  #-}

-- | The server groups that belong to the application.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsServerGroups :: Lens.Lens' GetAppResponse (Core.Maybe [Types.ServerGroup])
garrsServerGroups = Lens.field @"serverGroups"
{-# INLINEABLE garrsServerGroups #-}
{-# DEPRECATED serverGroups "Use generic-lens or generic-optics with 'serverGroups' instead"  #-}

-- | The tags associated with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsTags :: Lens.Lens' GetAppResponse (Core.Maybe [Types.Tag])
garrsTags = Lens.field @"tags"
{-# INLINEABLE garrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAppResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
