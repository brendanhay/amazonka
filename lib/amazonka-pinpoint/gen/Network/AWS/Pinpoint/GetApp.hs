{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an application.
module Network.AWS.Pinpoint.GetApp
    (
    -- * Creating a request
      GetApp (..)
    , mkGetApp
    -- ** Request lenses
    , gaApplicationId

    -- * Destructuring the response
    , GetAppResponse (..)
    , mkGetAppResponse
    -- ** Response lenses
    , garrsApplicationResponse
    , garrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetApp' smart constructor.
newtype GetApp = GetApp'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApp' value with any optional fields omitted.
mkGetApp
    :: Core.Text -- ^ 'applicationId'
    -> GetApp
mkGetApp applicationId = GetApp'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaApplicationId :: Lens.Lens' GetApp Core.Text
gaApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gaApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetApp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApp where
        toHeaders GetApp{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetApp where
        type Rs GetApp = GetAppResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/v1/apps/" Core.<> Core.toText applicationId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAppResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
  { applicationResponse :: Types.ApplicationResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppResponse' value with any optional fields omitted.
mkGetAppResponse
    :: Types.ApplicationResponse -- ^ 'applicationResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetAppResponse
mkGetAppResponse applicationResponse responseStatus
  = GetAppResponse'{applicationResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsApplicationResponse :: Lens.Lens' GetAppResponse Types.ApplicationResponse
garrsApplicationResponse = Lens.field @"applicationResponse"
{-# INLINEABLE garrsApplicationResponse #-}
{-# DEPRECATED applicationResponse "Use generic-lens or generic-optics with 'applicationResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAppResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
