{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetGcmChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the GCM channel for an application.
module Network.AWS.Pinpoint.GetGcmChannel
    (
    -- * Creating a request
      GetGcmChannel (..)
    , mkGetGcmChannel
    -- ** Request lenses
    , ggcApplicationId

    -- * Destructuring the response
    , GetGcmChannelResponse (..)
    , mkGetGcmChannelResponse
    -- ** Response lenses
    , ggcrrsGCMChannelResponse
    , ggcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGcmChannel' smart constructor.
newtype GetGcmChannel = GetGcmChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetGcmChannel' value with any optional fields omitted.
mkGetGcmChannel
    :: Core.Text -- ^ 'applicationId'
    -> GetGcmChannel
mkGetGcmChannel applicationId = GetGcmChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcApplicationId :: Lens.Lens' GetGcmChannel Core.Text
ggcApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ggcApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetGcmChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetGcmChannel where
        toHeaders GetGcmChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetGcmChannel where
        type Rs GetGcmChannel = GetGcmChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/gcm",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetGcmChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetGcmChannelResponse' smart constructor.
data GetGcmChannelResponse = GetGcmChannelResponse'
  { gCMChannelResponse :: Types.GCMChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGcmChannelResponse' value with any optional fields omitted.
mkGetGcmChannelResponse
    :: Types.GCMChannelResponse -- ^ 'gCMChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetGcmChannelResponse
mkGetGcmChannelResponse gCMChannelResponse responseStatus
  = GetGcmChannelResponse'{gCMChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gCMChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcrrsGCMChannelResponse :: Lens.Lens' GetGcmChannelResponse Types.GCMChannelResponse
ggcrrsGCMChannelResponse = Lens.field @"gCMChannelResponse"
{-# INLINEABLE ggcrrsGCMChannelResponse #-}
{-# DEPRECATED gCMChannelResponse "Use generic-lens or generic-optics with 'gCMChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcrrsResponseStatus :: Lens.Lens' GetGcmChannelResponse Core.Int
ggcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
