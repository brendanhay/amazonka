{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApnsVoipSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs VoIP sandbox channel for an application.
module Network.AWS.Pinpoint.GetApnsVoipSandboxChannel
    (
    -- * Creating a request
      GetApnsVoipSandboxChannel (..)
    , mkGetApnsVoipSandboxChannel
    -- ** Request lenses
    , gavscApplicationId

    -- * Destructuring the response
    , GetApnsVoipSandboxChannelResponse (..)
    , mkGetApnsVoipSandboxChannelResponse
    -- ** Response lenses
    , gavscrrsAPNSVoipSandboxChannelResponse
    , gavscrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetApnsVoipSandboxChannel' smart constructor.
newtype GetApnsVoipSandboxChannel = GetApnsVoipSandboxChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApnsVoipSandboxChannel' value with any optional fields omitted.
mkGetApnsVoipSandboxChannel
    :: Core.Text -- ^ 'applicationId'
    -> GetApnsVoipSandboxChannel
mkGetApnsVoipSandboxChannel applicationId
  = GetApnsVoipSandboxChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavscApplicationId :: Lens.Lens' GetApnsVoipSandboxChannel Core.Text
gavscApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gavscApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetApnsVoipSandboxChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApnsVoipSandboxChannel where
        toHeaders GetApnsVoipSandboxChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetApnsVoipSandboxChannel where
        type Rs GetApnsVoipSandboxChannel =
             GetApnsVoipSandboxChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns_voip_sandbox",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetApnsVoipSandboxChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetApnsVoipSandboxChannelResponse' smart constructor.
data GetApnsVoipSandboxChannelResponse = GetApnsVoipSandboxChannelResponse'
  { aPNSVoipSandboxChannelResponse :: Types.APNSVoipSandboxChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApnsVoipSandboxChannelResponse' value with any optional fields omitted.
mkGetApnsVoipSandboxChannelResponse
    :: Types.APNSVoipSandboxChannelResponse -- ^ 'aPNSVoipSandboxChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetApnsVoipSandboxChannelResponse
mkGetApnsVoipSandboxChannelResponse aPNSVoipSandboxChannelResponse
  responseStatus
  = GetApnsVoipSandboxChannelResponse'{aPNSVoipSandboxChannelResponse,
                                       responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSVoipSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavscrrsAPNSVoipSandboxChannelResponse :: Lens.Lens' GetApnsVoipSandboxChannelResponse Types.APNSVoipSandboxChannelResponse
gavscrrsAPNSVoipSandboxChannelResponse = Lens.field @"aPNSVoipSandboxChannelResponse"
{-# INLINEABLE gavscrrsAPNSVoipSandboxChannelResponse #-}
{-# DEPRECATED aPNSVoipSandboxChannelResponse "Use generic-lens or generic-optics with 'aPNSVoipSandboxChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavscrrsResponseStatus :: Lens.Lens' GetApnsVoipSandboxChannelResponse Core.Int
gavscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gavscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
