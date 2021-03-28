{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateApnsVoipSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs VoIP sandbox channel for an application or updates the status and settings of the APNs VoIP sandbox channel for an application.
module Network.AWS.Pinpoint.UpdateApnsVoipSandboxChannel
    (
    -- * Creating a request
      UpdateApnsVoipSandboxChannel (..)
    , mkUpdateApnsVoipSandboxChannel
    -- ** Request lenses
    , uavscApplicationId
    , uavscAPNSVoipSandboxChannelRequest

    -- * Destructuring the response
    , UpdateApnsVoipSandboxChannelResponse (..)
    , mkUpdateApnsVoipSandboxChannelResponse
    -- ** Response lenses
    , uavscrrsAPNSVoipSandboxChannelResponse
    , uavscrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApnsVoipSandboxChannel' smart constructor.
data UpdateApnsVoipSandboxChannel = UpdateApnsVoipSandboxChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , aPNSVoipSandboxChannelRequest :: Types.APNSVoipSandboxChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApnsVoipSandboxChannel' value with any optional fields omitted.
mkUpdateApnsVoipSandboxChannel
    :: Core.Text -- ^ 'applicationId'
    -> Types.APNSVoipSandboxChannelRequest -- ^ 'aPNSVoipSandboxChannelRequest'
    -> UpdateApnsVoipSandboxChannel
mkUpdateApnsVoipSandboxChannel applicationId
  aPNSVoipSandboxChannelRequest
  = UpdateApnsVoipSandboxChannel'{applicationId,
                                  aPNSVoipSandboxChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavscApplicationId :: Lens.Lens' UpdateApnsVoipSandboxChannel Core.Text
uavscApplicationId = Lens.field @"applicationId"
{-# INLINEABLE uavscApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSVoipSandboxChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavscAPNSVoipSandboxChannelRequest :: Lens.Lens' UpdateApnsVoipSandboxChannel Types.APNSVoipSandboxChannelRequest
uavscAPNSVoipSandboxChannelRequest = Lens.field @"aPNSVoipSandboxChannelRequest"
{-# INLINEABLE uavscAPNSVoipSandboxChannelRequest #-}
{-# DEPRECATED aPNSVoipSandboxChannelRequest "Use generic-lens or generic-optics with 'aPNSVoipSandboxChannelRequest' instead"  #-}

instance Core.ToQuery UpdateApnsVoipSandboxChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApnsVoipSandboxChannel where
        toHeaders UpdateApnsVoipSandboxChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApnsVoipSandboxChannel where
        toJSON UpdateApnsVoipSandboxChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("APNSVoipSandboxChannelRequest" Core..=
                       aPNSVoipSandboxChannelRequest)])

instance Core.AWSRequest UpdateApnsVoipSandboxChannel where
        type Rs UpdateApnsVoipSandboxChannel =
             UpdateApnsVoipSandboxChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns_voip_sandbox",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateApnsVoipSandboxChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateApnsVoipSandboxChannelResponse' smart constructor.
data UpdateApnsVoipSandboxChannelResponse = UpdateApnsVoipSandboxChannelResponse'
  { aPNSVoipSandboxChannelResponse :: Types.APNSVoipSandboxChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApnsVoipSandboxChannelResponse' value with any optional fields omitted.
mkUpdateApnsVoipSandboxChannelResponse
    :: Types.APNSVoipSandboxChannelResponse -- ^ 'aPNSVoipSandboxChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateApnsVoipSandboxChannelResponse
mkUpdateApnsVoipSandboxChannelResponse
  aPNSVoipSandboxChannelResponse responseStatus
  = UpdateApnsVoipSandboxChannelResponse'{aPNSVoipSandboxChannelResponse,
                                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSVoipSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavscrrsAPNSVoipSandboxChannelResponse :: Lens.Lens' UpdateApnsVoipSandboxChannelResponse Types.APNSVoipSandboxChannelResponse
uavscrrsAPNSVoipSandboxChannelResponse = Lens.field @"aPNSVoipSandboxChannelResponse"
{-# INLINEABLE uavscrrsAPNSVoipSandboxChannelResponse #-}
{-# DEPRECATED aPNSVoipSandboxChannelResponse "Use generic-lens or generic-optics with 'aPNSVoipSandboxChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavscrrsResponseStatus :: Lens.Lens' UpdateApnsVoipSandboxChannelResponse Core.Int
uavscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uavscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
