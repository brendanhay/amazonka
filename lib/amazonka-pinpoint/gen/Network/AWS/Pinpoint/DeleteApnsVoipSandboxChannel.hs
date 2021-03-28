{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteApnsVoipSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs VoIP sandbox channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteApnsVoipSandboxChannel
    (
    -- * Creating a request
      DeleteApnsVoipSandboxChannel (..)
    , mkDeleteApnsVoipSandboxChannel
    -- ** Request lenses
    , davscApplicationId

    -- * Destructuring the response
    , DeleteApnsVoipSandboxChannelResponse (..)
    , mkDeleteApnsVoipSandboxChannelResponse
    -- ** Response lenses
    , davscrrsAPNSVoipSandboxChannelResponse
    , davscrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApnsVoipSandboxChannel' smart constructor.
newtype DeleteApnsVoipSandboxChannel = DeleteApnsVoipSandboxChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApnsVoipSandboxChannel' value with any optional fields omitted.
mkDeleteApnsVoipSandboxChannel
    :: Core.Text -- ^ 'applicationId'
    -> DeleteApnsVoipSandboxChannel
mkDeleteApnsVoipSandboxChannel applicationId
  = DeleteApnsVoipSandboxChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davscApplicationId :: Lens.Lens' DeleteApnsVoipSandboxChannel Core.Text
davscApplicationId = Lens.field @"applicationId"
{-# INLINEABLE davscApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DeleteApnsVoipSandboxChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApnsVoipSandboxChannel where
        toHeaders DeleteApnsVoipSandboxChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteApnsVoipSandboxChannel where
        type Rs DeleteApnsVoipSandboxChannel =
             DeleteApnsVoipSandboxChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns_voip_sandbox",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteApnsVoipSandboxChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApnsVoipSandboxChannelResponse' smart constructor.
data DeleteApnsVoipSandboxChannelResponse = DeleteApnsVoipSandboxChannelResponse'
  { aPNSVoipSandboxChannelResponse :: Types.APNSVoipSandboxChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApnsVoipSandboxChannelResponse' value with any optional fields omitted.
mkDeleteApnsVoipSandboxChannelResponse
    :: Types.APNSVoipSandboxChannelResponse -- ^ 'aPNSVoipSandboxChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteApnsVoipSandboxChannelResponse
mkDeleteApnsVoipSandboxChannelResponse
  aPNSVoipSandboxChannelResponse responseStatus
  = DeleteApnsVoipSandboxChannelResponse'{aPNSVoipSandboxChannelResponse,
                                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSVoipSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davscrrsAPNSVoipSandboxChannelResponse :: Lens.Lens' DeleteApnsVoipSandboxChannelResponse Types.APNSVoipSandboxChannelResponse
davscrrsAPNSVoipSandboxChannelResponse = Lens.field @"aPNSVoipSandboxChannelResponse"
{-# INLINEABLE davscrrsAPNSVoipSandboxChannelResponse #-}
{-# DEPRECATED aPNSVoipSandboxChannelResponse "Use generic-lens or generic-optics with 'aPNSVoipSandboxChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davscrrsResponseStatus :: Lens.Lens' DeleteApnsVoipSandboxChannelResponse Core.Int
davscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE davscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
