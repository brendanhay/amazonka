{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateApnsVoipChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs VoIP channel for an application or updates the status and settings of the APNs VoIP channel for an application.
module Network.AWS.Pinpoint.UpdateApnsVoipChannel
    (
    -- * Creating a request
      UpdateApnsVoipChannel (..)
    , mkUpdateApnsVoipChannel
    -- ** Request lenses
    , uavcApplicationId
    , uavcAPNSVoipChannelRequest

    -- * Destructuring the response
    , UpdateApnsVoipChannelResponse (..)
    , mkUpdateApnsVoipChannelResponse
    -- ** Response lenses
    , uavcrrsAPNSVoipChannelResponse
    , uavcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApnsVoipChannel' smart constructor.
data UpdateApnsVoipChannel = UpdateApnsVoipChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , aPNSVoipChannelRequest :: Types.APNSVoipChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApnsVoipChannel' value with any optional fields omitted.
mkUpdateApnsVoipChannel
    :: Core.Text -- ^ 'applicationId'
    -> Types.APNSVoipChannelRequest -- ^ 'aPNSVoipChannelRequest'
    -> UpdateApnsVoipChannel
mkUpdateApnsVoipChannel applicationId aPNSVoipChannelRequest
  = UpdateApnsVoipChannel'{applicationId, aPNSVoipChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavcApplicationId :: Lens.Lens' UpdateApnsVoipChannel Core.Text
uavcApplicationId = Lens.field @"applicationId"
{-# INLINEABLE uavcApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSVoipChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavcAPNSVoipChannelRequest :: Lens.Lens' UpdateApnsVoipChannel Types.APNSVoipChannelRequest
uavcAPNSVoipChannelRequest = Lens.field @"aPNSVoipChannelRequest"
{-# INLINEABLE uavcAPNSVoipChannelRequest #-}
{-# DEPRECATED aPNSVoipChannelRequest "Use generic-lens or generic-optics with 'aPNSVoipChannelRequest' instead"  #-}

instance Core.ToQuery UpdateApnsVoipChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApnsVoipChannel where
        toHeaders UpdateApnsVoipChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApnsVoipChannel where
        toJSON UpdateApnsVoipChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("APNSVoipChannelRequest" Core..= aPNSVoipChannelRequest)])

instance Core.AWSRequest UpdateApnsVoipChannel where
        type Rs UpdateApnsVoipChannel = UpdateApnsVoipChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns_voip",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateApnsVoipChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateApnsVoipChannelResponse' smart constructor.
data UpdateApnsVoipChannelResponse = UpdateApnsVoipChannelResponse'
  { aPNSVoipChannelResponse :: Types.APNSVoipChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApnsVoipChannelResponse' value with any optional fields omitted.
mkUpdateApnsVoipChannelResponse
    :: Types.APNSVoipChannelResponse -- ^ 'aPNSVoipChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateApnsVoipChannelResponse
mkUpdateApnsVoipChannelResponse aPNSVoipChannelResponse
  responseStatus
  = UpdateApnsVoipChannelResponse'{aPNSVoipChannelResponse,
                                   responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSVoipChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavcrrsAPNSVoipChannelResponse :: Lens.Lens' UpdateApnsVoipChannelResponse Types.APNSVoipChannelResponse
uavcrrsAPNSVoipChannelResponse = Lens.field @"aPNSVoipChannelResponse"
{-# INLINEABLE uavcrrsAPNSVoipChannelResponse #-}
{-# DEPRECATED aPNSVoipChannelResponse "Use generic-lens or generic-optics with 'aPNSVoipChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavcrrsResponseStatus :: Lens.Lens' UpdateApnsVoipChannelResponse Core.Int
uavcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uavcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
