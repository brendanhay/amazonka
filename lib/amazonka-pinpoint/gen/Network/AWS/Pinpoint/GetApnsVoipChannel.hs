{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApnsVoipChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs VoIP channel for an application.
module Network.AWS.Pinpoint.GetApnsVoipChannel
    (
    -- * Creating a request
      GetApnsVoipChannel (..)
    , mkGetApnsVoipChannel
    -- ** Request lenses
    , gavcApplicationId

    -- * Destructuring the response
    , GetApnsVoipChannelResponse (..)
    , mkGetApnsVoipChannelResponse
    -- ** Response lenses
    , gavcrrsAPNSVoipChannelResponse
    , gavcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetApnsVoipChannel' smart constructor.
newtype GetApnsVoipChannel = GetApnsVoipChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApnsVoipChannel' value with any optional fields omitted.
mkGetApnsVoipChannel
    :: Core.Text -- ^ 'applicationId'
    -> GetApnsVoipChannel
mkGetApnsVoipChannel applicationId
  = GetApnsVoipChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcApplicationId :: Lens.Lens' GetApnsVoipChannel Core.Text
gavcApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gavcApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetApnsVoipChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApnsVoipChannel where
        toHeaders GetApnsVoipChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetApnsVoipChannel where
        type Rs GetApnsVoipChannel = GetApnsVoipChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns_voip",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetApnsVoipChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetApnsVoipChannelResponse' smart constructor.
data GetApnsVoipChannelResponse = GetApnsVoipChannelResponse'
  { aPNSVoipChannelResponse :: Types.APNSVoipChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApnsVoipChannelResponse' value with any optional fields omitted.
mkGetApnsVoipChannelResponse
    :: Types.APNSVoipChannelResponse -- ^ 'aPNSVoipChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetApnsVoipChannelResponse
mkGetApnsVoipChannelResponse aPNSVoipChannelResponse responseStatus
  = GetApnsVoipChannelResponse'{aPNSVoipChannelResponse,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSVoipChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrrsAPNSVoipChannelResponse :: Lens.Lens' GetApnsVoipChannelResponse Types.APNSVoipChannelResponse
gavcrrsAPNSVoipChannelResponse = Lens.field @"aPNSVoipChannelResponse"
{-# INLINEABLE gavcrrsAPNSVoipChannelResponse #-}
{-# DEPRECATED aPNSVoipChannelResponse "Use generic-lens or generic-optics with 'aPNSVoipChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrrsResponseStatus :: Lens.Lens' GetApnsVoipChannelResponse Core.Int
gavcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gavcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
