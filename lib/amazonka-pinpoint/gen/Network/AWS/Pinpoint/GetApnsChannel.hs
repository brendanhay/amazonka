{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApnsChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs channel for an application.
module Network.AWS.Pinpoint.GetApnsChannel
    (
    -- * Creating a request
      GetApnsChannel (..)
    , mkGetApnsChannel
    -- ** Request lenses
    , gacfApplicationId

    -- * Destructuring the response
    , GetApnsChannelResponse (..)
    , mkGetApnsChannelResponse
    -- ** Response lenses
    , gacrrsAPNSChannelResponse
    , gacrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetApnsChannel' smart constructor.
newtype GetApnsChannel = GetApnsChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApnsChannel' value with any optional fields omitted.
mkGetApnsChannel
    :: Core.Text -- ^ 'applicationId'
    -> GetApnsChannel
mkGetApnsChannel applicationId = GetApnsChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacfApplicationId :: Lens.Lens' GetApnsChannel Core.Text
gacfApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gacfApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetApnsChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApnsChannel where
        toHeaders GetApnsChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetApnsChannel where
        type Rs GetApnsChannel = GetApnsChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetApnsChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetApnsChannelResponse' smart constructor.
data GetApnsChannelResponse = GetApnsChannelResponse'
  { aPNSChannelResponse :: Types.APNSChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApnsChannelResponse' value with any optional fields omitted.
mkGetApnsChannelResponse
    :: Types.APNSChannelResponse -- ^ 'aPNSChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetApnsChannelResponse
mkGetApnsChannelResponse aPNSChannelResponse responseStatus
  = GetApnsChannelResponse'{aPNSChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrrsAPNSChannelResponse :: Lens.Lens' GetApnsChannelResponse Types.APNSChannelResponse
gacrrsAPNSChannelResponse = Lens.field @"aPNSChannelResponse"
{-# INLINEABLE gacrrsAPNSChannelResponse #-}
{-# DEPRECATED aPNSChannelResponse "Use generic-lens or generic-optics with 'aPNSChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrrsResponseStatus :: Lens.Lens' GetApnsChannelResponse Core.Int
gacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
