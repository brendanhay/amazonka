{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateApnsChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs channel for an application or updates the status and settings of the APNs channel for an application.
module Network.AWS.Pinpoint.UpdateApnsChannel
    (
    -- * Creating a request
      UpdateApnsChannel (..)
    , mkUpdateApnsChannel
    -- ** Request lenses
    , uApplicationId
    , uAPNSChannelRequest

    -- * Destructuring the response
    , UpdateApnsChannelResponse (..)
    , mkUpdateApnsChannelResponse
    -- ** Response lenses
    , ursAPNSChannelResponse
    , ursResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApnsChannel' smart constructor.
data UpdateApnsChannel = UpdateApnsChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , aPNSChannelRequest :: Types.APNSChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApnsChannel' value with any optional fields omitted.
mkUpdateApnsChannel
    :: Core.Text -- ^ 'applicationId'
    -> Types.APNSChannelRequest -- ^ 'aPNSChannelRequest'
    -> UpdateApnsChannel
mkUpdateApnsChannel applicationId aPNSChannelRequest
  = UpdateApnsChannel'{applicationId, aPNSChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uApplicationId :: Lens.Lens' UpdateApnsChannel Core.Text
uApplicationId = Lens.field @"applicationId"
{-# INLINEABLE uApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAPNSChannelRequest :: Lens.Lens' UpdateApnsChannel Types.APNSChannelRequest
uAPNSChannelRequest = Lens.field @"aPNSChannelRequest"
{-# INLINEABLE uAPNSChannelRequest #-}
{-# DEPRECATED aPNSChannelRequest "Use generic-lens or generic-optics with 'aPNSChannelRequest' instead"  #-}

instance Core.ToQuery UpdateApnsChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApnsChannel where
        toHeaders UpdateApnsChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApnsChannel where
        toJSON UpdateApnsChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("APNSChannelRequest" Core..= aPNSChannelRequest)])

instance Core.AWSRequest UpdateApnsChannel where
        type Rs UpdateApnsChannel = UpdateApnsChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateApnsChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateApnsChannelResponse' smart constructor.
data UpdateApnsChannelResponse = UpdateApnsChannelResponse'
  { aPNSChannelResponse :: Types.APNSChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApnsChannelResponse' value with any optional fields omitted.
mkUpdateApnsChannelResponse
    :: Types.APNSChannelResponse -- ^ 'aPNSChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateApnsChannelResponse
mkUpdateApnsChannelResponse aPNSChannelResponse responseStatus
  = UpdateApnsChannelResponse'{aPNSChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursAPNSChannelResponse :: Lens.Lens' UpdateApnsChannelResponse Types.APNSChannelResponse
ursAPNSChannelResponse = Lens.field @"aPNSChannelResponse"
{-# INLINEABLE ursAPNSChannelResponse #-}
{-# DEPRECATED aPNSChannelResponse "Use generic-lens or generic-optics with 'aPNSChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateApnsChannelResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ursResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
