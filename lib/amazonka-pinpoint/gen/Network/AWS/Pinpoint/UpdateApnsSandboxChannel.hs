{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateApnsSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs sandbox channel for an application or updates the status and settings of the APNs sandbox channel for an application.
module Network.AWS.Pinpoint.UpdateApnsSandboxChannel
    (
    -- * Creating a request
      UpdateApnsSandboxChannel (..)
    , mkUpdateApnsSandboxChannel
    -- ** Request lenses
    , uascApplicationId
    , uascAPNSSandboxChannelRequest

    -- * Destructuring the response
    , UpdateApnsSandboxChannelResponse (..)
    , mkUpdateApnsSandboxChannelResponse
    -- ** Response lenses
    , uascrrsAPNSSandboxChannelResponse
    , uascrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApnsSandboxChannel' smart constructor.
data UpdateApnsSandboxChannel = UpdateApnsSandboxChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , aPNSSandboxChannelRequest :: Types.APNSSandboxChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApnsSandboxChannel' value with any optional fields omitted.
mkUpdateApnsSandboxChannel
    :: Core.Text -- ^ 'applicationId'
    -> Types.APNSSandboxChannelRequest -- ^ 'aPNSSandboxChannelRequest'
    -> UpdateApnsSandboxChannel
mkUpdateApnsSandboxChannel applicationId aPNSSandboxChannelRequest
  = UpdateApnsSandboxChannel'{applicationId,
                              aPNSSandboxChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uascApplicationId :: Lens.Lens' UpdateApnsSandboxChannel Core.Text
uascApplicationId = Lens.field @"applicationId"
{-# INLINEABLE uascApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSSandboxChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uascAPNSSandboxChannelRequest :: Lens.Lens' UpdateApnsSandboxChannel Types.APNSSandboxChannelRequest
uascAPNSSandboxChannelRequest = Lens.field @"aPNSSandboxChannelRequest"
{-# INLINEABLE uascAPNSSandboxChannelRequest #-}
{-# DEPRECATED aPNSSandboxChannelRequest "Use generic-lens or generic-optics with 'aPNSSandboxChannelRequest' instead"  #-}

instance Core.ToQuery UpdateApnsSandboxChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApnsSandboxChannel where
        toHeaders UpdateApnsSandboxChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApnsSandboxChannel where
        toJSON UpdateApnsSandboxChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("APNSSandboxChannelRequest" Core..= aPNSSandboxChannelRequest)])

instance Core.AWSRequest UpdateApnsSandboxChannel where
        type Rs UpdateApnsSandboxChannel = UpdateApnsSandboxChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns_sandbox",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateApnsSandboxChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateApnsSandboxChannelResponse' smart constructor.
data UpdateApnsSandboxChannelResponse = UpdateApnsSandboxChannelResponse'
  { aPNSSandboxChannelResponse :: Types.APNSSandboxChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApnsSandboxChannelResponse' value with any optional fields omitted.
mkUpdateApnsSandboxChannelResponse
    :: Types.APNSSandboxChannelResponse -- ^ 'aPNSSandboxChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateApnsSandboxChannelResponse
mkUpdateApnsSandboxChannelResponse aPNSSandboxChannelResponse
  responseStatus
  = UpdateApnsSandboxChannelResponse'{aPNSSandboxChannelResponse,
                                      responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uascrrsAPNSSandboxChannelResponse :: Lens.Lens' UpdateApnsSandboxChannelResponse Types.APNSSandboxChannelResponse
uascrrsAPNSSandboxChannelResponse = Lens.field @"aPNSSandboxChannelResponse"
{-# INLINEABLE uascrrsAPNSSandboxChannelResponse #-}
{-# DEPRECATED aPNSSandboxChannelResponse "Use generic-lens or generic-optics with 'aPNSSandboxChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uascrrsResponseStatus :: Lens.Lens' UpdateApnsSandboxChannelResponse Core.Int
uascrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uascrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
