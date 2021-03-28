{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteApnsSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs sandbox channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteApnsSandboxChannel
    (
    -- * Creating a request
      DeleteApnsSandboxChannel (..)
    , mkDeleteApnsSandboxChannel
    -- ** Request lenses
    , dascApplicationId

    -- * Destructuring the response
    , DeleteApnsSandboxChannelResponse (..)
    , mkDeleteApnsSandboxChannelResponse
    -- ** Response lenses
    , dascrrsAPNSSandboxChannelResponse
    , dascrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApnsSandboxChannel' smart constructor.
newtype DeleteApnsSandboxChannel = DeleteApnsSandboxChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApnsSandboxChannel' value with any optional fields omitted.
mkDeleteApnsSandboxChannel
    :: Core.Text -- ^ 'applicationId'
    -> DeleteApnsSandboxChannel
mkDeleteApnsSandboxChannel applicationId
  = DeleteApnsSandboxChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dascApplicationId :: Lens.Lens' DeleteApnsSandboxChannel Core.Text
dascApplicationId = Lens.field @"applicationId"
{-# INLINEABLE dascApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DeleteApnsSandboxChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApnsSandboxChannel where
        toHeaders DeleteApnsSandboxChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteApnsSandboxChannel where
        type Rs DeleteApnsSandboxChannel = DeleteApnsSandboxChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns_sandbox",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteApnsSandboxChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApnsSandboxChannelResponse' smart constructor.
data DeleteApnsSandboxChannelResponse = DeleteApnsSandboxChannelResponse'
  { aPNSSandboxChannelResponse :: Types.APNSSandboxChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApnsSandboxChannelResponse' value with any optional fields omitted.
mkDeleteApnsSandboxChannelResponse
    :: Types.APNSSandboxChannelResponse -- ^ 'aPNSSandboxChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteApnsSandboxChannelResponse
mkDeleteApnsSandboxChannelResponse aPNSSandboxChannelResponse
  responseStatus
  = DeleteApnsSandboxChannelResponse'{aPNSSandboxChannelResponse,
                                      responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dascrrsAPNSSandboxChannelResponse :: Lens.Lens' DeleteApnsSandboxChannelResponse Types.APNSSandboxChannelResponse
dascrrsAPNSSandboxChannelResponse = Lens.field @"aPNSSandboxChannelResponse"
{-# INLINEABLE dascrrsAPNSSandboxChannelResponse #-}
{-# DEPRECATED aPNSSandboxChannelResponse "Use generic-lens or generic-optics with 'aPNSSandboxChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dascrrsResponseStatus :: Lens.Lens' DeleteApnsSandboxChannelResponse Core.Int
dascrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dascrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
