{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteApnsChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteApnsChannel
    (
    -- * Creating a request
      DeleteApnsChannel (..)
    , mkDeleteApnsChannel
    -- ** Request lenses
    , dApplicationId

    -- * Destructuring the response
    , DeleteApnsChannelResponse (..)
    , mkDeleteApnsChannelResponse
    -- ** Response lenses
    , drsAPNSChannelResponse
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApnsChannel' smart constructor.
newtype DeleteApnsChannel = DeleteApnsChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApnsChannel' value with any optional fields omitted.
mkDeleteApnsChannel
    :: Core.Text -- ^ 'applicationId'
    -> DeleteApnsChannel
mkDeleteApnsChannel applicationId
  = DeleteApnsChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dApplicationId :: Lens.Lens' DeleteApnsChannel Core.Text
dApplicationId = Lens.field @"applicationId"
{-# INLINEABLE dApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DeleteApnsChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApnsChannel where
        toHeaders DeleteApnsChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteApnsChannel where
        type Rs DeleteApnsChannel = DeleteApnsChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteApnsChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApnsChannelResponse' smart constructor.
data DeleteApnsChannelResponse = DeleteApnsChannelResponse'
  { aPNSChannelResponse :: Types.APNSChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApnsChannelResponse' value with any optional fields omitted.
mkDeleteApnsChannelResponse
    :: Types.APNSChannelResponse -- ^ 'aPNSChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteApnsChannelResponse
mkDeleteApnsChannelResponse aPNSChannelResponse responseStatus
  = DeleteApnsChannelResponse'{aPNSChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAPNSChannelResponse :: Lens.Lens' DeleteApnsChannelResponse Types.APNSChannelResponse
drsAPNSChannelResponse = Lens.field @"aPNSChannelResponse"
{-# INLINEABLE drsAPNSChannelResponse #-}
{-# DEPRECATED aPNSChannelResponse "Use generic-lens or generic-optics with 'aPNSChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteApnsChannelResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
