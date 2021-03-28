{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateGcmChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the GCM channel for an application or updates the status and settings of the GCM channel for an application.
module Network.AWS.Pinpoint.UpdateGcmChannel
    (
    -- * Creating a request
      UpdateGcmChannel (..)
    , mkUpdateGcmChannel
    -- ** Request lenses
    , ugcApplicationId
    , ugcGCMChannelRequest

    -- * Destructuring the response
    , UpdateGcmChannelResponse (..)
    , mkUpdateGcmChannelResponse
    -- ** Response lenses
    , ugcrrsGCMChannelResponse
    , ugcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGcmChannel' smart constructor.
data UpdateGcmChannel = UpdateGcmChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , gCMChannelRequest :: Types.GCMChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGcmChannel' value with any optional fields omitted.
mkUpdateGcmChannel
    :: Core.Text -- ^ 'applicationId'
    -> Types.GCMChannelRequest -- ^ 'gCMChannelRequest'
    -> UpdateGcmChannel
mkUpdateGcmChannel applicationId gCMChannelRequest
  = UpdateGcmChannel'{applicationId, gCMChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcApplicationId :: Lens.Lens' UpdateGcmChannel Core.Text
ugcApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ugcApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gCMChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcGCMChannelRequest :: Lens.Lens' UpdateGcmChannel Types.GCMChannelRequest
ugcGCMChannelRequest = Lens.field @"gCMChannelRequest"
{-# INLINEABLE ugcGCMChannelRequest #-}
{-# DEPRECATED gCMChannelRequest "Use generic-lens or generic-optics with 'gCMChannelRequest' instead"  #-}

instance Core.ToQuery UpdateGcmChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGcmChannel where
        toHeaders UpdateGcmChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateGcmChannel where
        toJSON UpdateGcmChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GCMChannelRequest" Core..= gCMChannelRequest)])

instance Core.AWSRequest UpdateGcmChannel where
        type Rs UpdateGcmChannel = UpdateGcmChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/gcm",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateGcmChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGcmChannelResponse' smart constructor.
data UpdateGcmChannelResponse = UpdateGcmChannelResponse'
  { gCMChannelResponse :: Types.GCMChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGcmChannelResponse' value with any optional fields omitted.
mkUpdateGcmChannelResponse
    :: Types.GCMChannelResponse -- ^ 'gCMChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateGcmChannelResponse
mkUpdateGcmChannelResponse gCMChannelResponse responseStatus
  = UpdateGcmChannelResponse'{gCMChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gCMChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrrsGCMChannelResponse :: Lens.Lens' UpdateGcmChannelResponse Types.GCMChannelResponse
ugcrrsGCMChannelResponse = Lens.field @"gCMChannelResponse"
{-# INLINEABLE ugcrrsGCMChannelResponse #-}
{-# DEPRECATED gCMChannelResponse "Use generic-lens or generic-optics with 'gCMChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrrsResponseStatus :: Lens.Lens' UpdateGcmChannelResponse Core.Int
ugcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ugcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
