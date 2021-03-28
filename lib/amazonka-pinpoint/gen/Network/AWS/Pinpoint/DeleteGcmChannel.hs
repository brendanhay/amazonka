{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteGcmChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the GCM channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteGcmChannel
    (
    -- * Creating a request
      DeleteGcmChannel (..)
    , mkDeleteGcmChannel
    -- ** Request lenses
    , dgcApplicationId

    -- * Destructuring the response
    , DeleteGcmChannelResponse (..)
    , mkDeleteGcmChannelResponse
    -- ** Response lenses
    , dgcrrsGCMChannelResponse
    , dgcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGcmChannel' smart constructor.
newtype DeleteGcmChannel = DeleteGcmChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGcmChannel' value with any optional fields omitted.
mkDeleteGcmChannel
    :: Core.Text -- ^ 'applicationId'
    -> DeleteGcmChannel
mkDeleteGcmChannel applicationId = DeleteGcmChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcApplicationId :: Lens.Lens' DeleteGcmChannel Core.Text
dgcApplicationId = Lens.field @"applicationId"
{-# INLINEABLE dgcApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DeleteGcmChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteGcmChannel where
        toHeaders DeleteGcmChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteGcmChannel where
        type Rs DeleteGcmChannel = DeleteGcmChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/gcm",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteGcmChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteGcmChannelResponse' smart constructor.
data DeleteGcmChannelResponse = DeleteGcmChannelResponse'
  { gCMChannelResponse :: Types.GCMChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGcmChannelResponse' value with any optional fields omitted.
mkDeleteGcmChannelResponse
    :: Types.GCMChannelResponse -- ^ 'gCMChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteGcmChannelResponse
mkDeleteGcmChannelResponse gCMChannelResponse responseStatus
  = DeleteGcmChannelResponse'{gCMChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gCMChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrrsGCMChannelResponse :: Lens.Lens' DeleteGcmChannelResponse Types.GCMChannelResponse
dgcrrsGCMChannelResponse = Lens.field @"gCMChannelResponse"
{-# INLINEABLE dgcrrsGCMChannelResponse #-}
{-# DEPRECATED gCMChannelResponse "Use generic-lens or generic-optics with 'gCMChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrrsResponseStatus :: Lens.Lens' DeleteGcmChannelResponse Core.Int
dgcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
