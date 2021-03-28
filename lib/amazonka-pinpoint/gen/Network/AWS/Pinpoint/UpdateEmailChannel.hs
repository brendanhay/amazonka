{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateEmailChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the email channel for an application or updates the status and settings of the email channel for an application.
module Network.AWS.Pinpoint.UpdateEmailChannel
    (
    -- * Creating a request
      UpdateEmailChannel (..)
    , mkUpdateEmailChannel
    -- ** Request lenses
    , uecApplicationId
    , uecEmailChannelRequest

    -- * Destructuring the response
    , UpdateEmailChannelResponse (..)
    , mkUpdateEmailChannelResponse
    -- ** Response lenses
    , uecrrsEmailChannelResponse
    , uecrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEmailChannel' smart constructor.
data UpdateEmailChannel = UpdateEmailChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , emailChannelRequest :: Types.EmailChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEmailChannel' value with any optional fields omitted.
mkUpdateEmailChannel
    :: Core.Text -- ^ 'applicationId'
    -> Types.EmailChannelRequest -- ^ 'emailChannelRequest'
    -> UpdateEmailChannel
mkUpdateEmailChannel applicationId emailChannelRequest
  = UpdateEmailChannel'{applicationId, emailChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecApplicationId :: Lens.Lens' UpdateEmailChannel Core.Text
uecApplicationId = Lens.field @"applicationId"
{-# INLINEABLE uecApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecEmailChannelRequest :: Lens.Lens' UpdateEmailChannel Types.EmailChannelRequest
uecEmailChannelRequest = Lens.field @"emailChannelRequest"
{-# INLINEABLE uecEmailChannelRequest #-}
{-# DEPRECATED emailChannelRequest "Use generic-lens or generic-optics with 'emailChannelRequest' instead"  #-}

instance Core.ToQuery UpdateEmailChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateEmailChannel where
        toHeaders UpdateEmailChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateEmailChannel where
        toJSON UpdateEmailChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EmailChannelRequest" Core..= emailChannelRequest)])

instance Core.AWSRequest UpdateEmailChannel where
        type Rs UpdateEmailChannel = UpdateEmailChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/email",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateEmailChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateEmailChannelResponse' smart constructor.
data UpdateEmailChannelResponse = UpdateEmailChannelResponse'
  { emailChannelResponse :: Types.EmailChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEmailChannelResponse' value with any optional fields omitted.
mkUpdateEmailChannelResponse
    :: Types.EmailChannelResponse -- ^ 'emailChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateEmailChannelResponse
mkUpdateEmailChannelResponse emailChannelResponse responseStatus
  = UpdateEmailChannelResponse'{emailChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecrrsEmailChannelResponse :: Lens.Lens' UpdateEmailChannelResponse Types.EmailChannelResponse
uecrrsEmailChannelResponse = Lens.field @"emailChannelResponse"
{-# INLINEABLE uecrrsEmailChannelResponse #-}
{-# DEPRECATED emailChannelResponse "Use generic-lens or generic-optics with 'emailChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecrrsResponseStatus :: Lens.Lens' UpdateEmailChannelResponse Core.Int
uecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
