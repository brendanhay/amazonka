{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteEmailChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the email channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteEmailChannel
    (
    -- * Creating a request
      DeleteEmailChannel (..)
    , mkDeleteEmailChannel
    -- ** Request lenses
    , decApplicationId

    -- * Destructuring the response
    , DeleteEmailChannelResponse (..)
    , mkDeleteEmailChannelResponse
    -- ** Response lenses
    , decrrsEmailChannelResponse
    , decrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEmailChannel' smart constructor.
newtype DeleteEmailChannel = DeleteEmailChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEmailChannel' value with any optional fields omitted.
mkDeleteEmailChannel
    :: Core.Text -- ^ 'applicationId'
    -> DeleteEmailChannel
mkDeleteEmailChannel applicationId
  = DeleteEmailChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decApplicationId :: Lens.Lens' DeleteEmailChannel Core.Text
decApplicationId = Lens.field @"applicationId"
{-# INLINEABLE decApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DeleteEmailChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteEmailChannel where
        toHeaders DeleteEmailChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteEmailChannel where
        type Rs DeleteEmailChannel = DeleteEmailChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/email",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteEmailChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteEmailChannelResponse' smart constructor.
data DeleteEmailChannelResponse = DeleteEmailChannelResponse'
  { emailChannelResponse :: Types.EmailChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEmailChannelResponse' value with any optional fields omitted.
mkDeleteEmailChannelResponse
    :: Types.EmailChannelResponse -- ^ 'emailChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteEmailChannelResponse
mkDeleteEmailChannelResponse emailChannelResponse responseStatus
  = DeleteEmailChannelResponse'{emailChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEmailChannelResponse :: Lens.Lens' DeleteEmailChannelResponse Types.EmailChannelResponse
decrrsEmailChannelResponse = Lens.field @"emailChannelResponse"
{-# INLINEABLE decrrsEmailChannelResponse #-}
{-# DEPRECATED emailChannelResponse "Use generic-lens or generic-optics with 'emailChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsResponseStatus :: Lens.Lens' DeleteEmailChannelResponse Core.Int
decrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE decrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
