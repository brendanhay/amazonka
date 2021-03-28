{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.SendUsersMessages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and sends a message to a list of users.
module Network.AWS.Pinpoint.SendUsersMessages
    (
    -- * Creating a request
      SendUsersMessages (..)
    , mkSendUsersMessages
    -- ** Request lenses
    , sumApplicationId
    , sumSendUsersMessageRequest

    -- * Destructuring the response
    , SendUsersMessagesResponse (..)
    , mkSendUsersMessagesResponse
    -- ** Response lenses
    , sumrrsSendUsersMessageResponse
    , sumrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSendUsersMessages' smart constructor.
data SendUsersMessages = SendUsersMessages'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , sendUsersMessageRequest :: Types.SendUsersMessageRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendUsersMessages' value with any optional fields omitted.
mkSendUsersMessages
    :: Core.Text -- ^ 'applicationId'
    -> Types.SendUsersMessageRequest -- ^ 'sendUsersMessageRequest'
    -> SendUsersMessages
mkSendUsersMessages applicationId sendUsersMessageRequest
  = SendUsersMessages'{applicationId, sendUsersMessageRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumApplicationId :: Lens.Lens' SendUsersMessages Core.Text
sumApplicationId = Lens.field @"applicationId"
{-# INLINEABLE sumApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sendUsersMessageRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumSendUsersMessageRequest :: Lens.Lens' SendUsersMessages Types.SendUsersMessageRequest
sumSendUsersMessageRequest = Lens.field @"sendUsersMessageRequest"
{-# INLINEABLE sumSendUsersMessageRequest #-}
{-# DEPRECATED sendUsersMessageRequest "Use generic-lens or generic-optics with 'sendUsersMessageRequest' instead"  #-}

instance Core.ToQuery SendUsersMessages where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SendUsersMessages where
        toHeaders SendUsersMessages{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SendUsersMessages where
        toJSON SendUsersMessages{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("SendUsersMessageRequest" Core..= sendUsersMessageRequest)])

instance Core.AWSRequest SendUsersMessages where
        type Rs SendUsersMessages = SendUsersMessagesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/users-messages",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SendUsersMessagesResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSendUsersMessagesResponse' smart constructor.
data SendUsersMessagesResponse = SendUsersMessagesResponse'
  { sendUsersMessageResponse :: Types.SendUsersMessageResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendUsersMessagesResponse' value with any optional fields omitted.
mkSendUsersMessagesResponse
    :: Types.SendUsersMessageResponse -- ^ 'sendUsersMessageResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> SendUsersMessagesResponse
mkSendUsersMessagesResponse sendUsersMessageResponse responseStatus
  = SendUsersMessagesResponse'{sendUsersMessageResponse,
                               responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sendUsersMessageResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrrsSendUsersMessageResponse :: Lens.Lens' SendUsersMessagesResponse Types.SendUsersMessageResponse
sumrrsSendUsersMessageResponse = Lens.field @"sendUsersMessageResponse"
{-# INLINEABLE sumrrsSendUsersMessageResponse #-}
{-# DEPRECATED sendUsersMessageResponse "Use generic-lens or generic-optics with 'sendUsersMessageResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrrsResponseStatus :: Lens.Lens' SendUsersMessagesResponse Core.Int
sumrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sumrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
