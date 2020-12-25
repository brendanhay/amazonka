{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.SendMessages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and sends a direct message.
module Network.AWS.Pinpoint.SendMessages
  ( -- * Creating a request
    SendMessages (..),
    mkSendMessages,

    -- ** Request lenses
    smApplicationId,
    smMessageRequest,

    -- * Destructuring the response
    SendMessagesResponse (..),
    mkSendMessagesResponse,

    -- ** Response lenses
    smrrsMessageResponse,
    smrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSendMessages' smart constructor.
data SendMessages = SendMessages'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    messageRequest :: Types.MessageRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendMessages' value with any optional fields omitted.
mkSendMessages ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'messageRequest'
  Types.MessageRequest ->
  SendMessages
mkSendMessages applicationId messageRequest =
  SendMessages' {applicationId, messageRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smApplicationId :: Lens.Lens' SendMessages Core.Text
smApplicationId = Lens.field @"applicationId"
{-# DEPRECATED smApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smMessageRequest :: Lens.Lens' SendMessages Types.MessageRequest
smMessageRequest = Lens.field @"messageRequest"
{-# DEPRECATED smMessageRequest "Use generic-lens or generic-optics with 'messageRequest' instead." #-}

instance Core.FromJSON SendMessages where
  toJSON SendMessages {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("MessageRequest" Core..= messageRequest)]
      )

instance Core.AWSRequest SendMessages where
  type Rs SendMessages = SendMessagesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/messages")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SendMessagesResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSendMessagesResponse' smart constructor.
data SendMessagesResponse = SendMessagesResponse'
  { messageResponse :: Types.MessageResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendMessagesResponse' value with any optional fields omitted.
mkSendMessagesResponse ::
  -- | 'messageResponse'
  Types.MessageResponse ->
  -- | 'responseStatus'
  Core.Int ->
  SendMessagesResponse
mkSendMessagesResponse messageResponse responseStatus =
  SendMessagesResponse' {messageResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsMessageResponse :: Lens.Lens' SendMessagesResponse Types.MessageResponse
smrrsMessageResponse = Lens.field @"messageResponse"
{-# DEPRECATED smrrsMessageResponse "Use generic-lens or generic-optics with 'messageResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsResponseStatus :: Lens.Lens' SendMessagesResponse Core.Int
smrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED smrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
