{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SendUsersMessages (..),
    mkSendUsersMessages,

    -- ** Request lenses
    sumApplicationId,
    sumSendUsersMessageRequest,

    -- * Destructuring the response
    SendUsersMessagesResponse (..),
    mkSendUsersMessagesResponse,

    -- ** Response lenses
    sumrrsSendUsersMessageResponse,
    sumrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSendUsersMessages' smart constructor.
data SendUsersMessages = SendUsersMessages'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    sendUsersMessageRequest :: Types.SendUsersMessageRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendUsersMessages' value with any optional fields omitted.
mkSendUsersMessages ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'sendUsersMessageRequest'
  Types.SendUsersMessageRequest ->
  SendUsersMessages
mkSendUsersMessages applicationId sendUsersMessageRequest =
  SendUsersMessages' {applicationId, sendUsersMessageRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumApplicationId :: Lens.Lens' SendUsersMessages Core.Text
sumApplicationId = Lens.field @"applicationId"
{-# DEPRECATED sumApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sendUsersMessageRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumSendUsersMessageRequest :: Lens.Lens' SendUsersMessages Types.SendUsersMessageRequest
sumSendUsersMessageRequest = Lens.field @"sendUsersMessageRequest"
{-# DEPRECATED sumSendUsersMessageRequest "Use generic-lens or generic-optics with 'sendUsersMessageRequest' instead." #-}

instance Core.FromJSON SendUsersMessages where
  toJSON SendUsersMessages {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("SendUsersMessageRequest" Core..= sendUsersMessageRequest)
          ]
      )

instance Core.AWSRequest SendUsersMessages where
  type Rs SendUsersMessages = SendUsersMessagesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/users-messages")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SendUsersMessagesResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSendUsersMessagesResponse' smart constructor.
data SendUsersMessagesResponse = SendUsersMessagesResponse'
  { sendUsersMessageResponse :: Types.SendUsersMessageResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendUsersMessagesResponse' value with any optional fields omitted.
mkSendUsersMessagesResponse ::
  -- | 'sendUsersMessageResponse'
  Types.SendUsersMessageResponse ->
  -- | 'responseStatus'
  Core.Int ->
  SendUsersMessagesResponse
mkSendUsersMessagesResponse sendUsersMessageResponse responseStatus =
  SendUsersMessagesResponse'
    { sendUsersMessageResponse,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'sendUsersMessageResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrrsSendUsersMessageResponse :: Lens.Lens' SendUsersMessagesResponse Types.SendUsersMessageResponse
sumrrsSendUsersMessageResponse = Lens.field @"sendUsersMessageResponse"
{-# DEPRECATED sumrrsSendUsersMessageResponse "Use generic-lens or generic-optics with 'sendUsersMessageResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrrsResponseStatus :: Lens.Lens' SendUsersMessagesResponse Core.Int
sumrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sumrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
