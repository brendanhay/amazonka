{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    smrsResponseStatus,
    smrsMessageResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSendMessages' smart constructor.
data SendMessages = SendMessages'
  { applicationId :: Lude.Text,
    messageRequest :: MessageRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendMessages' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'messageRequest' - Undocumented field.
mkSendMessages ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'messageRequest'
  MessageRequest ->
  SendMessages
mkSendMessages pApplicationId_ pMessageRequest_ =
  SendMessages'
    { applicationId = pApplicationId_,
      messageRequest = pMessageRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smApplicationId :: Lens.Lens' SendMessages Lude.Text
smApplicationId = Lens.lens (applicationId :: SendMessages -> Lude.Text) (\s a -> s {applicationId = a} :: SendMessages)
{-# DEPRECATED smApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smMessageRequest :: Lens.Lens' SendMessages MessageRequest
smMessageRequest = Lens.lens (messageRequest :: SendMessages -> MessageRequest) (\s a -> s {messageRequest = a} :: SendMessages)
{-# DEPRECATED smMessageRequest "Use generic-lens or generic-optics with 'messageRequest' instead." #-}

instance Lude.AWSRequest SendMessages where
  type Rs SendMessages = SendMessagesResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          SendMessagesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders SendMessages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SendMessages where
  toJSON SendMessages' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("MessageRequest" Lude..= messageRequest)]
      )

instance Lude.ToPath SendMessages where
  toPath SendMessages' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/messages"]

instance Lude.ToQuery SendMessages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSendMessagesResponse' smart constructor.
data SendMessagesResponse = SendMessagesResponse'
  { responseStatus ::
      Lude.Int,
    messageResponse :: MessageResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendMessagesResponse' with the minimum fields required to make a request.
--
-- * 'messageResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkSendMessagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'messageResponse'
  MessageResponse ->
  SendMessagesResponse
mkSendMessagesResponse pResponseStatus_ pMessageResponse_ =
  SendMessagesResponse'
    { responseStatus = pResponseStatus_,
      messageResponse = pMessageResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsResponseStatus :: Lens.Lens' SendMessagesResponse Lude.Int
smrsResponseStatus = Lens.lens (responseStatus :: SendMessagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendMessagesResponse)
{-# DEPRECATED smrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsMessageResponse :: Lens.Lens' SendMessagesResponse MessageResponse
smrsMessageResponse = Lens.lens (messageResponse :: SendMessagesResponse -> MessageResponse) (\s a -> s {messageResponse = a} :: SendMessagesResponse)
{-# DEPRECATED smrsMessageResponse "Use generic-lens or generic-optics with 'messageResponse' instead." #-}
