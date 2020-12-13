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
    sumsSendUsersMessageRequest,
    sumsApplicationId,

    -- * Destructuring the response
    SendUsersMessagesResponse (..),
    mkSendUsersMessagesResponse,

    -- ** Response lenses
    sumrsSendUsersMessageResponse,
    sumrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSendUsersMessages' smart constructor.
data SendUsersMessages = SendUsersMessages'
  { sendUsersMessageRequest :: SendUsersMessageRequest,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendUsersMessages' with the minimum fields required to make a request.
--
-- * 'sendUsersMessageRequest' -
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkSendUsersMessages ::
  -- | 'sendUsersMessageRequest'
  SendUsersMessageRequest ->
  -- | 'applicationId'
  Lude.Text ->
  SendUsersMessages
mkSendUsersMessages pSendUsersMessageRequest_ pApplicationId_ =
  SendUsersMessages'
    { sendUsersMessageRequest =
        pSendUsersMessageRequest_,
      applicationId = pApplicationId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'sendUsersMessageRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumsSendUsersMessageRequest :: Lens.Lens' SendUsersMessages SendUsersMessageRequest
sumsSendUsersMessageRequest = Lens.lens (sendUsersMessageRequest :: SendUsersMessages -> SendUsersMessageRequest) (\s a -> s {sendUsersMessageRequest = a} :: SendUsersMessages)
{-# DEPRECATED sumsSendUsersMessageRequest "Use generic-lens or generic-optics with 'sendUsersMessageRequest' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumsApplicationId :: Lens.Lens' SendUsersMessages Lude.Text
sumsApplicationId = Lens.lens (applicationId :: SendUsersMessages -> Lude.Text) (\s a -> s {applicationId = a} :: SendUsersMessages)
{-# DEPRECATED sumsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest SendUsersMessages where
  type Rs SendUsersMessages = SendUsersMessagesResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          SendUsersMessagesResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendUsersMessages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SendUsersMessages where
  toJSON SendUsersMessages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("SendUsersMessageRequest" Lude..= sendUsersMessageRequest)
          ]
      )

instance Lude.ToPath SendUsersMessages where
  toPath SendUsersMessages' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/users-messages"]

instance Lude.ToQuery SendUsersMessages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSendUsersMessagesResponse' smart constructor.
data SendUsersMessagesResponse = SendUsersMessagesResponse'
  { sendUsersMessageResponse :: SendUsersMessageResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendUsersMessagesResponse' with the minimum fields required to make a request.
--
-- * 'sendUsersMessageResponse' -
-- * 'responseStatus' - The response status code.
mkSendUsersMessagesResponse ::
  -- | 'sendUsersMessageResponse'
  SendUsersMessageResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  SendUsersMessagesResponse
mkSendUsersMessagesResponse
  pSendUsersMessageResponse_
  pResponseStatus_ =
    SendUsersMessagesResponse'
      { sendUsersMessageResponse =
          pSendUsersMessageResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'sendUsersMessageResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrsSendUsersMessageResponse :: Lens.Lens' SendUsersMessagesResponse SendUsersMessageResponse
sumrsSendUsersMessageResponse = Lens.lens (sendUsersMessageResponse :: SendUsersMessagesResponse -> SendUsersMessageResponse) (\s a -> s {sendUsersMessageResponse = a} :: SendUsersMessagesResponse)
{-# DEPRECATED sumrsSendUsersMessageResponse "Use generic-lens or generic-optics with 'sendUsersMessageResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrsResponseStatus :: Lens.Lens' SendUsersMessagesResponse Lude.Int
sumrsResponseStatus = Lens.lens (responseStatus :: SendUsersMessagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendUsersMessagesResponse)
{-# DEPRECATED sumrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
