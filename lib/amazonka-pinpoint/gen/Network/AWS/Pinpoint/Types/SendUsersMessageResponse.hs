{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SendUsersMessageResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SendUsersMessageResponse
  ( SendUsersMessageResponse (..),

    -- * Smart constructor
    mkSendUsersMessageResponse,

    -- * Lenses
    sumRequestId,
    sumResult,
    sumApplicationId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointMessageResult
import qualified Network.AWS.Prelude as Lude

-- | Provides information about which users and endpoints a message was sent to.
--
-- /See:/ 'mkSendUsersMessageResponse' smart constructor.
data SendUsersMessageResponse = SendUsersMessageResponse'
  { -- | The unique identifier that was assigned to the message request.
    requestId :: Lude.Maybe Lude.Text,
    -- | An object that indicates which endpoints the message was sent to, for each user. The object lists user IDs and, for each user ID, provides the endpoint IDs that the message was sent to. For each endpoint ID, it provides an EndpointMessageResult object.
    result :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.HashMap Lude.Text (EndpointMessageResult))),
    -- | The unique identifier for the application that was used to send the message.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendUsersMessageResponse' with the minimum fields required to make a request.
--
-- * 'requestId' - The unique identifier that was assigned to the message request.
-- * 'result' - An object that indicates which endpoints the message was sent to, for each user. The object lists user IDs and, for each user ID, provides the endpoint IDs that the message was sent to. For each endpoint ID, it provides an EndpointMessageResult object.
-- * 'applicationId' - The unique identifier for the application that was used to send the message.
mkSendUsersMessageResponse ::
  -- | 'applicationId'
  Lude.Text ->
  SendUsersMessageResponse
mkSendUsersMessageResponse pApplicationId_ =
  SendUsersMessageResponse'
    { requestId = Lude.Nothing,
      result = Lude.Nothing,
      applicationId = pApplicationId_
    }

-- | The unique identifier that was assigned to the message request.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumRequestId :: Lens.Lens' SendUsersMessageResponse (Lude.Maybe Lude.Text)
sumRequestId = Lens.lens (requestId :: SendUsersMessageResponse -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: SendUsersMessageResponse)
{-# DEPRECATED sumRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | An object that indicates which endpoints the message was sent to, for each user. The object lists user IDs and, for each user ID, provides the endpoint IDs that the message was sent to. For each endpoint ID, it provides an EndpointMessageResult object.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumResult :: Lens.Lens' SendUsersMessageResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.HashMap Lude.Text (EndpointMessageResult))))
sumResult = Lens.lens (result :: SendUsersMessageResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.HashMap Lude.Text (EndpointMessageResult)))) (\s a -> s {result = a} :: SendUsersMessageResponse)
{-# DEPRECATED sumResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The unique identifier for the application that was used to send the message.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumApplicationId :: Lens.Lens' SendUsersMessageResponse Lude.Text
sumApplicationId = Lens.lens (applicationId :: SendUsersMessageResponse -> Lude.Text) (\s a -> s {applicationId = a} :: SendUsersMessageResponse)
{-# DEPRECATED sumApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.FromJSON SendUsersMessageResponse where
  parseJSON =
    Lude.withObject
      "SendUsersMessageResponse"
      ( \x ->
          SendUsersMessageResponse'
            Lude.<$> (x Lude..:? "RequestId")
            Lude.<*> (x Lude..:? "Result" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "ApplicationId")
      )
