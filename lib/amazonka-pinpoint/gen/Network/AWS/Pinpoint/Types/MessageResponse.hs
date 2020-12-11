-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageResponse
  ( MessageResponse (..),

    -- * Smart constructor
    mkMessageResponse,

    -- * Lenses
    mRequestId,
    mResult,
    mEndpointResult,
    mApplicationId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointMessageResult
import Network.AWS.Pinpoint.Types.MessageResult
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the results of a request to send a message to an endpoint address.
--
-- /See:/ 'mkMessageResponse' smart constructor.
data MessageResponse = MessageResponse'
  { requestId ::
      Lude.Maybe Lude.Text,
    result ::
      Lude.Maybe (Lude.HashMap Lude.Text (MessageResult)),
    endpointResult ::
      Lude.Maybe (Lude.HashMap Lude.Text (EndpointMessageResult)),
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that was used to send the message.
-- * 'endpointResult' - A map that contains a multipart response for each address that the message was sent to. In the map, the endpoint ID is the key and the result is the value.
-- * 'requestId' - The identifier for the original request that the message was delivered for.
-- * 'result' - A map that contains a multipart response for each address (email address, phone number, or push notification token) that the message was sent to. In the map, the address is the key and the result is the value.
mkMessageResponse ::
  -- | 'applicationId'
  Lude.Text ->
  MessageResponse
mkMessageResponse pApplicationId_ =
  MessageResponse'
    { requestId = Lude.Nothing,
      result = Lude.Nothing,
      endpointResult = Lude.Nothing,
      applicationId = pApplicationId_
    }

-- | The identifier for the original request that the message was delivered for.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRequestId :: Lens.Lens' MessageResponse (Lude.Maybe Lude.Text)
mRequestId = Lens.lens (requestId :: MessageResponse -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: MessageResponse)
{-# DEPRECATED mRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | A map that contains a multipart response for each address (email address, phone number, or push notification token) that the message was sent to. In the map, the address is the key and the result is the value.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mResult :: Lens.Lens' MessageResponse (Lude.Maybe (Lude.HashMap Lude.Text (MessageResult)))
mResult = Lens.lens (result :: MessageResponse -> Lude.Maybe (Lude.HashMap Lude.Text (MessageResult))) (\s a -> s {result = a} :: MessageResponse)
{-# DEPRECATED mResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | A map that contains a multipart response for each address that the message was sent to. In the map, the endpoint ID is the key and the result is the value.
--
-- /Note:/ Consider using 'endpointResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEndpointResult :: Lens.Lens' MessageResponse (Lude.Maybe (Lude.HashMap Lude.Text (EndpointMessageResult)))
mEndpointResult = Lens.lens (endpointResult :: MessageResponse -> Lude.Maybe (Lude.HashMap Lude.Text (EndpointMessageResult))) (\s a -> s {endpointResult = a} :: MessageResponse)
{-# DEPRECATED mEndpointResult "Use generic-lens or generic-optics with 'endpointResult' instead." #-}

-- | The unique identifier for the application that was used to send the message.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mApplicationId :: Lens.Lens' MessageResponse Lude.Text
mApplicationId = Lens.lens (applicationId :: MessageResponse -> Lude.Text) (\s a -> s {applicationId = a} :: MessageResponse)
{-# DEPRECATED mApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.FromJSON MessageResponse where
  parseJSON =
    Lude.withObject
      "MessageResponse"
      ( \x ->
          MessageResponse'
            Lude.<$> (x Lude..:? "RequestId")
            Lude.<*> (x Lude..:? "Result" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EndpointResult" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "ApplicationId")
      )
