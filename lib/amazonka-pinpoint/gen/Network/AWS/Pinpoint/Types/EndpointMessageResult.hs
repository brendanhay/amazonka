{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointMessageResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointMessageResult
  ( EndpointMessageResult (..),

    -- * Smart constructor
    mkEndpointMessageResult,

    -- * Lenses
    emrAddress,
    emrStatusMessage,
    emrUpdatedToken,
    emrMessageId,
    emrDeliveryStatus,
    emrStatusCode,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DeliveryStatus
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the delivery status and results of sending a message directly to an endpoint.
--
-- /See:/ 'mkEndpointMessageResult' smart constructor.
data EndpointMessageResult = EndpointMessageResult'
  { address ::
      Lude.Maybe Lude.Text,
    statusMessage :: Lude.Maybe Lude.Text,
    updatedToken :: Lude.Maybe Lude.Text,
    messageId :: Lude.Maybe Lude.Text,
    deliveryStatus :: DeliveryStatus,
    statusCode :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointMessageResult' with the minimum fields required to make a request.
--
-- * 'address' - The endpoint address that the message was delivered to.
-- * 'deliveryStatus' - The delivery status of the message. Possible values are:
--
--
--     * DUPLICATE - The endpoint address is a duplicate of another endpoint address. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * OPT_OUT - The user who's associated with the endpoint has opted out of receiving messages from you. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * PERMANENT_FAILURE - An error occurred when delivering the message to the endpoint. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * SUCCESSFUL - The message was successfully delivered to the endpoint.
--
--
--     * TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * THROTTLED - Amazon Pinpoint throttled the operation to send the message to the endpoint.
--
--
--     * TIMEOUT - The message couldn't be sent within the timeout period.
--
--
--     * UNKNOWN_FAILURE - An unknown error occurred.
--
--
-- * 'messageId' - The unique identifier for the message that was sent.
-- * 'statusCode' - The downstream service status code for delivering the message.
-- * 'statusMessage' - The status message for delivering the message.
-- * 'updatedToken' - For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
mkEndpointMessageResult ::
  -- | 'deliveryStatus'
  DeliveryStatus ->
  -- | 'statusCode'
  Lude.Int ->
  EndpointMessageResult
mkEndpointMessageResult pDeliveryStatus_ pStatusCode_ =
  EndpointMessageResult'
    { address = Lude.Nothing,
      statusMessage = Lude.Nothing,
      updatedToken = Lude.Nothing,
      messageId = Lude.Nothing,
      deliveryStatus = pDeliveryStatus_,
      statusCode = pStatusCode_
    }

-- | The endpoint address that the message was delivered to.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrAddress :: Lens.Lens' EndpointMessageResult (Lude.Maybe Lude.Text)
emrAddress = Lens.lens (address :: EndpointMessageResult -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: EndpointMessageResult)
{-# DEPRECATED emrAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The status message for delivering the message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrStatusMessage :: Lens.Lens' EndpointMessageResult (Lude.Maybe Lude.Text)
emrStatusMessage = Lens.lens (statusMessage :: EndpointMessageResult -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: EndpointMessageResult)
{-# DEPRECATED emrStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
--
-- /Note:/ Consider using 'updatedToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrUpdatedToken :: Lens.Lens' EndpointMessageResult (Lude.Maybe Lude.Text)
emrUpdatedToken = Lens.lens (updatedToken :: EndpointMessageResult -> Lude.Maybe Lude.Text) (\s a -> s {updatedToken = a} :: EndpointMessageResult)
{-# DEPRECATED emrUpdatedToken "Use generic-lens or generic-optics with 'updatedToken' instead." #-}

-- | The unique identifier for the message that was sent.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrMessageId :: Lens.Lens' EndpointMessageResult (Lude.Maybe Lude.Text)
emrMessageId = Lens.lens (messageId :: EndpointMessageResult -> Lude.Maybe Lude.Text) (\s a -> s {messageId = a} :: EndpointMessageResult)
{-# DEPRECATED emrMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The delivery status of the message. Possible values are:
--
--
--     * DUPLICATE - The endpoint address is a duplicate of another endpoint address. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * OPT_OUT - The user who's associated with the endpoint has opted out of receiving messages from you. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * PERMANENT_FAILURE - An error occurred when delivering the message to the endpoint. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * SUCCESSFUL - The message was successfully delivered to the endpoint.
--
--
--     * TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * THROTTLED - Amazon Pinpoint throttled the operation to send the message to the endpoint.
--
--
--     * TIMEOUT - The message couldn't be sent within the timeout period.
--
--
--     * UNKNOWN_FAILURE - An unknown error occurred.
--
--
--
-- /Note:/ Consider using 'deliveryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrDeliveryStatus :: Lens.Lens' EndpointMessageResult DeliveryStatus
emrDeliveryStatus = Lens.lens (deliveryStatus :: EndpointMessageResult -> DeliveryStatus) (\s a -> s {deliveryStatus = a} :: EndpointMessageResult)
{-# DEPRECATED emrDeliveryStatus "Use generic-lens or generic-optics with 'deliveryStatus' instead." #-}

-- | The downstream service status code for delivering the message.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrStatusCode :: Lens.Lens' EndpointMessageResult Lude.Int
emrStatusCode = Lens.lens (statusCode :: EndpointMessageResult -> Lude.Int) (\s a -> s {statusCode = a} :: EndpointMessageResult)
{-# DEPRECATED emrStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON EndpointMessageResult where
  parseJSON =
    Lude.withObject
      "EndpointMessageResult"
      ( \x ->
          EndpointMessageResult'
            Lude.<$> (x Lude..:? "Address")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "UpdatedToken")
            Lude.<*> (x Lude..:? "MessageId")
            Lude.<*> (x Lude..: "DeliveryStatus")
            Lude.<*> (x Lude..: "StatusCode")
      )
