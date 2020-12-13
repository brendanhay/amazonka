{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageResult
  ( MessageResult (..),

    -- * Smart constructor
    mkMessageResult,

    -- * Lenses
    mrDeliveryStatus,
    mrStatusMessage,
    mrUpdatedToken,
    mrMessageId,
    mrStatusCode,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DeliveryStatus
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the results of sending a message directly to an endpoint address.
--
-- /See:/ 'mkMessageResult' smart constructor.
data MessageResult = MessageResult'
  { -- | The delivery status of the message. Possible values are:
    --
    --
    --     * DUPLICATE - The endpoint address is a duplicate of another endpoint address. Amazon Pinpoint won't attempt to send the message again.
    --
    --
    --     * OPT_OUT - The user who's associated with the endpoint address has opted out of receiving messages from you. Amazon Pinpoint won't attempt to send the message again.
    --
    --
    --     * PERMANENT_FAILURE - An error occurred when delivering the message to the endpoint address. Amazon Pinpoint won't attempt to send the message again.
    --
    --
    --     * SUCCESSFUL - The message was successfully delivered to the endpoint address.
    --
    --
    --     * TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint won't attempt to send the message again.
    --
    --
    --     * THROTTLED - Amazon Pinpoint throttled the operation to send the message to the endpoint address.
    --
    --
    --     * TIMEOUT - The message couldn't be sent within the timeout period.
    --
    --
    --     * UNKNOWN_FAILURE - An unknown error occurred.
    deliveryStatus :: DeliveryStatus,
    -- | The status message for delivering the message.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
    updatedToken :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the message that was sent.
    messageId :: Lude.Maybe Lude.Text,
    -- | The downstream service status code for delivering the message.
    statusCode :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageResult' with the minimum fields required to make a request.
--
-- * 'deliveryStatus' - The delivery status of the message. Possible values are:
--
--
--     * DUPLICATE - The endpoint address is a duplicate of another endpoint address. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * OPT_OUT - The user who's associated with the endpoint address has opted out of receiving messages from you. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * PERMANENT_FAILURE - An error occurred when delivering the message to the endpoint address. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * SUCCESSFUL - The message was successfully delivered to the endpoint address.
--
--
--     * TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * THROTTLED - Amazon Pinpoint throttled the operation to send the message to the endpoint address.
--
--
--     * TIMEOUT - The message couldn't be sent within the timeout period.
--
--
--     * UNKNOWN_FAILURE - An unknown error occurred.
--
--
-- * 'statusMessage' - The status message for delivering the message.
-- * 'updatedToken' - For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
-- * 'messageId' - The unique identifier for the message that was sent.
-- * 'statusCode' - The downstream service status code for delivering the message.
mkMessageResult ::
  -- | 'deliveryStatus'
  DeliveryStatus ->
  -- | 'statusCode'
  Lude.Int ->
  MessageResult
mkMessageResult pDeliveryStatus_ pStatusCode_ =
  MessageResult'
    { deliveryStatus = pDeliveryStatus_,
      statusMessage = Lude.Nothing,
      updatedToken = Lude.Nothing,
      messageId = Lude.Nothing,
      statusCode = pStatusCode_
    }

-- | The delivery status of the message. Possible values are:
--
--
--     * DUPLICATE - The endpoint address is a duplicate of another endpoint address. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * OPT_OUT - The user who's associated with the endpoint address has opted out of receiving messages from you. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * PERMANENT_FAILURE - An error occurred when delivering the message to the endpoint address. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * SUCCESSFUL - The message was successfully delivered to the endpoint address.
--
--
--     * TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint won't attempt to send the message again.
--
--
--     * THROTTLED - Amazon Pinpoint throttled the operation to send the message to the endpoint address.
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
mrDeliveryStatus :: Lens.Lens' MessageResult DeliveryStatus
mrDeliveryStatus = Lens.lens (deliveryStatus :: MessageResult -> DeliveryStatus) (\s a -> s {deliveryStatus = a} :: MessageResult)
{-# DEPRECATED mrDeliveryStatus "Use generic-lens or generic-optics with 'deliveryStatus' instead." #-}

-- | The status message for delivering the message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrStatusMessage :: Lens.Lens' MessageResult (Lude.Maybe Lude.Text)
mrStatusMessage = Lens.lens (statusMessage :: MessageResult -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: MessageResult)
{-# DEPRECATED mrStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
--
-- /Note:/ Consider using 'updatedToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrUpdatedToken :: Lens.Lens' MessageResult (Lude.Maybe Lude.Text)
mrUpdatedToken = Lens.lens (updatedToken :: MessageResult -> Lude.Maybe Lude.Text) (\s a -> s {updatedToken = a} :: MessageResult)
{-# DEPRECATED mrUpdatedToken "Use generic-lens or generic-optics with 'updatedToken' instead." #-}

-- | The unique identifier for the message that was sent.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrMessageId :: Lens.Lens' MessageResult (Lude.Maybe Lude.Text)
mrMessageId = Lens.lens (messageId :: MessageResult -> Lude.Maybe Lude.Text) (\s a -> s {messageId = a} :: MessageResult)
{-# DEPRECATED mrMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The downstream service status code for delivering the message.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrStatusCode :: Lens.Lens' MessageResult Lude.Int
mrStatusCode = Lens.lens (statusCode :: MessageResult -> Lude.Int) (\s a -> s {statusCode = a} :: MessageResult)
{-# DEPRECATED mrStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON MessageResult where
  parseJSON =
    Lude.withObject
      "MessageResult"
      ( \x ->
          MessageResult'
            Lude.<$> (x Lude..: "DeliveryStatus")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "UpdatedToken")
            Lude.<*> (x Lude..:? "MessageId")
            Lude.<*> (x Lude..: "StatusCode")
      )
