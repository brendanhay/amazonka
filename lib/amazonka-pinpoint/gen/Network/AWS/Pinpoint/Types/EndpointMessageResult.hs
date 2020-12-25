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
    emrDeliveryStatus,
    emrStatusCode,
    emrAddress,
    emrMessageId,
    emrStatusMessage,
    emrUpdatedToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.DeliveryStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the delivery status and results of sending a message directly to an endpoint.
--
-- /See:/ 'mkEndpointMessageResult' smart constructor.
data EndpointMessageResult = EndpointMessageResult'
  { -- | The delivery status of the message. Possible values are:
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
    deliveryStatus :: Types.DeliveryStatus,
    -- | The downstream service status code for delivering the message.
    statusCode :: Core.Int,
    -- | The endpoint address that the message was delivered to.
    address :: Core.Maybe Core.Text,
    -- | The unique identifier for the message that was sent.
    messageId :: Core.Maybe Core.Text,
    -- | The status message for delivering the message.
    statusMessage :: Core.Maybe Core.Text,
    -- | For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
    updatedToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointMessageResult' value with any optional fields omitted.
mkEndpointMessageResult ::
  -- | 'deliveryStatus'
  Types.DeliveryStatus ->
  -- | 'statusCode'
  Core.Int ->
  EndpointMessageResult
mkEndpointMessageResult deliveryStatus statusCode =
  EndpointMessageResult'
    { deliveryStatus,
      statusCode,
      address = Core.Nothing,
      messageId = Core.Nothing,
      statusMessage = Core.Nothing,
      updatedToken = Core.Nothing
    }

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
emrDeliveryStatus :: Lens.Lens' EndpointMessageResult Types.DeliveryStatus
emrDeliveryStatus = Lens.field @"deliveryStatus"
{-# DEPRECATED emrDeliveryStatus "Use generic-lens or generic-optics with 'deliveryStatus' instead." #-}

-- | The downstream service status code for delivering the message.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrStatusCode :: Lens.Lens' EndpointMessageResult Core.Int
emrStatusCode = Lens.field @"statusCode"
{-# DEPRECATED emrStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | The endpoint address that the message was delivered to.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrAddress :: Lens.Lens' EndpointMessageResult (Core.Maybe Core.Text)
emrAddress = Lens.field @"address"
{-# DEPRECATED emrAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The unique identifier for the message that was sent.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrMessageId :: Lens.Lens' EndpointMessageResult (Core.Maybe Core.Text)
emrMessageId = Lens.field @"messageId"
{-# DEPRECATED emrMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The status message for delivering the message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrStatusMessage :: Lens.Lens' EndpointMessageResult (Core.Maybe Core.Text)
emrStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED emrStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
--
-- /Note:/ Consider using 'updatedToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emrUpdatedToken :: Lens.Lens' EndpointMessageResult (Core.Maybe Core.Text)
emrUpdatedToken = Lens.field @"updatedToken"
{-# DEPRECATED emrUpdatedToken "Use generic-lens or generic-optics with 'updatedToken' instead." #-}

instance Core.FromJSON EndpointMessageResult where
  parseJSON =
    Core.withObject "EndpointMessageResult" Core.$
      \x ->
        EndpointMessageResult'
          Core.<$> (x Core..: "DeliveryStatus")
          Core.<*> (x Core..: "StatusCode")
          Core.<*> (x Core..:? "Address")
          Core.<*> (x Core..:? "MessageId")
          Core.<*> (x Core..:? "StatusMessage")
          Core.<*> (x Core..:? "UpdatedToken")
