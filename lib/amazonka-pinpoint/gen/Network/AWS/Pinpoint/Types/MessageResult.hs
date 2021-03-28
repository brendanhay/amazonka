{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.MessageResult
  ( MessageResult (..)
  -- * Smart constructor
  , mkMessageResult
  -- * Lenses
  , mrDeliveryStatus
  , mrStatusCode
  , mrMessageId
  , mrStatusMessage
  , mrUpdatedToken
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.DeliveryStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the results of sending a message directly to an endpoint address.
--
-- /See:/ 'mkMessageResult' smart constructor.
data MessageResult = MessageResult'
  { deliveryStatus :: Types.DeliveryStatus
    -- ^ The delivery status of the message. Possible values are:
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
  , statusCode :: Core.Int
    -- ^ The downstream service status code for delivering the message.
  , messageId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the message that was sent.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ The status message for delivering the message.
  , updatedToken :: Core.Maybe Core.Text
    -- ^ For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MessageResult' value with any optional fields omitted.
mkMessageResult
    :: Types.DeliveryStatus -- ^ 'deliveryStatus'
    -> Core.Int -- ^ 'statusCode'
    -> MessageResult
mkMessageResult deliveryStatus statusCode
  = MessageResult'{deliveryStatus, statusCode,
                   messageId = Core.Nothing, statusMessage = Core.Nothing,
                   updatedToken = Core.Nothing}

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
mrDeliveryStatus :: Lens.Lens' MessageResult Types.DeliveryStatus
mrDeliveryStatus = Lens.field @"deliveryStatus"
{-# INLINEABLE mrDeliveryStatus #-}
{-# DEPRECATED deliveryStatus "Use generic-lens or generic-optics with 'deliveryStatus' instead"  #-}

-- | The downstream service status code for delivering the message.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrStatusCode :: Lens.Lens' MessageResult Core.Int
mrStatusCode = Lens.field @"statusCode"
{-# INLINEABLE mrStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | The unique identifier for the message that was sent.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrMessageId :: Lens.Lens' MessageResult (Core.Maybe Core.Text)
mrMessageId = Lens.field @"messageId"
{-# INLINEABLE mrMessageId #-}
{-# DEPRECATED messageId "Use generic-lens or generic-optics with 'messageId' instead"  #-}

-- | The status message for delivering the message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrStatusMessage :: Lens.Lens' MessageResult (Core.Maybe Core.Text)
mrStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE mrStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
--
-- /Note:/ Consider using 'updatedToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrUpdatedToken :: Lens.Lens' MessageResult (Core.Maybe Core.Text)
mrUpdatedToken = Lens.field @"updatedToken"
{-# INLINEABLE mrUpdatedToken #-}
{-# DEPRECATED updatedToken "Use generic-lens or generic-optics with 'updatedToken' instead"  #-}

instance Core.FromJSON MessageResult where
        parseJSON
          = Core.withObject "MessageResult" Core.$
              \ x ->
                MessageResult' Core.<$>
                  (x Core..: "DeliveryStatus") Core.<*> x Core..: "StatusCode"
                    Core.<*> x Core..:? "MessageId"
                    Core.<*> x Core..:? "StatusMessage"
                    Core.<*> x Core..:? "UpdatedToken"
