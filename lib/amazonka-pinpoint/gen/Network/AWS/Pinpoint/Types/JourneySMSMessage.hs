{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneySMSMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JourneySMSMessage
  ( JourneySMSMessage (..)
  -- * Smart constructor
  , mkJourneySMSMessage
  -- * Lenses
  , jsmsmMessageType
  , jsmsmSenderId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.MessageType as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the sender ID and message type for an SMS message that's sent to participants in a journey.
--
-- /See:/ 'mkJourneySMSMessage' smart constructor.
data JourneySMSMessage = JourneySMSMessage'
  { messageType :: Core.Maybe Types.MessageType
    -- ^ The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
  , senderId :: Core.Maybe Core.Text
    -- ^ The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region. For more information, see <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-countries.html Supported Countries and Regions> in the Amazon Pinpoint User Guide.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JourneySMSMessage' value with any optional fields omitted.
mkJourneySMSMessage
    :: JourneySMSMessage
mkJourneySMSMessage
  = JourneySMSMessage'{messageType = Core.Nothing,
                       senderId = Core.Nothing}

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsmsmMessageType :: Lens.Lens' JourneySMSMessage (Core.Maybe Types.MessageType)
jsmsmMessageType = Lens.field @"messageType"
{-# INLINEABLE jsmsmMessageType #-}
{-# DEPRECATED messageType "Use generic-lens or generic-optics with 'messageType' instead"  #-}

-- | The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region. For more information, see <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-countries.html Supported Countries and Regions> in the Amazon Pinpoint User Guide.
--
-- /Note:/ Consider using 'senderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsmsmSenderId :: Lens.Lens' JourneySMSMessage (Core.Maybe Core.Text)
jsmsmSenderId = Lens.field @"senderId"
{-# INLINEABLE jsmsmSenderId #-}
{-# DEPRECATED senderId "Use generic-lens or generic-optics with 'senderId' instead"  #-}

instance Core.FromJSON JourneySMSMessage where
        toJSON JourneySMSMessage{..}
          = Core.object
              (Core.catMaybes
                 [("MessageType" Core..=) Core.<$> messageType,
                  ("SenderId" Core..=) Core.<$> senderId])

instance Core.FromJSON JourneySMSMessage where
        parseJSON
          = Core.withObject "JourneySMSMessage" Core.$
              \ x ->
                JourneySMSMessage' Core.<$>
                  (x Core..:? "MessageType") Core.<*> x Core..:? "SenderId"
