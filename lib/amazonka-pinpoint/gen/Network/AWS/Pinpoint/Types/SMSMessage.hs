{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSMessage
  ( SMSMessage (..),

    -- * Smart constructor
    mkSMSMessage,

    -- * Lenses
    smsmBody,
    smsmKeyword,
    smsmMediaUrl,
    smsmMessageType,
    smsmOriginationNumber,
    smsmSenderId,
    smsmSubstitutions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.MessageType as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the default settings for a one-time SMS message that's sent directly to an endpoint.
--
-- /See:/ 'mkSMSMessage' smart constructor.
data SMSMessage = SMSMessage'
  { -- | The body of the SMS message.
    body :: Core.Maybe Core.Text,
    -- | The SMS program name that you provided to AWS Support when you requested your dedicated number.
    keyword :: Core.Maybe Core.Text,
    -- | This field is reserved for future use.
    mediaUrl :: Core.Maybe Core.Text,
    -- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
    messageType :: Core.Maybe Types.MessageType,
    -- | The number to send the SMS message from. This value should be one of the dedicated long or short codes that's assigned to your AWS account. If you don't specify a long or short code, Amazon Pinpoint assigns a random long code to the SMS message and sends the message from that code.
    originationNumber :: Core.Maybe Core.Text,
    -- | The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region.
    senderId :: Core.Maybe Core.Text,
    -- | The message variables to use in the SMS message. You can override the default variables with individual address variables.
    substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SMSMessage' value with any optional fields omitted.
mkSMSMessage ::
  SMSMessage
mkSMSMessage =
  SMSMessage'
    { body = Core.Nothing,
      keyword = Core.Nothing,
      mediaUrl = Core.Nothing,
      messageType = Core.Nothing,
      originationNumber = Core.Nothing,
      senderId = Core.Nothing,
      substitutions = Core.Nothing
    }

-- | The body of the SMS message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmBody :: Lens.Lens' SMSMessage (Core.Maybe Core.Text)
smsmBody = Lens.field @"body"
{-# DEPRECATED smsmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The SMS program name that you provided to AWS Support when you requested your dedicated number.
--
-- /Note:/ Consider using 'keyword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmKeyword :: Lens.Lens' SMSMessage (Core.Maybe Core.Text)
smsmKeyword = Lens.field @"keyword"
{-# DEPRECATED smsmKeyword "Use generic-lens or generic-optics with 'keyword' instead." #-}

-- | This field is reserved for future use.
--
-- /Note:/ Consider using 'mediaUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmMediaUrl :: Lens.Lens' SMSMessage (Core.Maybe Core.Text)
smsmMediaUrl = Lens.field @"mediaUrl"
{-# DEPRECATED smsmMediaUrl "Use generic-lens or generic-optics with 'mediaUrl' instead." #-}

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmMessageType :: Lens.Lens' SMSMessage (Core.Maybe Types.MessageType)
smsmMessageType = Lens.field @"messageType"
{-# DEPRECATED smsmMessageType "Use generic-lens or generic-optics with 'messageType' instead." #-}

-- | The number to send the SMS message from. This value should be one of the dedicated long or short codes that's assigned to your AWS account. If you don't specify a long or short code, Amazon Pinpoint assigns a random long code to the SMS message and sends the message from that code.
--
-- /Note:/ Consider using 'originationNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmOriginationNumber :: Lens.Lens' SMSMessage (Core.Maybe Core.Text)
smsmOriginationNumber = Lens.field @"originationNumber"
{-# DEPRECATED smsmOriginationNumber "Use generic-lens or generic-optics with 'originationNumber' instead." #-}

-- | The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region.
--
-- /Note:/ Consider using 'senderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmSenderId :: Lens.Lens' SMSMessage (Core.Maybe Core.Text)
smsmSenderId = Lens.field @"senderId"
{-# DEPRECATED smsmSenderId "Use generic-lens or generic-optics with 'senderId' instead." #-}

-- | The message variables to use in the SMS message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmSubstitutions :: Lens.Lens' SMSMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
smsmSubstitutions = Lens.field @"substitutions"
{-# DEPRECATED smsmSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

instance Core.FromJSON SMSMessage where
  toJSON SMSMessage {..} =
    Core.object
      ( Core.catMaybes
          [ ("Body" Core..=) Core.<$> body,
            ("Keyword" Core..=) Core.<$> keyword,
            ("MediaUrl" Core..=) Core.<$> mediaUrl,
            ("MessageType" Core..=) Core.<$> messageType,
            ("OriginationNumber" Core..=) Core.<$> originationNumber,
            ("SenderId" Core..=) Core.<$> senderId,
            ("Substitutions" Core..=) Core.<$> substitutions
          ]
      )
