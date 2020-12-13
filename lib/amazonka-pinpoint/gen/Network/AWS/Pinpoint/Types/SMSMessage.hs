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
    smsmSubstitutions,
    smsmOriginationNumber,
    smsmBody,
    smsmMessageType,
    smsmSenderId,
    smsmMediaURL,
    smsmKeyword,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.MessageType
import qualified Network.AWS.Prelude as Lude

-- | Specifies the default settings for a one-time SMS message that's sent directly to an endpoint.
--
-- /See:/ 'mkSMSMessage' smart constructor.
data SMSMessage = SMSMessage'
  { -- | The message variables to use in the SMS message. You can override the default variables with individual address variables.
    substitutions :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | The number to send the SMS message from. This value should be one of the dedicated long or short codes that's assigned to your AWS account. If you don't specify a long or short code, Amazon Pinpoint assigns a random long code to the SMS message and sends the message from that code.
    originationNumber :: Lude.Maybe Lude.Text,
    -- | The body of the SMS message.
    body :: Lude.Maybe Lude.Text,
    -- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
    messageType :: Lude.Maybe MessageType,
    -- | The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region.
    senderId :: Lude.Maybe Lude.Text,
    -- | This field is reserved for future use.
    mediaURL :: Lude.Maybe Lude.Text,
    -- | The SMS program name that you provided to AWS Support when you requested your dedicated number.
    keyword :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SMSMessage' with the minimum fields required to make a request.
--
-- * 'substitutions' - The message variables to use in the SMS message. You can override the default variables with individual address variables.
-- * 'originationNumber' - The number to send the SMS message from. This value should be one of the dedicated long or short codes that's assigned to your AWS account. If you don't specify a long or short code, Amazon Pinpoint assigns a random long code to the SMS message and sends the message from that code.
-- * 'body' - The body of the SMS message.
-- * 'messageType' - The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
-- * 'senderId' - The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region.
-- * 'mediaURL' - This field is reserved for future use.
-- * 'keyword' - The SMS program name that you provided to AWS Support when you requested your dedicated number.
mkSMSMessage ::
  SMSMessage
mkSMSMessage =
  SMSMessage'
    { substitutions = Lude.Nothing,
      originationNumber = Lude.Nothing,
      body = Lude.Nothing,
      messageType = Lude.Nothing,
      senderId = Lude.Nothing,
      mediaURL = Lude.Nothing,
      keyword = Lude.Nothing
    }

-- | The message variables to use in the SMS message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmSubstitutions :: Lens.Lens' SMSMessage (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
smsmSubstitutions = Lens.lens (substitutions :: SMSMessage -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: SMSMessage)
{-# DEPRECATED smsmSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | The number to send the SMS message from. This value should be one of the dedicated long or short codes that's assigned to your AWS account. If you don't specify a long or short code, Amazon Pinpoint assigns a random long code to the SMS message and sends the message from that code.
--
-- /Note:/ Consider using 'originationNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmOriginationNumber :: Lens.Lens' SMSMessage (Lude.Maybe Lude.Text)
smsmOriginationNumber = Lens.lens (originationNumber :: SMSMessage -> Lude.Maybe Lude.Text) (\s a -> s {originationNumber = a} :: SMSMessage)
{-# DEPRECATED smsmOriginationNumber "Use generic-lens or generic-optics with 'originationNumber' instead." #-}

-- | The body of the SMS message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmBody :: Lens.Lens' SMSMessage (Lude.Maybe Lude.Text)
smsmBody = Lens.lens (body :: SMSMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: SMSMessage)
{-# DEPRECATED smsmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmMessageType :: Lens.Lens' SMSMessage (Lude.Maybe MessageType)
smsmMessageType = Lens.lens (messageType :: SMSMessage -> Lude.Maybe MessageType) (\s a -> s {messageType = a} :: SMSMessage)
{-# DEPRECATED smsmMessageType "Use generic-lens or generic-optics with 'messageType' instead." #-}

-- | The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region.
--
-- /Note:/ Consider using 'senderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmSenderId :: Lens.Lens' SMSMessage (Lude.Maybe Lude.Text)
smsmSenderId = Lens.lens (senderId :: SMSMessage -> Lude.Maybe Lude.Text) (\s a -> s {senderId = a} :: SMSMessage)
{-# DEPRECATED smsmSenderId "Use generic-lens or generic-optics with 'senderId' instead." #-}

-- | This field is reserved for future use.
--
-- /Note:/ Consider using 'mediaURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmMediaURL :: Lens.Lens' SMSMessage (Lude.Maybe Lude.Text)
smsmMediaURL = Lens.lens (mediaURL :: SMSMessage -> Lude.Maybe Lude.Text) (\s a -> s {mediaURL = a} :: SMSMessage)
{-# DEPRECATED smsmMediaURL "Use generic-lens or generic-optics with 'mediaURL' instead." #-}

-- | The SMS program name that you provided to AWS Support when you requested your dedicated number.
--
-- /Note:/ Consider using 'keyword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmKeyword :: Lens.Lens' SMSMessage (Lude.Maybe Lude.Text)
smsmKeyword = Lens.lens (keyword :: SMSMessage -> Lude.Maybe Lude.Text) (\s a -> s {keyword = a} :: SMSMessage)
{-# DEPRECATED smsmKeyword "Use generic-lens or generic-optics with 'keyword' instead." #-}

instance Lude.ToJSON SMSMessage where
  toJSON SMSMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("OriginationNumber" Lude..=) Lude.<$> originationNumber,
            ("Body" Lude..=) Lude.<$> body,
            ("MessageType" Lude..=) Lude.<$> messageType,
            ("SenderId" Lude..=) Lude.<$> senderId,
            ("MediaUrl" Lude..=) Lude.<$> mediaURL,
            ("Keyword" Lude..=) Lude.<$> keyword
          ]
      )
