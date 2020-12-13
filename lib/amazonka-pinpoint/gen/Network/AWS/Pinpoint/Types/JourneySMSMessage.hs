{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneySMSMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneySMSMessage
  ( JourneySMSMessage (..),

    -- * Smart constructor
    mkJourneySMSMessage,

    -- * Lenses
    jsmsmMessageType,
    jsmsmSenderId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.MessageType
import qualified Network.AWS.Prelude as Lude

-- | Specifies the sender ID and message type for an SMS message that's sent to participants in a journey.
--
-- /See:/ 'mkJourneySMSMessage' smart constructor.
data JourneySMSMessage = JourneySMSMessage'
  { -- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
    messageType :: Lude.Maybe MessageType,
    -- | The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region. For more information, see <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-countries.html Supported Countries and Regions> in the Amazon Pinpoint User Guide.
    senderId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneySMSMessage' with the minimum fields required to make a request.
--
-- * 'messageType' - The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
-- * 'senderId' - The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region. For more information, see <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-countries.html Supported Countries and Regions> in the Amazon Pinpoint User Guide.
mkJourneySMSMessage ::
  JourneySMSMessage
mkJourneySMSMessage =
  JourneySMSMessage'
    { messageType = Lude.Nothing,
      senderId = Lude.Nothing
    }

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsmsmMessageType :: Lens.Lens' JourneySMSMessage (Lude.Maybe MessageType)
jsmsmMessageType = Lens.lens (messageType :: JourneySMSMessage -> Lude.Maybe MessageType) (\s a -> s {messageType = a} :: JourneySMSMessage)
{-# DEPRECATED jsmsmMessageType "Use generic-lens or generic-optics with 'messageType' instead." #-}

-- | The sender ID to display as the sender of the message on a recipient's device. Support for sender IDs varies by country or region. For more information, see <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-countries.html Supported Countries and Regions> in the Amazon Pinpoint User Guide.
--
-- /Note:/ Consider using 'senderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsmsmSenderId :: Lens.Lens' JourneySMSMessage (Lude.Maybe Lude.Text)
jsmsmSenderId = Lens.lens (senderId :: JourneySMSMessage -> Lude.Maybe Lude.Text) (\s a -> s {senderId = a} :: JourneySMSMessage)
{-# DEPRECATED jsmsmSenderId "Use generic-lens or generic-optics with 'senderId' instead." #-}

instance Lude.FromJSON JourneySMSMessage where
  parseJSON =
    Lude.withObject
      "JourneySMSMessage"
      ( \x ->
          JourneySMSMessage'
            Lude.<$> (x Lude..:? "MessageType") Lude.<*> (x Lude..:? "SenderId")
      )

instance Lude.ToJSON JourneySMSMessage where
  toJSON JourneySMSMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MessageType" Lude..=) Lude.<$> messageType,
            ("SenderId" Lude..=) Lude.<$> senderId
          ]
      )
