-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignSmsMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignSmsMessage
  ( CampaignSmsMessage (..),

    -- * Smart constructor
    mkCampaignSmsMessage,

    -- * Lenses
    csmBody,
    csmMessageType,
    csmSenderId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.MessageType
import qualified Network.AWS.Prelude as Lude

-- | Specifies the content and settings for an SMS message that's sent to recipients of a campaign.
--
-- /See:/ 'mkCampaignSmsMessage' smart constructor.
data CampaignSmsMessage = CampaignSmsMessage'
  { body ::
      Lude.Maybe Lude.Text,
    messageType :: Lude.Maybe MessageType,
    senderId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CampaignSmsMessage' with the minimum fields required to make a request.
--
-- * 'body' - The body of the SMS message.
-- * 'messageType' - The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
-- * 'senderId' - The sender ID to display on recipients' devices when they receive the SMS message.
mkCampaignSmsMessage ::
  CampaignSmsMessage
mkCampaignSmsMessage =
  CampaignSmsMessage'
    { body = Lude.Nothing,
      messageType = Lude.Nothing,
      senderId = Lude.Nothing
    }

-- | The body of the SMS message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmBody :: Lens.Lens' CampaignSmsMessage (Lude.Maybe Lude.Text)
csmBody = Lens.lens (body :: CampaignSmsMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: CampaignSmsMessage)
{-# DEPRECATED csmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmMessageType :: Lens.Lens' CampaignSmsMessage (Lude.Maybe MessageType)
csmMessageType = Lens.lens (messageType :: CampaignSmsMessage -> Lude.Maybe MessageType) (\s a -> s {messageType = a} :: CampaignSmsMessage)
{-# DEPRECATED csmMessageType "Use generic-lens or generic-optics with 'messageType' instead." #-}

-- | The sender ID to display on recipients' devices when they receive the SMS message.
--
-- /Note:/ Consider using 'senderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmSenderId :: Lens.Lens' CampaignSmsMessage (Lude.Maybe Lude.Text)
csmSenderId = Lens.lens (senderId :: CampaignSmsMessage -> Lude.Maybe Lude.Text) (\s a -> s {senderId = a} :: CampaignSmsMessage)
{-# DEPRECATED csmSenderId "Use generic-lens or generic-optics with 'senderId' instead." #-}

instance Lude.FromJSON CampaignSmsMessage where
  parseJSON =
    Lude.withObject
      "CampaignSmsMessage"
      ( \x ->
          CampaignSmsMessage'
            Lude.<$> (x Lude..:? "Body")
            Lude.<*> (x Lude..:? "MessageType")
            Lude.<*> (x Lude..:? "SenderId")
      )

instance Lude.ToJSON CampaignSmsMessage where
  toJSON CampaignSmsMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Body" Lude..=) Lude.<$> body,
            ("MessageType" Lude..=) Lude.<$> messageType,
            ("SenderId" Lude..=) Lude.<$> senderId
          ]
      )
