{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Pinpoint.Types.MessageType as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the content and settings for an SMS message that's sent to recipients of a campaign.
--
-- /See:/ 'mkCampaignSmsMessage' smart constructor.
data CampaignSmsMessage = CampaignSmsMessage'
  { -- | The body of the SMS message.
    body :: Core.Maybe Core.Text,
    -- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
    messageType :: Core.Maybe Types.MessageType,
    -- | The sender ID to display on recipients' devices when they receive the SMS message.
    senderId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CampaignSmsMessage' value with any optional fields omitted.
mkCampaignSmsMessage ::
  CampaignSmsMessage
mkCampaignSmsMessage =
  CampaignSmsMessage'
    { body = Core.Nothing,
      messageType = Core.Nothing,
      senderId = Core.Nothing
    }

-- | The body of the SMS message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmBody :: Lens.Lens' CampaignSmsMessage (Core.Maybe Core.Text)
csmBody = Lens.field @"body"
{-# DEPRECATED csmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that are critical or time-sensitive, such as a one-time passwords) and PROMOTIONAL (for messsages that aren't critical or time-sensitive, such as marketing messages).
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmMessageType :: Lens.Lens' CampaignSmsMessage (Core.Maybe Types.MessageType)
csmMessageType = Lens.field @"messageType"
{-# DEPRECATED csmMessageType "Use generic-lens or generic-optics with 'messageType' instead." #-}

-- | The sender ID to display on recipients' devices when they receive the SMS message.
--
-- /Note:/ Consider using 'senderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmSenderId :: Lens.Lens' CampaignSmsMessage (Core.Maybe Core.Text)
csmSenderId = Lens.field @"senderId"
{-# DEPRECATED csmSenderId "Use generic-lens or generic-optics with 'senderId' instead." #-}

instance Core.FromJSON CampaignSmsMessage where
  toJSON CampaignSmsMessage {..} =
    Core.object
      ( Core.catMaybes
          [ ("Body" Core..=) Core.<$> body,
            ("MessageType" Core..=) Core.<$> messageType,
            ("SenderId" Core..=) Core.<$> senderId
          ]
      )

instance Core.FromJSON CampaignSmsMessage where
  parseJSON =
    Core.withObject "CampaignSmsMessage" Core.$
      \x ->
        CampaignSmsMessage'
          Core.<$> (x Core..:? "Body")
          Core.<*> (x Core..:? "MessageType")
          Core.<*> (x Core..:? "SenderId")
