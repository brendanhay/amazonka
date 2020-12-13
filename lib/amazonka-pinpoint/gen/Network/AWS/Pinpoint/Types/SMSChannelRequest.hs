{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSChannelRequest
  ( SMSChannelRequest (..),

    -- * Smart constructor
    mkSMSChannelRequest,

    -- * Lenses
    smscrShortCode,
    smscrEnabled,
    smscrSenderId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the status and settings of the SMS channel for an application.
--
-- /See:/ 'mkSMSChannelRequest' smart constructor.
data SMSChannelRequest = SMSChannelRequest'
  { -- | The registered short code that you want to use when you send messages through the SMS channel.
    shortCode :: Lude.Maybe Lude.Text,
    -- | Specifies whether to enable the SMS channel for the application.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The identity that you want to display on recipients' devices when they receive messages from the SMS channel.
    senderId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SMSChannelRequest' with the minimum fields required to make a request.
--
-- * 'shortCode' - The registered short code that you want to use when you send messages through the SMS channel.
-- * 'enabled' - Specifies whether to enable the SMS channel for the application.
-- * 'senderId' - The identity that you want to display on recipients' devices when they receive messages from the SMS channel.
mkSMSChannelRequest ::
  SMSChannelRequest
mkSMSChannelRequest =
  SMSChannelRequest'
    { shortCode = Lude.Nothing,
      enabled = Lude.Nothing,
      senderId = Lude.Nothing
    }

-- | The registered short code that you want to use when you send messages through the SMS channel.
--
-- /Note:/ Consider using 'shortCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrShortCode :: Lens.Lens' SMSChannelRequest (Lude.Maybe Lude.Text)
smscrShortCode = Lens.lens (shortCode :: SMSChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {shortCode = a} :: SMSChannelRequest)
{-# DEPRECATED smscrShortCode "Use generic-lens or generic-optics with 'shortCode' instead." #-}

-- | Specifies whether to enable the SMS channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrEnabled :: Lens.Lens' SMSChannelRequest (Lude.Maybe Lude.Bool)
smscrEnabled = Lens.lens (enabled :: SMSChannelRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: SMSChannelRequest)
{-# DEPRECATED smscrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The identity that you want to display on recipients' devices when they receive messages from the SMS channel.
--
-- /Note:/ Consider using 'senderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrSenderId :: Lens.Lens' SMSChannelRequest (Lude.Maybe Lude.Text)
smscrSenderId = Lens.lens (senderId :: SMSChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {senderId = a} :: SMSChannelRequest)
{-# DEPRECATED smscrSenderId "Use generic-lens or generic-optics with 'senderId' instead." #-}

instance Lude.ToJSON SMSChannelRequest where
  toJSON SMSChannelRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ShortCode" Lude..=) Lude.<$> shortCode,
            ("Enabled" Lude..=) Lude.<$> enabled,
            ("SenderId" Lude..=) Lude.<$> senderId
          ]
      )
