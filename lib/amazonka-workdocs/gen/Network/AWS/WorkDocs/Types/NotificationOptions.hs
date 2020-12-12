{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.NotificationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.NotificationOptions
  ( NotificationOptions (..),

    -- * Smart constructor
    mkNotificationOptions,

    -- * Lenses
    noEmailMessage,
    noSendEmail,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Set of options which defines notification preferences of given action.
--
-- /See:/ 'mkNotificationOptions' smart constructor.
data NotificationOptions = NotificationOptions'
  { emailMessage ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    sendEmail :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationOptions' with the minimum fields required to make a request.
--
-- * 'emailMessage' - Text value to be included in the email body.
-- * 'sendEmail' - Boolean value to indicate an email notification should be sent to the receipients.
mkNotificationOptions ::
  NotificationOptions
mkNotificationOptions =
  NotificationOptions'
    { emailMessage = Lude.Nothing,
      sendEmail = Lude.Nothing
    }

-- | Text value to be included in the email body.
--
-- /Note:/ Consider using 'emailMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
noEmailMessage :: Lens.Lens' NotificationOptions (Lude.Maybe (Lude.Sensitive Lude.Text))
noEmailMessage = Lens.lens (emailMessage :: NotificationOptions -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {emailMessage = a} :: NotificationOptions)
{-# DEPRECATED noEmailMessage "Use generic-lens or generic-optics with 'emailMessage' instead." #-}

-- | Boolean value to indicate an email notification should be sent to the receipients.
--
-- /Note:/ Consider using 'sendEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
noSendEmail :: Lens.Lens' NotificationOptions (Lude.Maybe Lude.Bool)
noSendEmail = Lens.lens (sendEmail :: NotificationOptions -> Lude.Maybe Lude.Bool) (\s a -> s {sendEmail = a} :: NotificationOptions)
{-# DEPRECATED noSendEmail "Use generic-lens or generic-optics with 'sendEmail' instead." #-}

instance Lude.ToJSON NotificationOptions where
  toJSON NotificationOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EmailMessage" Lude..=) Lude.<$> emailMessage,
            ("SendEmail" Lude..=) Lude.<$> sendEmail
          ]
      )
