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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.EmailMessage as Types

-- | Set of options which defines notification preferences of given action.
--
-- /See:/ 'mkNotificationOptions' smart constructor.
data NotificationOptions = NotificationOptions'
  { -- | Text value to be included in the email body.
    emailMessage :: Core.Maybe Types.EmailMessage,
    -- | Boolean value to indicate an email notification should be sent to the receipients.
    sendEmail :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationOptions' value with any optional fields omitted.
mkNotificationOptions ::
  NotificationOptions
mkNotificationOptions =
  NotificationOptions'
    { emailMessage = Core.Nothing,
      sendEmail = Core.Nothing
    }

-- | Text value to be included in the email body.
--
-- /Note:/ Consider using 'emailMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
noEmailMessage :: Lens.Lens' NotificationOptions (Core.Maybe Types.EmailMessage)
noEmailMessage = Lens.field @"emailMessage"
{-# DEPRECATED noEmailMessage "Use generic-lens or generic-optics with 'emailMessage' instead." #-}

-- | Boolean value to indicate an email notification should be sent to the receipients.
--
-- /Note:/ Consider using 'sendEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
noSendEmail :: Lens.Lens' NotificationOptions (Core.Maybe Core.Bool)
noSendEmail = Lens.field @"sendEmail"
{-# DEPRECATED noSendEmail "Use generic-lens or generic-optics with 'sendEmail' instead." #-}

instance Core.FromJSON NotificationOptions where
  toJSON NotificationOptions {..} =
    Core.object
      ( Core.catMaybes
          [ ("EmailMessage" Core..=) Core.<$> emailMessage,
            ("SendEmail" Core..=) Core.<$> sendEmail
          ]
      )
