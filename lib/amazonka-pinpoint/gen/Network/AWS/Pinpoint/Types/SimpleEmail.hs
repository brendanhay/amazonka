{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SimpleEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SimpleEmail
  ( SimpleEmail (..),

    -- * Smart constructor
    mkSimpleEmail,

    -- * Lenses
    seHtmlPart,
    seSubject,
    seTextPart,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.SimpleEmailPart as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the contents of an email message, composed of a subject, a text part, and an HTML part.
--
-- /See:/ 'mkSimpleEmail' smart constructor.
data SimpleEmail = SimpleEmail'
  { -- | The body of the email message, in HTML format. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
    htmlPart :: Core.Maybe Types.SimpleEmailPart,
    -- | The subject line, or title, of the email.
    subject :: Core.Maybe Types.SimpleEmailPart,
    -- | The body of the email message, in plain text format. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
    textPart :: Core.Maybe Types.SimpleEmailPart
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SimpleEmail' value with any optional fields omitted.
mkSimpleEmail ::
  SimpleEmail
mkSimpleEmail =
  SimpleEmail'
    { htmlPart = Core.Nothing,
      subject = Core.Nothing,
      textPart = Core.Nothing
    }

-- | The body of the email message, in HTML format. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
--
-- /Note:/ Consider using 'htmlPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seHtmlPart :: Lens.Lens' SimpleEmail (Core.Maybe Types.SimpleEmailPart)
seHtmlPart = Lens.field @"htmlPart"
{-# DEPRECATED seHtmlPart "Use generic-lens or generic-optics with 'htmlPart' instead." #-}

-- | The subject line, or title, of the email.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seSubject :: Lens.Lens' SimpleEmail (Core.Maybe Types.SimpleEmailPart)
seSubject = Lens.field @"subject"
{-# DEPRECATED seSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The body of the email message, in plain text format. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
--
-- /Note:/ Consider using 'textPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTextPart :: Lens.Lens' SimpleEmail (Core.Maybe Types.SimpleEmailPart)
seTextPart = Lens.field @"textPart"
{-# DEPRECATED seTextPart "Use generic-lens or generic-optics with 'textPart' instead." #-}

instance Core.FromJSON SimpleEmail where
  toJSON SimpleEmail {..} =
    Core.object
      ( Core.catMaybes
          [ ("HtmlPart" Core..=) Core.<$> htmlPart,
            ("Subject" Core..=) Core.<$> subject,
            ("TextPart" Core..=) Core.<$> textPart
          ]
      )
