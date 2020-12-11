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
    seSubject,
    seTextPart,
    seHTMLPart,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SimpleEmailPart
import qualified Network.AWS.Prelude as Lude

-- | Specifies the contents of an email message, composed of a subject, a text part, and an HTML part.
--
-- /See:/ 'mkSimpleEmail' smart constructor.
data SimpleEmail = SimpleEmail'
  { subject ::
      Lude.Maybe SimpleEmailPart,
    textPart :: Lude.Maybe SimpleEmailPart,
    htmlPart :: Lude.Maybe SimpleEmailPart
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SimpleEmail' with the minimum fields required to make a request.
--
-- * 'htmlPart' - The body of the email message, in HTML format. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
-- * 'subject' - The subject line, or title, of the email.
-- * 'textPart' - The body of the email message, in plain text format. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
mkSimpleEmail ::
  SimpleEmail
mkSimpleEmail =
  SimpleEmail'
    { subject = Lude.Nothing,
      textPart = Lude.Nothing,
      htmlPart = Lude.Nothing
    }

-- | The subject line, or title, of the email.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seSubject :: Lens.Lens' SimpleEmail (Lude.Maybe SimpleEmailPart)
seSubject = Lens.lens (subject :: SimpleEmail -> Lude.Maybe SimpleEmailPart) (\s a -> s {subject = a} :: SimpleEmail)
{-# DEPRECATED seSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The body of the email message, in plain text format. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
--
-- /Note:/ Consider using 'textPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTextPart :: Lens.Lens' SimpleEmail (Lude.Maybe SimpleEmailPart)
seTextPart = Lens.lens (textPart :: SimpleEmail -> Lude.Maybe SimpleEmailPart) (\s a -> s {textPart = a} :: SimpleEmail)
{-# DEPRECATED seTextPart "Use generic-lens or generic-optics with 'textPart' instead." #-}

-- | The body of the email message, in HTML format. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
--
-- /Note:/ Consider using 'htmlPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seHTMLPart :: Lens.Lens' SimpleEmail (Lude.Maybe SimpleEmailPart)
seHTMLPart = Lens.lens (htmlPart :: SimpleEmail -> Lude.Maybe SimpleEmailPart) (\s a -> s {htmlPart = a} :: SimpleEmail)
{-# DEPRECATED seHTMLPart "Use generic-lens or generic-optics with 'htmlPart' instead." #-}

instance Lude.ToJSON SimpleEmail where
  toJSON SimpleEmail' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Subject" Lude..=) Lude.<$> subject,
            ("TextPart" Lude..=) Lude.<$> textPart,
            ("HtmlPart" Lude..=) Lude.<$> htmlPart
          ]
      )
