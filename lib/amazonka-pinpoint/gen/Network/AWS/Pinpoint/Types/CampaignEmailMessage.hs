-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignEmailMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignEmailMessage
  ( CampaignEmailMessage (..),

    -- * Smart constructor
    mkCampaignEmailMessage,

    -- * Lenses
    cemBody,
    cemFromAddress,
    cemHTMLBody,
    cemTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the content and "From" address for an email message that's sent to recipients of a campaign.
--
-- /See:/ 'mkCampaignEmailMessage' smart constructor.
data CampaignEmailMessage = CampaignEmailMessage'
  { body ::
      Lude.Maybe Lude.Text,
    fromAddress :: Lude.Maybe Lude.Text,
    htmlBody :: Lude.Maybe Lude.Text,
    title :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CampaignEmailMessage' with the minimum fields required to make a request.
--
-- * 'body' - The body of the email for recipients whose email clients don't render HTML content.
-- * 'fromAddress' - The verified email address to send the email from. The default address is the FromAddress specified for the email channel for the application.
-- * 'htmlBody' - The body of the email, in HTML format, for recipients whose email clients render HTML content.
-- * 'title' - The subject line, or title, of the email.
mkCampaignEmailMessage ::
  CampaignEmailMessage
mkCampaignEmailMessage =
  CampaignEmailMessage'
    { body = Lude.Nothing,
      fromAddress = Lude.Nothing,
      htmlBody = Lude.Nothing,
      title = Lude.Nothing
    }

-- | The body of the email for recipients whose email clients don't render HTML content.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemBody :: Lens.Lens' CampaignEmailMessage (Lude.Maybe Lude.Text)
cemBody = Lens.lens (body :: CampaignEmailMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: CampaignEmailMessage)
{-# DEPRECATED cemBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The verified email address to send the email from. The default address is the FromAddress specified for the email channel for the application.
--
-- /Note:/ Consider using 'fromAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemFromAddress :: Lens.Lens' CampaignEmailMessage (Lude.Maybe Lude.Text)
cemFromAddress = Lens.lens (fromAddress :: CampaignEmailMessage -> Lude.Maybe Lude.Text) (\s a -> s {fromAddress = a} :: CampaignEmailMessage)
{-# DEPRECATED cemFromAddress "Use generic-lens or generic-optics with 'fromAddress' instead." #-}

-- | The body of the email, in HTML format, for recipients whose email clients render HTML content.
--
-- /Note:/ Consider using 'htmlBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemHTMLBody :: Lens.Lens' CampaignEmailMessage (Lude.Maybe Lude.Text)
cemHTMLBody = Lens.lens (htmlBody :: CampaignEmailMessage -> Lude.Maybe Lude.Text) (\s a -> s {htmlBody = a} :: CampaignEmailMessage)
{-# DEPRECATED cemHTMLBody "Use generic-lens or generic-optics with 'htmlBody' instead." #-}

-- | The subject line, or title, of the email.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemTitle :: Lens.Lens' CampaignEmailMessage (Lude.Maybe Lude.Text)
cemTitle = Lens.lens (title :: CampaignEmailMessage -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: CampaignEmailMessage)
{-# DEPRECATED cemTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Lude.FromJSON CampaignEmailMessage where
  parseJSON =
    Lude.withObject
      "CampaignEmailMessage"
      ( \x ->
          CampaignEmailMessage'
            Lude.<$> (x Lude..:? "Body")
            Lude.<*> (x Lude..:? "FromAddress")
            Lude.<*> (x Lude..:? "HtmlBody")
            Lude.<*> (x Lude..:? "Title")
      )

instance Lude.ToJSON CampaignEmailMessage where
  toJSON CampaignEmailMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Body" Lude..=) Lude.<$> body,
            ("FromAddress" Lude..=) Lude.<$> fromAddress,
            ("HtmlBody" Lude..=) Lude.<$> htmlBody,
            ("Title" Lude..=) Lude.<$> title
          ]
      )
