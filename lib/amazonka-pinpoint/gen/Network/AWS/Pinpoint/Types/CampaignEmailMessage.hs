{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignEmailMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.CampaignEmailMessage
  ( CampaignEmailMessage (..)
  -- * Smart constructor
  , mkCampaignEmailMessage
  -- * Lenses
  , cemBody
  , cemFromAddress
  , cemHtmlBody
  , cemTitle
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the content and "From" address for an email message that's sent to recipients of a campaign.
--
-- /See:/ 'mkCampaignEmailMessage' smart constructor.
data CampaignEmailMessage = CampaignEmailMessage'
  { body :: Core.Maybe Core.Text
    -- ^ The body of the email for recipients whose email clients don't render HTML content.
  , fromAddress :: Core.Maybe Core.Text
    -- ^ The verified email address to send the email from. The default address is the FromAddress specified for the email channel for the application.
  , htmlBody :: Core.Maybe Core.Text
    -- ^ The body of the email, in HTML format, for recipients whose email clients render HTML content.
  , title :: Core.Maybe Core.Text
    -- ^ The subject line, or title, of the email.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CampaignEmailMessage' value with any optional fields omitted.
mkCampaignEmailMessage
    :: CampaignEmailMessage
mkCampaignEmailMessage
  = CampaignEmailMessage'{body = Core.Nothing,
                          fromAddress = Core.Nothing, htmlBody = Core.Nothing,
                          title = Core.Nothing}

-- | The body of the email for recipients whose email clients don't render HTML content.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemBody :: Lens.Lens' CampaignEmailMessage (Core.Maybe Core.Text)
cemBody = Lens.field @"body"
{-# INLINEABLE cemBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The verified email address to send the email from. The default address is the FromAddress specified for the email channel for the application.
--
-- /Note:/ Consider using 'fromAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemFromAddress :: Lens.Lens' CampaignEmailMessage (Core.Maybe Core.Text)
cemFromAddress = Lens.field @"fromAddress"
{-# INLINEABLE cemFromAddress #-}
{-# DEPRECATED fromAddress "Use generic-lens or generic-optics with 'fromAddress' instead"  #-}

-- | The body of the email, in HTML format, for recipients whose email clients render HTML content.
--
-- /Note:/ Consider using 'htmlBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemHtmlBody :: Lens.Lens' CampaignEmailMessage (Core.Maybe Core.Text)
cemHtmlBody = Lens.field @"htmlBody"
{-# INLINEABLE cemHtmlBody #-}
{-# DEPRECATED htmlBody "Use generic-lens or generic-optics with 'htmlBody' instead"  #-}

-- | The subject line, or title, of the email.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemTitle :: Lens.Lens' CampaignEmailMessage (Core.Maybe Core.Text)
cemTitle = Lens.field @"title"
{-# INLINEABLE cemTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

instance Core.FromJSON CampaignEmailMessage where
        toJSON CampaignEmailMessage{..}
          = Core.object
              (Core.catMaybes
                 [("Body" Core..=) Core.<$> body,
                  ("FromAddress" Core..=) Core.<$> fromAddress,
                  ("HtmlBody" Core..=) Core.<$> htmlBody,
                  ("Title" Core..=) Core.<$> title])

instance Core.FromJSON CampaignEmailMessage where
        parseJSON
          = Core.withObject "CampaignEmailMessage" Core.$
              \ x ->
                CampaignEmailMessage' Core.<$>
                  (x Core..:? "Body") Core.<*> x Core..:? "FromAddress" Core.<*>
                    x Core..:? "HtmlBody"
                    Core.<*> x Core..:? "Title"
