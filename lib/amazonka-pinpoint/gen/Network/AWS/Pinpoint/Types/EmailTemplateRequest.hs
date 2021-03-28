{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailTemplateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EmailTemplateRequest
  ( EmailTemplateRequest (..)
  -- * Smart constructor
  , mkEmailTemplateRequest
  -- * Lenses
  , etrDefaultSubstitutions
  , etrHtmlPart
  , etrRecommenderId
  , etrSubject
  , etrTemplateDescription
  , etrTextPart
  , etrTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the content and settings for a message template that can be used in messages that are sent through the email channel.
--
-- /See:/ 'mkEmailTemplateRequest' smart constructor.
data EmailTemplateRequest = EmailTemplateRequest'
  { defaultSubstitutions :: Core.Maybe Core.Text
    -- ^ A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
  , htmlPart :: Core.Maybe Core.Text
    -- ^ The message body, in HTML format, to use in email messages that are based on the message template. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
  , recommenderId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
  , subject :: Core.Maybe Core.Text
    -- ^ The subject line, or title, to use in email messages that are based on the message template.
  , templateDescription :: Core.Maybe Core.Text
    -- ^ A custom description of the message template.
  , textPart :: Core.Maybe Core.Text
    -- ^ The message body, in plain text format, to use in email messages that are based on the message template. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmailTemplateRequest' value with any optional fields omitted.
mkEmailTemplateRequest
    :: EmailTemplateRequest
mkEmailTemplateRequest
  = EmailTemplateRequest'{defaultSubstitutions = Core.Nothing,
                          htmlPart = Core.Nothing, recommenderId = Core.Nothing,
                          subject = Core.Nothing, templateDescription = Core.Nothing,
                          textPart = Core.Nothing, tags = Core.Nothing}

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrDefaultSubstitutions :: Lens.Lens' EmailTemplateRequest (Core.Maybe Core.Text)
etrDefaultSubstitutions = Lens.field @"defaultSubstitutions"
{-# INLINEABLE etrDefaultSubstitutions #-}
{-# DEPRECATED defaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead"  #-}

-- | The message body, in HTML format, to use in email messages that are based on the message template. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
--
-- /Note:/ Consider using 'htmlPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrHtmlPart :: Lens.Lens' EmailTemplateRequest (Core.Maybe Core.Text)
etrHtmlPart = Lens.field @"htmlPart"
{-# INLINEABLE etrHtmlPart #-}
{-# DEPRECATED htmlPart "Use generic-lens or generic-optics with 'htmlPart' instead"  #-}

-- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrRecommenderId :: Lens.Lens' EmailTemplateRequest (Core.Maybe Core.Text)
etrRecommenderId = Lens.field @"recommenderId"
{-# INLINEABLE etrRecommenderId #-}
{-# DEPRECATED recommenderId "Use generic-lens or generic-optics with 'recommenderId' instead"  #-}

-- | The subject line, or title, to use in email messages that are based on the message template.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrSubject :: Lens.Lens' EmailTemplateRequest (Core.Maybe Core.Text)
etrSubject = Lens.field @"subject"
{-# INLINEABLE etrSubject #-}
{-# DEPRECATED subject "Use generic-lens or generic-optics with 'subject' instead"  #-}

-- | A custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrTemplateDescription :: Lens.Lens' EmailTemplateRequest (Core.Maybe Core.Text)
etrTemplateDescription = Lens.field @"templateDescription"
{-# INLINEABLE etrTemplateDescription #-}
{-# DEPRECATED templateDescription "Use generic-lens or generic-optics with 'templateDescription' instead"  #-}

-- | The message body, in plain text format, to use in email messages that are based on the message template. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
--
-- /Note:/ Consider using 'textPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrTextPart :: Lens.Lens' EmailTemplateRequest (Core.Maybe Core.Text)
etrTextPart = Lens.field @"textPart"
{-# INLINEABLE etrTextPart #-}
{-# DEPRECATED textPart "Use generic-lens or generic-optics with 'textPart' instead"  #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrTags :: Lens.Lens' EmailTemplateRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
etrTags = Lens.field @"tags"
{-# INLINEABLE etrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON EmailTemplateRequest where
        toJSON EmailTemplateRequest{..}
          = Core.object
              (Core.catMaybes
                 [("DefaultSubstitutions" Core..=) Core.<$> defaultSubstitutions,
                  ("HtmlPart" Core..=) Core.<$> htmlPart,
                  ("RecommenderId" Core..=) Core.<$> recommenderId,
                  ("Subject" Core..=) Core.<$> subject,
                  ("TemplateDescription" Core..=) Core.<$> templateDescription,
                  ("TextPart" Core..=) Core.<$> textPart,
                  ("tags" Core..=) Core.<$> tags])
