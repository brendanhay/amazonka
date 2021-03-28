{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailTemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EmailTemplateResponse
  ( EmailTemplateResponse (..)
  -- * Smart constructor
  , mkEmailTemplateResponse
  -- * Lenses
  , eLastModifiedDate
  , eCreationDate
  , eTemplateName
  , eTemplateType
  , eArn
  , eDefaultSubstitutions
  , eHtmlPart
  , eRecommenderId
  , eSubject
  , eTemplateDescription
  , eTextPart
  , eVersion
  , eTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.TemplateType as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the content and settings for a message template that can be used in messages that are sent through the email channel.
--
-- /See:/ 'mkEmailTemplateResponse' smart constructor.
data EmailTemplateResponse = EmailTemplateResponse'
  { lastModifiedDate :: Core.Text
    -- ^ The date, in ISO 8601 format, when the message template was last modified.
  , creationDate :: Core.Text
    -- ^ The date, in ISO 8601 format, when the message template was created.
  , templateName :: Core.Text
    -- ^ The name of the message template.
  , templateType :: Types.TemplateType
    -- ^ The type of channel that the message template is designed for. For an email template, this value is EMAIL.
  , arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the message template.
  , defaultSubstitutions :: Core.Maybe Core.Text
    -- ^ The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
  , htmlPart :: Core.Maybe Core.Text
    -- ^ The message body, in HTML format, that's used in email messages that are based on the message template.
  , recommenderId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the recommender model that's used by the message template.
  , subject :: Core.Maybe Core.Text
    -- ^ The subject line, or title, that's used in email messages that are based on the message template.
  , templateDescription :: Core.Maybe Core.Text
    -- ^ The custom description of the message template.
  , textPart :: Core.Maybe Core.Text
    -- ^ The message body, in plain text format, that's used in email messages that are based on the message template.
  , version :: Core.Maybe Core.Text
    -- ^ The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmailTemplateResponse' value with any optional fields omitted.
mkEmailTemplateResponse
    :: Core.Text -- ^ 'lastModifiedDate'
    -> Core.Text -- ^ 'creationDate'
    -> Core.Text -- ^ 'templateName'
    -> Types.TemplateType -- ^ 'templateType'
    -> EmailTemplateResponse
mkEmailTemplateResponse lastModifiedDate creationDate templateName
  templateType
  = EmailTemplateResponse'{lastModifiedDate, creationDate,
                           templateName, templateType, arn = Core.Nothing,
                           defaultSubstitutions = Core.Nothing, htmlPart = Core.Nothing,
                           recommenderId = Core.Nothing, subject = Core.Nothing,
                           templateDescription = Core.Nothing, textPart = Core.Nothing,
                           version = Core.Nothing, tags = Core.Nothing}

-- | The date, in ISO 8601 format, when the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastModifiedDate :: Lens.Lens' EmailTemplateResponse Core.Text
eLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE eLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The date, in ISO 8601 format, when the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCreationDate :: Lens.Lens' EmailTemplateResponse Core.Text
eCreationDate = Lens.field @"creationDate"
{-# INLINEABLE eCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTemplateName :: Lens.Lens' EmailTemplateResponse Core.Text
eTemplateName = Lens.field @"templateName"
{-# INLINEABLE eTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The type of channel that the message template is designed for. For an email template, this value is EMAIL.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTemplateType :: Lens.Lens' EmailTemplateResponse Types.TemplateType
eTemplateType = Lens.field @"templateType"
{-# INLINEABLE eTemplateType #-}
{-# DEPRECATED templateType "Use generic-lens or generic-optics with 'templateType' instead"  #-}

-- | The Amazon Resource Name (ARN) of the message template.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eArn :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
eArn = Lens.field @"arn"
{-# INLINEABLE eArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDefaultSubstitutions :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
eDefaultSubstitutions = Lens.field @"defaultSubstitutions"
{-# INLINEABLE eDefaultSubstitutions #-}
{-# DEPRECATED defaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead"  #-}

-- | The message body, in HTML format, that's used in email messages that are based on the message template.
--
-- /Note:/ Consider using 'htmlPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eHtmlPart :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
eHtmlPart = Lens.field @"htmlPart"
{-# INLINEABLE eHtmlPart #-}
{-# DEPRECATED htmlPart "Use generic-lens or generic-optics with 'htmlPart' instead"  #-}

-- | The unique identifier for the recommender model that's used by the message template.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRecommenderId :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
eRecommenderId = Lens.field @"recommenderId"
{-# INLINEABLE eRecommenderId #-}
{-# DEPRECATED recommenderId "Use generic-lens or generic-optics with 'recommenderId' instead"  #-}

-- | The subject line, or title, that's used in email messages that are based on the message template.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSubject :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
eSubject = Lens.field @"subject"
{-# INLINEABLE eSubject #-}
{-# DEPRECATED subject "Use generic-lens or generic-optics with 'subject' instead"  #-}

-- | The custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTemplateDescription :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
eTemplateDescription = Lens.field @"templateDescription"
{-# INLINEABLE eTemplateDescription #-}
{-# DEPRECATED templateDescription "Use generic-lens or generic-optics with 'templateDescription' instead"  #-}

-- | The message body, in plain text format, that's used in email messages that are based on the message template.
--
-- /Note:/ Consider using 'textPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTextPart :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
eTextPart = Lens.field @"textPart"
{-# INLINEABLE eTextPart #-}
{-# DEPRECATED textPart "Use generic-lens or generic-optics with 'textPart' instead"  #-}

-- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eVersion :: Lens.Lens' EmailTemplateResponse (Core.Maybe Core.Text)
eVersion = Lens.field @"version"
{-# INLINEABLE eVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTags :: Lens.Lens' EmailTemplateResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
eTags = Lens.field @"tags"
{-# INLINEABLE eTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON EmailTemplateResponse where
        parseJSON
          = Core.withObject "EmailTemplateResponse" Core.$
              \ x ->
                EmailTemplateResponse' Core.<$>
                  (x Core..: "LastModifiedDate") Core.<*> x Core..: "CreationDate"
                    Core.<*> x Core..: "TemplateName"
                    Core.<*> x Core..: "TemplateType"
                    Core.<*> x Core..:? "Arn"
                    Core.<*> x Core..:? "DefaultSubstitutions"
                    Core.<*> x Core..:? "HtmlPart"
                    Core.<*> x Core..:? "RecommenderId"
                    Core.<*> x Core..:? "Subject"
                    Core.<*> x Core..:? "TemplateDescription"
                    Core.<*> x Core..:? "TextPart"
                    Core.<*> x Core..:? "Version"
                    Core.<*> x Core..:? "tags"
