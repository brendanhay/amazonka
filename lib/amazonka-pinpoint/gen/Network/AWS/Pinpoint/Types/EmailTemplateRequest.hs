{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailTemplateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailTemplateRequest
  ( EmailTemplateRequest (..),

    -- * Smart constructor
    mkEmailTemplateRequest,

    -- * Lenses
    etrSubject,
    etrTextPart,
    etrTemplateDescription,
    etrDefaultSubstitutions,
    etrHTMLPart,
    etrRecommenderId,
    etrTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the content and settings for a message template that can be used in messages that are sent through the email channel.
--
-- /See:/ 'mkEmailTemplateRequest' smart constructor.
data EmailTemplateRequest = EmailTemplateRequest'
  { -- | The subject line, or title, to use in email messages that are based on the message template.
    subject :: Lude.Maybe Lude.Text,
    -- | The message body, in plain text format, to use in email messages that are based on the message template. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
    textPart :: Lude.Maybe Lude.Text,
    -- | A custom description of the message template.
    templateDescription :: Lude.Maybe Lude.Text,
    -- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
    defaultSubstitutions :: Lude.Maybe Lude.Text,
    -- | The message body, in HTML format, to use in email messages that are based on the message template. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
    htmlPart :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
    recommenderId :: Lude.Maybe Lude.Text,
    -- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmailTemplateRequest' with the minimum fields required to make a request.
--
-- * 'subject' - The subject line, or title, to use in email messages that are based on the message template.
-- * 'textPart' - The message body, in plain text format, to use in email messages that are based on the message template. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
-- * 'templateDescription' - A custom description of the message template.
-- * 'defaultSubstitutions' - A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
-- * 'htmlPart' - The message body, in HTML format, to use in email messages that are based on the message template. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
-- * 'recommenderId' - The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
-- * 'tags' - A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
mkEmailTemplateRequest ::
  EmailTemplateRequest
mkEmailTemplateRequest =
  EmailTemplateRequest'
    { subject = Lude.Nothing,
      textPart = Lude.Nothing,
      templateDescription = Lude.Nothing,
      defaultSubstitutions = Lude.Nothing,
      htmlPart = Lude.Nothing,
      recommenderId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The subject line, or title, to use in email messages that are based on the message template.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrSubject :: Lens.Lens' EmailTemplateRequest (Lude.Maybe Lude.Text)
etrSubject = Lens.lens (subject :: EmailTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {subject = a} :: EmailTemplateRequest)
{-# DEPRECATED etrSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The message body, in plain text format, to use in email messages that are based on the message template. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
--
-- /Note:/ Consider using 'textPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrTextPart :: Lens.Lens' EmailTemplateRequest (Lude.Maybe Lude.Text)
etrTextPart = Lens.lens (textPart :: EmailTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {textPart = a} :: EmailTemplateRequest)
{-# DEPRECATED etrTextPart "Use generic-lens or generic-optics with 'textPart' instead." #-}

-- | A custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrTemplateDescription :: Lens.Lens' EmailTemplateRequest (Lude.Maybe Lude.Text)
etrTemplateDescription = Lens.lens (templateDescription :: EmailTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: EmailTemplateRequest)
{-# DEPRECATED etrTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrDefaultSubstitutions :: Lens.Lens' EmailTemplateRequest (Lude.Maybe Lude.Text)
etrDefaultSubstitutions = Lens.lens (defaultSubstitutions :: EmailTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubstitutions = a} :: EmailTemplateRequest)
{-# DEPRECATED etrDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The message body, in HTML format, to use in email messages that are based on the message template. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
--
-- /Note:/ Consider using 'htmlPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrHTMLPart :: Lens.Lens' EmailTemplateRequest (Lude.Maybe Lude.Text)
etrHTMLPart = Lens.lens (htmlPart :: EmailTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {htmlPart = a} :: EmailTemplateRequest)
{-# DEPRECATED etrHTMLPart "Use generic-lens or generic-optics with 'htmlPart' instead." #-}

-- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrRecommenderId :: Lens.Lens' EmailTemplateRequest (Lude.Maybe Lude.Text)
etrRecommenderId = Lens.lens (recommenderId :: EmailTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {recommenderId = a} :: EmailTemplateRequest)
{-# DEPRECATED etrRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrTags :: Lens.Lens' EmailTemplateRequest (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
etrTags = Lens.lens (tags :: EmailTemplateRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: EmailTemplateRequest)
{-# DEPRECATED etrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON EmailTemplateRequest where
  toJSON EmailTemplateRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Subject" Lude..=) Lude.<$> subject,
            ("TextPart" Lude..=) Lude.<$> textPart,
            ("TemplateDescription" Lude..=) Lude.<$> templateDescription,
            ("DefaultSubstitutions" Lude..=) Lude.<$> defaultSubstitutions,
            ("HtmlPart" Lude..=) Lude.<$> htmlPart,
            ("RecommenderId" Lude..=) Lude.<$> recommenderId,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )
