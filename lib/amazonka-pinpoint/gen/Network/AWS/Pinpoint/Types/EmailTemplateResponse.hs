{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailTemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailTemplateResponse
  ( EmailTemplateResponse (..),

    -- * Smart constructor
    mkEmailTemplateResponse,

    -- * Lenses
    etSubject,
    etTextPart,
    etARN,
    etTemplateDescription,
    etDefaultSubstitutions,
    etVersion,
    etHTMLPart,
    etRecommenderId,
    etTags,
    etLastModifiedDate,
    etCreationDate,
    etTemplateName,
    etTemplateType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateType
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the content and settings for a message template that can be used in messages that are sent through the email channel.
--
-- /See:/ 'mkEmailTemplateResponse' smart constructor.
data EmailTemplateResponse = EmailTemplateResponse'
  { subject ::
      Lude.Maybe Lude.Text,
    textPart :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    templateDescription :: Lude.Maybe Lude.Text,
    defaultSubstitutions :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    htmlPart :: Lude.Maybe Lude.Text,
    recommenderId :: Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    lastModifiedDate :: Lude.Text,
    creationDate :: Lude.Text,
    templateName :: Lude.Text,
    templateType :: TemplateType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmailTemplateResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the message template.
-- * 'creationDate' - The date, in ISO 8601 format, when the message template was created.
-- * 'defaultSubstitutions' - The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
-- * 'htmlPart' - The message body, in HTML format, that's used in email messages that are based on the message template.
-- * 'lastModifiedDate' - The date, in ISO 8601 format, when the message template was last modified.
-- * 'recommenderId' - The unique identifier for the recommender model that's used by the message template.
-- * 'subject' - The subject line, or title, that's used in email messages that are based on the message template.
-- * 'tags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
-- * 'templateDescription' - The custom description of the message template.
-- * 'templateName' - The name of the message template.
-- * 'templateType' - The type of channel that the message template is designed for. For an email template, this value is EMAIL.
-- * 'textPart' - The message body, in plain text format, that's used in email messages that are based on the message template.
-- * 'version' - The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
mkEmailTemplateResponse ::
  -- | 'lastModifiedDate'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Text ->
  -- | 'templateName'
  Lude.Text ->
  -- | 'templateType'
  TemplateType ->
  EmailTemplateResponse
mkEmailTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    EmailTemplateResponse'
      { subject = Lude.Nothing,
        textPart = Lude.Nothing,
        arn = Lude.Nothing,
        templateDescription = Lude.Nothing,
        defaultSubstitutions = Lude.Nothing,
        version = Lude.Nothing,
        htmlPart = Lude.Nothing,
        recommenderId = Lude.Nothing,
        tags = Lude.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | The subject line, or title, that's used in email messages that are based on the message template.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etSubject :: Lens.Lens' EmailTemplateResponse (Lude.Maybe Lude.Text)
etSubject = Lens.lens (subject :: EmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {subject = a} :: EmailTemplateResponse)
{-# DEPRECATED etSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The message body, in plain text format, that's used in email messages that are based on the message template.
--
-- /Note:/ Consider using 'textPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTextPart :: Lens.Lens' EmailTemplateResponse (Lude.Maybe Lude.Text)
etTextPart = Lens.lens (textPart :: EmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {textPart = a} :: EmailTemplateResponse)
{-# DEPRECATED etTextPart "Use generic-lens or generic-optics with 'textPart' instead." #-}

-- | The Amazon Resource Name (ARN) of the message template.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etARN :: Lens.Lens' EmailTemplateResponse (Lude.Maybe Lude.Text)
etARN = Lens.lens (arn :: EmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: EmailTemplateResponse)
{-# DEPRECATED etARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTemplateDescription :: Lens.Lens' EmailTemplateResponse (Lude.Maybe Lude.Text)
etTemplateDescription = Lens.lens (templateDescription :: EmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: EmailTemplateResponse)
{-# DEPRECATED etTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etDefaultSubstitutions :: Lens.Lens' EmailTemplateResponse (Lude.Maybe Lude.Text)
etDefaultSubstitutions = Lens.lens (defaultSubstitutions :: EmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubstitutions = a} :: EmailTemplateResponse)
{-# DEPRECATED etDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etVersion :: Lens.Lens' EmailTemplateResponse (Lude.Maybe Lude.Text)
etVersion = Lens.lens (version :: EmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: EmailTemplateResponse)
{-# DEPRECATED etVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The message body, in HTML format, that's used in email messages that are based on the message template.
--
-- /Note:/ Consider using 'htmlPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etHTMLPart :: Lens.Lens' EmailTemplateResponse (Lude.Maybe Lude.Text)
etHTMLPart = Lens.lens (htmlPart :: EmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {htmlPart = a} :: EmailTemplateResponse)
{-# DEPRECATED etHTMLPart "Use generic-lens or generic-optics with 'htmlPart' instead." #-}

-- | The unique identifier for the recommender model that's used by the message template.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etRecommenderId :: Lens.Lens' EmailTemplateResponse (Lude.Maybe Lude.Text)
etRecommenderId = Lens.lens (recommenderId :: EmailTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {recommenderId = a} :: EmailTemplateResponse)
{-# DEPRECATED etRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTags :: Lens.Lens' EmailTemplateResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
etTags = Lens.lens (tags :: EmailTemplateResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: EmailTemplateResponse)
{-# DEPRECATED etTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The date, in ISO 8601 format, when the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etLastModifiedDate :: Lens.Lens' EmailTemplateResponse Lude.Text
etLastModifiedDate = Lens.lens (lastModifiedDate :: EmailTemplateResponse -> Lude.Text) (\s a -> s {lastModifiedDate = a} :: EmailTemplateResponse)
{-# DEPRECATED etLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The date, in ISO 8601 format, when the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etCreationDate :: Lens.Lens' EmailTemplateResponse Lude.Text
etCreationDate = Lens.lens (creationDate :: EmailTemplateResponse -> Lude.Text) (\s a -> s {creationDate = a} :: EmailTemplateResponse)
{-# DEPRECATED etCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTemplateName :: Lens.Lens' EmailTemplateResponse Lude.Text
etTemplateName = Lens.lens (templateName :: EmailTemplateResponse -> Lude.Text) (\s a -> s {templateName = a} :: EmailTemplateResponse)
{-# DEPRECATED etTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The type of channel that the message template is designed for. For an email template, this value is EMAIL.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTemplateType :: Lens.Lens' EmailTemplateResponse TemplateType
etTemplateType = Lens.lens (templateType :: EmailTemplateResponse -> TemplateType) (\s a -> s {templateType = a} :: EmailTemplateResponse)
{-# DEPRECATED etTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

instance Lude.FromJSON EmailTemplateResponse where
  parseJSON =
    Lude.withObject
      "EmailTemplateResponse"
      ( \x ->
          EmailTemplateResponse'
            Lude.<$> (x Lude..:? "Subject")
            Lude.<*> (x Lude..:? "TextPart")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "TemplateDescription")
            Lude.<*> (x Lude..:? "DefaultSubstitutions")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "HtmlPart")
            Lude.<*> (x Lude..:? "RecommenderId")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "LastModifiedDate")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..: "TemplateName")
            Lude.<*> (x Lude..: "TemplateType")
      )
