{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSTemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSTemplateResponse
  ( SMSTemplateResponse (..),

    -- * Smart constructor
    mkSMSTemplateResponse,

    -- * Lenses
    smstTemplateName,
    smstLastModifiedDate,
    smstARN,
    smstTemplateType,
    smstBody,
    smstTemplateDescription,
    smstDefaultSubstitutions,
    smstVersion,
    smstCreationDate,
    smstRecommenderId,
    smstTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateType
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the content and settings for a message template that can be used in text messages that are sent through the SMS channel.
--
-- /See:/ 'mkSMSTemplateResponse' smart constructor.
data SMSTemplateResponse = SMSTemplateResponse'
  { -- | The name of the message template.
    templateName :: Lude.Text,
    -- | The date, in ISO 8601 format, when the message template was last modified.
    lastModifiedDate :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the message template.
    arn :: Lude.Maybe Lude.Text,
    -- | The type of channel that the message template is designed for. For an SMS template, this value is SMS.
    templateType :: TemplateType,
    -- | The message body that's used in text messages that are based on the message template.
    body :: Lude.Maybe Lude.Text,
    -- | The custom description of the message template.
    templateDescription :: Lude.Maybe Lude.Text,
    -- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
    defaultSubstitutions :: Lude.Maybe Lude.Text,
    -- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
    version :: Lude.Maybe Lude.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Lude.Text,
    -- | The unique identifier for the recommender model that's used by the message template.
    recommenderId :: Lude.Maybe Lude.Text,
    -- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SMSTemplateResponse' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the message template.
-- * 'lastModifiedDate' - The date, in ISO 8601 format, when the message template was last modified.
-- * 'arn' - The Amazon Resource Name (ARN) of the message template.
-- * 'templateType' - The type of channel that the message template is designed for. For an SMS template, this value is SMS.
-- * 'body' - The message body that's used in text messages that are based on the message template.
-- * 'templateDescription' - The custom description of the message template.
-- * 'defaultSubstitutions' - The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
-- * 'version' - The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
-- * 'creationDate' - The date, in ISO 8601 format, when the message template was created.
-- * 'recommenderId' - The unique identifier for the recommender model that's used by the message template.
-- * 'tags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
mkSMSTemplateResponse ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'lastModifiedDate'
  Lude.Text ->
  -- | 'templateType'
  TemplateType ->
  -- | 'creationDate'
  Lude.Text ->
  SMSTemplateResponse
mkSMSTemplateResponse
  pTemplateName_
  pLastModifiedDate_
  pTemplateType_
  pCreationDate_ =
    SMSTemplateResponse'
      { templateName = pTemplateName_,
        lastModifiedDate = pLastModifiedDate_,
        arn = Lude.Nothing,
        templateType = pTemplateType_,
        body = Lude.Nothing,
        templateDescription = Lude.Nothing,
        defaultSubstitutions = Lude.Nothing,
        version = Lude.Nothing,
        creationDate = pCreationDate_,
        recommenderId = Lude.Nothing,
        tags = Lude.Nothing
      }

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstTemplateName :: Lens.Lens' SMSTemplateResponse Lude.Text
smstTemplateName = Lens.lens (templateName :: SMSTemplateResponse -> Lude.Text) (\s a -> s {templateName = a} :: SMSTemplateResponse)
{-# DEPRECATED smstTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The date, in ISO 8601 format, when the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstLastModifiedDate :: Lens.Lens' SMSTemplateResponse Lude.Text
smstLastModifiedDate = Lens.lens (lastModifiedDate :: SMSTemplateResponse -> Lude.Text) (\s a -> s {lastModifiedDate = a} :: SMSTemplateResponse)
{-# DEPRECATED smstLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the message template.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstARN :: Lens.Lens' SMSTemplateResponse (Lude.Maybe Lude.Text)
smstARN = Lens.lens (arn :: SMSTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: SMSTemplateResponse)
{-# DEPRECATED smstARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The type of channel that the message template is designed for. For an SMS template, this value is SMS.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstTemplateType :: Lens.Lens' SMSTemplateResponse TemplateType
smstTemplateType = Lens.lens (templateType :: SMSTemplateResponse -> TemplateType) (\s a -> s {templateType = a} :: SMSTemplateResponse)
{-# DEPRECATED smstTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

-- | The message body that's used in text messages that are based on the message template.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstBody :: Lens.Lens' SMSTemplateResponse (Lude.Maybe Lude.Text)
smstBody = Lens.lens (body :: SMSTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: SMSTemplateResponse)
{-# DEPRECATED smstBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstTemplateDescription :: Lens.Lens' SMSTemplateResponse (Lude.Maybe Lude.Text)
smstTemplateDescription = Lens.lens (templateDescription :: SMSTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: SMSTemplateResponse)
{-# DEPRECATED smstTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstDefaultSubstitutions :: Lens.Lens' SMSTemplateResponse (Lude.Maybe Lude.Text)
smstDefaultSubstitutions = Lens.lens (defaultSubstitutions :: SMSTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubstitutions = a} :: SMSTemplateResponse)
{-# DEPRECATED smstDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstVersion :: Lens.Lens' SMSTemplateResponse (Lude.Maybe Lude.Text)
smstVersion = Lens.lens (version :: SMSTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: SMSTemplateResponse)
{-# DEPRECATED smstVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date, in ISO 8601 format, when the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstCreationDate :: Lens.Lens' SMSTemplateResponse Lude.Text
smstCreationDate = Lens.lens (creationDate :: SMSTemplateResponse -> Lude.Text) (\s a -> s {creationDate = a} :: SMSTemplateResponse)
{-# DEPRECATED smstCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The unique identifier for the recommender model that's used by the message template.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstRecommenderId :: Lens.Lens' SMSTemplateResponse (Lude.Maybe Lude.Text)
smstRecommenderId = Lens.lens (recommenderId :: SMSTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {recommenderId = a} :: SMSTemplateResponse)
{-# DEPRECATED smstRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smstTags :: Lens.Lens' SMSTemplateResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
smstTags = Lens.lens (tags :: SMSTemplateResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: SMSTemplateResponse)
{-# DEPRECATED smstTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON SMSTemplateResponse where
  parseJSON =
    Lude.withObject
      "SMSTemplateResponse"
      ( \x ->
          SMSTemplateResponse'
            Lude.<$> (x Lude..: "TemplateName")
            Lude.<*> (x Lude..: "LastModifiedDate")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..: "TemplateType")
            Lude.<*> (x Lude..:? "Body")
            Lude.<*> (x Lude..:? "TemplateDescription")
            Lude.<*> (x Lude..:? "DefaultSubstitutions")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..:? "RecommenderId")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
