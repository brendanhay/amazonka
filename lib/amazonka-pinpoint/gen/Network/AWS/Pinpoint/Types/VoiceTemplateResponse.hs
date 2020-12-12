{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceTemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceTemplateResponse
  ( VoiceTemplateResponse (..),

    -- * Smart constructor
    mkVoiceTemplateResponse,

    -- * Lenses
    vtLanguageCode,
    vtARN,
    vtBody,
    vtTemplateDescription,
    vtDefaultSubstitutions,
    vtVersion,
    vtVoiceId,
    vtTags,
    vtLastModifiedDate,
    vtCreationDate,
    vtTemplateName,
    vtTemplateType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateType
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the content and settings for a message template that can be used in messages that are sent through the voice channel.
--
-- /See:/ 'mkVoiceTemplateResponse' smart constructor.
data VoiceTemplateResponse = VoiceTemplateResponse'
  { languageCode ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    body :: Lude.Maybe Lude.Text,
    templateDescription :: Lude.Maybe Lude.Text,
    defaultSubstitutions :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    voiceId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'VoiceTemplateResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the message template.
-- * 'body' - The text of the script that's used in messages that are based on the message template, in plain text format.
-- * 'creationDate' - The date, in ISO 8601 format, when the message template was created.
-- * 'defaultSubstitutions' - The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
-- * 'languageCode' - The code for the language that's used when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
-- * 'lastModifiedDate' - The date, in ISO 8601 format, when the message template was last modified.
-- * 'tags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
-- * 'templateDescription' - The custom description of the message template.
-- * 'templateName' - The name of the message template.
-- * 'templateType' - The type of channel that the message template is designed for. For a voice template, this value is VOICE.
-- * 'version' - The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
-- * 'voiceId' - The name of the voice that's used when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
mkVoiceTemplateResponse ::
  -- | 'lastModifiedDate'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Text ->
  -- | 'templateName'
  Lude.Text ->
  -- | 'templateType'
  TemplateType ->
  VoiceTemplateResponse
mkVoiceTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    VoiceTemplateResponse'
      { languageCode = Lude.Nothing,
        arn = Lude.Nothing,
        body = Lude.Nothing,
        templateDescription = Lude.Nothing,
        defaultSubstitutions = Lude.Nothing,
        version = Lude.Nothing,
        voiceId = Lude.Nothing,
        tags = Lude.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | The code for the language that's used when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtLanguageCode :: Lens.Lens' VoiceTemplateResponse (Lude.Maybe Lude.Text)
vtLanguageCode = Lens.lens (languageCode :: VoiceTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {languageCode = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The Amazon Resource Name (ARN) of the message template.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtARN :: Lens.Lens' VoiceTemplateResponse (Lude.Maybe Lude.Text)
vtARN = Lens.lens (arn :: VoiceTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The text of the script that's used in messages that are based on the message template, in plain text format.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtBody :: Lens.Lens' VoiceTemplateResponse (Lude.Maybe Lude.Text)
vtBody = Lens.lens (body :: VoiceTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTemplateDescription :: Lens.Lens' VoiceTemplateResponse (Lude.Maybe Lude.Text)
vtTemplateDescription = Lens.lens (templateDescription :: VoiceTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtDefaultSubstitutions :: Lens.Lens' VoiceTemplateResponse (Lude.Maybe Lude.Text)
vtDefaultSubstitutions = Lens.lens (defaultSubstitutions :: VoiceTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubstitutions = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtVersion :: Lens.Lens' VoiceTemplateResponse (Lude.Maybe Lude.Text)
vtVersion = Lens.lens (version :: VoiceTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the voice that's used when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtVoiceId :: Lens.Lens' VoiceTemplateResponse (Lude.Maybe Lude.Text)
vtVoiceId = Lens.lens (voiceId :: VoiceTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {voiceId = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTags :: Lens.Lens' VoiceTemplateResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
vtTags = Lens.lens (tags :: VoiceTemplateResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The date, in ISO 8601 format, when the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtLastModifiedDate :: Lens.Lens' VoiceTemplateResponse Lude.Text
vtLastModifiedDate = Lens.lens (lastModifiedDate :: VoiceTemplateResponse -> Lude.Text) (\s a -> s {lastModifiedDate = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The date, in ISO 8601 format, when the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtCreationDate :: Lens.Lens' VoiceTemplateResponse Lude.Text
vtCreationDate = Lens.lens (creationDate :: VoiceTemplateResponse -> Lude.Text) (\s a -> s {creationDate = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTemplateName :: Lens.Lens' VoiceTemplateResponse Lude.Text
vtTemplateName = Lens.lens (templateName :: VoiceTemplateResponse -> Lude.Text) (\s a -> s {templateName = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The type of channel that the message template is designed for. For a voice template, this value is VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTemplateType :: Lens.Lens' VoiceTemplateResponse TemplateType
vtTemplateType = Lens.lens (templateType :: VoiceTemplateResponse -> TemplateType) (\s a -> s {templateType = a} :: VoiceTemplateResponse)
{-# DEPRECATED vtTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

instance Lude.FromJSON VoiceTemplateResponse where
  parseJSON =
    Lude.withObject
      "VoiceTemplateResponse"
      ( \x ->
          VoiceTemplateResponse'
            Lude.<$> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Body")
            Lude.<*> (x Lude..:? "TemplateDescription")
            Lude.<*> (x Lude..:? "DefaultSubstitutions")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "VoiceId")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "LastModifiedDate")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..: "TemplateName")
            Lude.<*> (x Lude..: "TemplateType")
      )
