-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceTemplateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceTemplateRequest
  ( VoiceTemplateRequest (..),

    -- * Smart constructor
    mkVoiceTemplateRequest,

    -- * Lenses
    vtrLanguageCode,
    vtrBody,
    vtrTemplateDescription,
    vtrDefaultSubstitutions,
    vtrVoiceId,
    vtrTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the content and settings for a message template that can be used in messages that are sent through the voice channel.
--
-- /See:/ 'mkVoiceTemplateRequest' smart constructor.
data VoiceTemplateRequest = VoiceTemplateRequest'
  { languageCode ::
      Lude.Maybe Lude.Text,
    body :: Lude.Maybe Lude.Text,
    templateDescription :: Lude.Maybe Lude.Text,
    defaultSubstitutions :: Lude.Maybe Lude.Text,
    voiceId :: Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VoiceTemplateRequest' with the minimum fields required to make a request.
--
-- * 'body' - The text of the script to use in messages that are based on the message template, in plain text format.
-- * 'defaultSubstitutions' - A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
-- * 'languageCode' - The code for the language to use when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
-- * 'tags' - A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
-- * 'templateDescription' - A custom description of the message template.
-- * 'voiceId' - The name of the voice to use when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
mkVoiceTemplateRequest ::
  VoiceTemplateRequest
mkVoiceTemplateRequest =
  VoiceTemplateRequest'
    { languageCode = Lude.Nothing,
      body = Lude.Nothing,
      templateDescription = Lude.Nothing,
      defaultSubstitutions = Lude.Nothing,
      voiceId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The code for the language to use when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrLanguageCode :: Lens.Lens' VoiceTemplateRequest (Lude.Maybe Lude.Text)
vtrLanguageCode = Lens.lens (languageCode :: VoiceTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {languageCode = a} :: VoiceTemplateRequest)
{-# DEPRECATED vtrLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The text of the script to use in messages that are based on the message template, in plain text format.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrBody :: Lens.Lens' VoiceTemplateRequest (Lude.Maybe Lude.Text)
vtrBody = Lens.lens (body :: VoiceTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: VoiceTemplateRequest)
{-# DEPRECATED vtrBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | A custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrTemplateDescription :: Lens.Lens' VoiceTemplateRequest (Lude.Maybe Lude.Text)
vtrTemplateDescription = Lens.lens (templateDescription :: VoiceTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: VoiceTemplateRequest)
{-# DEPRECATED vtrTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrDefaultSubstitutions :: Lens.Lens' VoiceTemplateRequest (Lude.Maybe Lude.Text)
vtrDefaultSubstitutions = Lens.lens (defaultSubstitutions :: VoiceTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubstitutions = a} :: VoiceTemplateRequest)
{-# DEPRECATED vtrDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The name of the voice to use when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrVoiceId :: Lens.Lens' VoiceTemplateRequest (Lude.Maybe Lude.Text)
vtrVoiceId = Lens.lens (voiceId :: VoiceTemplateRequest -> Lude.Maybe Lude.Text) (\s a -> s {voiceId = a} :: VoiceTemplateRequest)
{-# DEPRECATED vtrVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrTags :: Lens.Lens' VoiceTemplateRequest (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
vtrTags = Lens.lens (tags :: VoiceTemplateRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: VoiceTemplateRequest)
{-# DEPRECATED vtrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON VoiceTemplateRequest where
  toJSON VoiceTemplateRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LanguageCode" Lude..=) Lude.<$> languageCode,
            ("Body" Lude..=) Lude.<$> body,
            ("TemplateDescription" Lude..=) Lude.<$> templateDescription,
            ("DefaultSubstitutions" Lude..=) Lude.<$> defaultSubstitutions,
            ("VoiceId" Lude..=) Lude.<$> voiceId,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )
