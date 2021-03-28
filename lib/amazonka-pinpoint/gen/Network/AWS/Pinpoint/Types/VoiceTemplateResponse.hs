{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceTemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.VoiceTemplateResponse
  ( VoiceTemplateResponse (..)
  -- * Smart constructor
  , mkVoiceTemplateResponse
  -- * Lenses
  , vLastModifiedDate
  , vCreationDate
  , vTemplateName
  , vTemplateType
  , vArn
  , vBody
  , vDefaultSubstitutions
  , vLanguageCode
  , vTemplateDescription
  , vVersion
  , vVoiceId
  , vTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.TemplateType as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the content and settings for a message template that can be used in messages that are sent through the voice channel.
--
-- /See:/ 'mkVoiceTemplateResponse' smart constructor.
data VoiceTemplateResponse = VoiceTemplateResponse'
  { lastModifiedDate :: Core.Text
    -- ^ The date, in ISO 8601 format, when the message template was last modified.
  , creationDate :: Core.Text
    -- ^ The date, in ISO 8601 format, when the message template was created.
  , templateName :: Core.Text
    -- ^ The name of the message template.
  , templateType :: Types.TemplateType
    -- ^ The type of channel that the message template is designed for. For a voice template, this value is VOICE.
  , arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the message template.
  , body :: Core.Maybe Core.Text
    -- ^ The text of the script that's used in messages that are based on the message template, in plain text format.
  , defaultSubstitutions :: Core.Maybe Core.Text
    -- ^ The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
  , languageCode :: Core.Maybe Core.Text
    -- ^ The code for the language that's used when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
  , templateDescription :: Core.Maybe Core.Text
    -- ^ The custom description of the message template.
  , version :: Core.Maybe Core.Text
    -- ^ The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
  , voiceId :: Core.Maybe Core.Text
    -- ^ The name of the voice that's used when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VoiceTemplateResponse' value with any optional fields omitted.
mkVoiceTemplateResponse
    :: Core.Text -- ^ 'lastModifiedDate'
    -> Core.Text -- ^ 'creationDate'
    -> Core.Text -- ^ 'templateName'
    -> Types.TemplateType -- ^ 'templateType'
    -> VoiceTemplateResponse
mkVoiceTemplateResponse lastModifiedDate creationDate templateName
  templateType
  = VoiceTemplateResponse'{lastModifiedDate, creationDate,
                           templateName, templateType, arn = Core.Nothing,
                           body = Core.Nothing, defaultSubstitutions = Core.Nothing,
                           languageCode = Core.Nothing, templateDescription = Core.Nothing,
                           version = Core.Nothing, voiceId = Core.Nothing,
                           tags = Core.Nothing}

-- | The date, in ISO 8601 format, when the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vLastModifiedDate :: Lens.Lens' VoiceTemplateResponse Core.Text
vLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE vLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The date, in ISO 8601 format, when the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vCreationDate :: Lens.Lens' VoiceTemplateResponse Core.Text
vCreationDate = Lens.field @"creationDate"
{-# INLINEABLE vCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vTemplateName :: Lens.Lens' VoiceTemplateResponse Core.Text
vTemplateName = Lens.field @"templateName"
{-# INLINEABLE vTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The type of channel that the message template is designed for. For a voice template, this value is VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vTemplateType :: Lens.Lens' VoiceTemplateResponse Types.TemplateType
vTemplateType = Lens.field @"templateType"
{-# INLINEABLE vTemplateType #-}
{-# DEPRECATED templateType "Use generic-lens or generic-optics with 'templateType' instead"  #-}

-- | The Amazon Resource Name (ARN) of the message template.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vArn :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
vArn = Lens.field @"arn"
{-# INLINEABLE vArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The text of the script that's used in messages that are based on the message template, in plain text format.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vBody :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
vBody = Lens.field @"body"
{-# INLINEABLE vBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vDefaultSubstitutions :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
vDefaultSubstitutions = Lens.field @"defaultSubstitutions"
{-# INLINEABLE vDefaultSubstitutions #-}
{-# DEPRECATED defaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead"  #-}

-- | The code for the language that's used when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vLanguageCode :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
vLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE vLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | The custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vTemplateDescription :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
vTemplateDescription = Lens.field @"templateDescription"
{-# INLINEABLE vTemplateDescription #-}
{-# DEPRECATED templateDescription "Use generic-lens or generic-optics with 'templateDescription' instead"  #-}

-- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vVersion :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
vVersion = Lens.field @"version"
{-# INLINEABLE vVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The name of the voice that's used when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vVoiceId :: Lens.Lens' VoiceTemplateResponse (Core.Maybe Core.Text)
vVoiceId = Lens.field @"voiceId"
{-# INLINEABLE vVoiceId #-}
{-# DEPRECATED voiceId "Use generic-lens or generic-optics with 'voiceId' instead"  #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vTags :: Lens.Lens' VoiceTemplateResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
vTags = Lens.field @"tags"
{-# INLINEABLE vTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON VoiceTemplateResponse where
        parseJSON
          = Core.withObject "VoiceTemplateResponse" Core.$
              \ x ->
                VoiceTemplateResponse' Core.<$>
                  (x Core..: "LastModifiedDate") Core.<*> x Core..: "CreationDate"
                    Core.<*> x Core..: "TemplateName"
                    Core.<*> x Core..: "TemplateType"
                    Core.<*> x Core..:? "Arn"
                    Core.<*> x Core..:? "Body"
                    Core.<*> x Core..:? "DefaultSubstitutions"
                    Core.<*> x Core..:? "LanguageCode"
                    Core.<*> x Core..:? "TemplateDescription"
                    Core.<*> x Core..:? "Version"
                    Core.<*> x Core..:? "VoiceId"
                    Core.<*> x Core..:? "tags"
