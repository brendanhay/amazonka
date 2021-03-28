{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceTemplateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.VoiceTemplateRequest
  ( VoiceTemplateRequest (..)
  -- * Smart constructor
  , mkVoiceTemplateRequest
  -- * Lenses
  , vtrBody
  , vtrDefaultSubstitutions
  , vtrLanguageCode
  , vtrTemplateDescription
  , vtrVoiceId
  , vtrTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the content and settings for a message template that can be used in messages that are sent through the voice channel.
--
-- /See:/ 'mkVoiceTemplateRequest' smart constructor.
data VoiceTemplateRequest = VoiceTemplateRequest'
  { body :: Core.Maybe Core.Text
    -- ^ The text of the script to use in messages that are based on the message template, in plain text format.
  , defaultSubstitutions :: Core.Maybe Core.Text
    -- ^ A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
  , languageCode :: Core.Maybe Core.Text
    -- ^ The code for the language to use when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
  , templateDescription :: Core.Maybe Core.Text
    -- ^ A custom description of the message template.
  , voiceId :: Core.Maybe Core.Text
    -- ^ The name of the voice to use when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VoiceTemplateRequest' value with any optional fields omitted.
mkVoiceTemplateRequest
    :: VoiceTemplateRequest
mkVoiceTemplateRequest
  = VoiceTemplateRequest'{body = Core.Nothing,
                          defaultSubstitutions = Core.Nothing, languageCode = Core.Nothing,
                          templateDescription = Core.Nothing, voiceId = Core.Nothing,
                          tags = Core.Nothing}

-- | The text of the script to use in messages that are based on the message template, in plain text format.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrBody :: Lens.Lens' VoiceTemplateRequest (Core.Maybe Core.Text)
vtrBody = Lens.field @"body"
{-# INLINEABLE vtrBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrDefaultSubstitutions :: Lens.Lens' VoiceTemplateRequest (Core.Maybe Core.Text)
vtrDefaultSubstitutions = Lens.field @"defaultSubstitutions"
{-# INLINEABLE vtrDefaultSubstitutions #-}
{-# DEPRECATED defaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead"  #-}

-- | The code for the language to use when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrLanguageCode :: Lens.Lens' VoiceTemplateRequest (Core.Maybe Core.Text)
vtrLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE vtrLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | A custom description of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrTemplateDescription :: Lens.Lens' VoiceTemplateRequest (Core.Maybe Core.Text)
vtrTemplateDescription = Lens.field @"templateDescription"
{-# INLINEABLE vtrTemplateDescription #-}
{-# DEPRECATED templateDescription "Use generic-lens or generic-optics with 'templateDescription' instead"  #-}

-- | The name of the voice to use when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrVoiceId :: Lens.Lens' VoiceTemplateRequest (Core.Maybe Core.Text)
vtrVoiceId = Lens.field @"voiceId"
{-# INLINEABLE vtrVoiceId #-}
{-# DEPRECATED voiceId "Use generic-lens or generic-optics with 'voiceId' instead"  #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrTags :: Lens.Lens' VoiceTemplateRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
vtrTags = Lens.field @"tags"
{-# INLINEABLE vtrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON VoiceTemplateRequest where
        toJSON VoiceTemplateRequest{..}
          = Core.object
              (Core.catMaybes
                 [("Body" Core..=) Core.<$> body,
                  ("DefaultSubstitutions" Core..=) Core.<$> defaultSubstitutions,
                  ("LanguageCode" Core..=) Core.<$> languageCode,
                  ("TemplateDescription" Core..=) Core.<$> templateDescription,
                  ("VoiceId" Core..=) Core.<$> voiceId,
                  ("tags" Core..=) Core.<$> tags])
