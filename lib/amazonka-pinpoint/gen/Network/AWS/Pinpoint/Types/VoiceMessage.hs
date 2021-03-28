{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.VoiceMessage
  ( VoiceMessage (..)
  -- * Smart constructor
  , mkVoiceMessage
  -- * Lenses
  , vmBody
  , vmLanguageCode
  , vmOriginationNumber
  , vmSubstitutions
  , vmVoiceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a one-time voice message that's sent directly to an endpoint through the voice channel.
--
-- /See:/ 'mkVoiceMessage' smart constructor.
data VoiceMessage = VoiceMessage'
  { body :: Core.Maybe Core.Text
    -- ^ The text of the script to use for the voice message.
  , languageCode :: Core.Maybe Core.Text
    -- ^ The code for the language to use when synthesizing the text of the message script. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
  , originationNumber :: Core.Maybe Core.Text
    -- ^ The long code to send the voice message from. This value should be one of the dedicated long codes that's assigned to your AWS account. Although it isn't required, we recommend that you specify the long code in E.164 format, for example +12065550100, to ensure prompt and accurate delivery of the message.
  , substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ The default message variables to use in the voice message. You can override the default variables with individual address variables.
  , voiceId :: Core.Maybe Core.Text
    -- ^ The name of the voice to use when delivering the message. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VoiceMessage' value with any optional fields omitted.
mkVoiceMessage
    :: VoiceMessage
mkVoiceMessage
  = VoiceMessage'{body = Core.Nothing, languageCode = Core.Nothing,
                  originationNumber = Core.Nothing, substitutions = Core.Nothing,
                  voiceId = Core.Nothing}

-- | The text of the script to use for the voice message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmBody :: Lens.Lens' VoiceMessage (Core.Maybe Core.Text)
vmBody = Lens.field @"body"
{-# INLINEABLE vmBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The code for the language to use when synthesizing the text of the message script. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmLanguageCode :: Lens.Lens' VoiceMessage (Core.Maybe Core.Text)
vmLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE vmLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | The long code to send the voice message from. This value should be one of the dedicated long codes that's assigned to your AWS account. Although it isn't required, we recommend that you specify the long code in E.164 format, for example +12065550100, to ensure prompt and accurate delivery of the message.
--
-- /Note:/ Consider using 'originationNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmOriginationNumber :: Lens.Lens' VoiceMessage (Core.Maybe Core.Text)
vmOriginationNumber = Lens.field @"originationNumber"
{-# INLINEABLE vmOriginationNumber #-}
{-# DEPRECATED originationNumber "Use generic-lens or generic-optics with 'originationNumber' instead"  #-}

-- | The default message variables to use in the voice message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmSubstitutions :: Lens.Lens' VoiceMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
vmSubstitutions = Lens.field @"substitutions"
{-# INLINEABLE vmSubstitutions #-}
{-# DEPRECATED substitutions "Use generic-lens or generic-optics with 'substitutions' instead"  #-}

-- | The name of the voice to use when delivering the message. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmVoiceId :: Lens.Lens' VoiceMessage (Core.Maybe Core.Text)
vmVoiceId = Lens.field @"voiceId"
{-# INLINEABLE vmVoiceId #-}
{-# DEPRECATED voiceId "Use generic-lens or generic-optics with 'voiceId' instead"  #-}

instance Core.FromJSON VoiceMessage where
        toJSON VoiceMessage{..}
          = Core.object
              (Core.catMaybes
                 [("Body" Core..=) Core.<$> body,
                  ("LanguageCode" Core..=) Core.<$> languageCode,
                  ("OriginationNumber" Core..=) Core.<$> originationNumber,
                  ("Substitutions" Core..=) Core.<$> substitutions,
                  ("VoiceId" Core..=) Core.<$> voiceId])
