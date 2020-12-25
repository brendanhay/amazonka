{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Voice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.Voice
  ( Voice (..),

    -- * Smart constructor
    mkVoice,

    -- * Lenses
    vAdditionalLanguageCodes,
    vGender,
    vId,
    vLanguageCode,
    vLanguageName,
    vName,
    vSupportedEngines,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types.Engine as Types
import qualified Network.AWS.Polly.Types.Gender as Types
import qualified Network.AWS.Polly.Types.LanguageCode as Types
import qualified Network.AWS.Polly.Types.LanguageName as Types
import qualified Network.AWS.Polly.Types.Name as Types
import qualified Network.AWS.Polly.Types.VoiceId as Types
import qualified Network.AWS.Prelude as Core

-- | Description of the voice.
--
-- /See:/ 'mkVoice' smart constructor.
data Voice = Voice'
  { -- | Additional codes for languages available for the specified voice in addition to its default language.
    --
    -- For example, the default language for Aditi is Indian English (en-IN) because it was first used for that language. Since Aditi is bilingual and fluent in both Indian English and Hindi, this parameter would show the code @hi-IN@ .
    additionalLanguageCodes :: Core.Maybe [Types.LanguageCode],
    -- | Gender of the voice.
    gender :: Core.Maybe Types.Gender,
    -- | Amazon Polly assigned voice ID. This is the ID that you specify when calling the @SynthesizeSpeech@ operation.
    id :: Core.Maybe Types.VoiceId,
    -- | Language code of the voice.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | Human readable name of the language in English.
    languageName :: Core.Maybe Types.LanguageName,
    -- | Name of the voice (for example, Salli, Kendra, etc.). This provides a human readable voice name that you might display in your application.
    name :: Core.Maybe Types.Name,
    -- | Specifies which engines (@standard@ or @neural@ ) that are supported by a given voice.
    supportedEngines :: Core.Maybe [Types.Engine]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Voice' value with any optional fields omitted.
mkVoice ::
  Voice
mkVoice =
  Voice'
    { additionalLanguageCodes = Core.Nothing,
      gender = Core.Nothing,
      id = Core.Nothing,
      languageCode = Core.Nothing,
      languageName = Core.Nothing,
      name = Core.Nothing,
      supportedEngines = Core.Nothing
    }

-- | Additional codes for languages available for the specified voice in addition to its default language.
--
-- For example, the default language for Aditi is Indian English (en-IN) because it was first used for that language. Since Aditi is bilingual and fluent in both Indian English and Hindi, this parameter would show the code @hi-IN@ .
--
-- /Note:/ Consider using 'additionalLanguageCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vAdditionalLanguageCodes :: Lens.Lens' Voice (Core.Maybe [Types.LanguageCode])
vAdditionalLanguageCodes = Lens.field @"additionalLanguageCodes"
{-# DEPRECATED vAdditionalLanguageCodes "Use generic-lens or generic-optics with 'additionalLanguageCodes' instead." #-}

-- | Gender of the voice.
--
-- /Note:/ Consider using 'gender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vGender :: Lens.Lens' Voice (Core.Maybe Types.Gender)
vGender = Lens.field @"gender"
{-# DEPRECATED vGender "Use generic-lens or generic-optics with 'gender' instead." #-}

-- | Amazon Polly assigned voice ID. This is the ID that you specify when calling the @SynthesizeSpeech@ operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vId :: Lens.Lens' Voice (Core.Maybe Types.VoiceId)
vId = Lens.field @"id"
{-# DEPRECATED vId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Language code of the voice.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vLanguageCode :: Lens.Lens' Voice (Core.Maybe Types.LanguageCode)
vLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED vLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Human readable name of the language in English.
--
-- /Note:/ Consider using 'languageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vLanguageName :: Lens.Lens' Voice (Core.Maybe Types.LanguageName)
vLanguageName = Lens.field @"languageName"
{-# DEPRECATED vLanguageName "Use generic-lens or generic-optics with 'languageName' instead." #-}

-- | Name of the voice (for example, Salli, Kendra, etc.). This provides a human readable voice name that you might display in your application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vName :: Lens.Lens' Voice (Core.Maybe Types.Name)
vName = Lens.field @"name"
{-# DEPRECATED vName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies which engines (@standard@ or @neural@ ) that are supported by a given voice.
--
-- /Note:/ Consider using 'supportedEngines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSupportedEngines :: Lens.Lens' Voice (Core.Maybe [Types.Engine])
vSupportedEngines = Lens.field @"supportedEngines"
{-# DEPRECATED vSupportedEngines "Use generic-lens or generic-optics with 'supportedEngines' instead." #-}

instance Core.FromJSON Voice where
  parseJSON =
    Core.withObject "Voice" Core.$
      \x ->
        Voice'
          Core.<$> (x Core..:? "AdditionalLanguageCodes")
          Core.<*> (x Core..:? "Gender")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "LanguageCode")
          Core.<*> (x Core..:? "LanguageName")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "SupportedEngines")
