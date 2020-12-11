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
    vLanguageCode,
    vLanguageName,
    vGender,
    vName,
    vId,
    vAdditionalLanguageCodes,
    vSupportedEngines,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.Engine
import Network.AWS.Polly.Types.Gender
import Network.AWS.Polly.Types.LanguageCode
import Network.AWS.Polly.Types.VoiceId
import qualified Network.AWS.Prelude as Lude

-- | Description of the voice.
--
-- /See:/ 'mkVoice' smart constructor.
data Voice = Voice'
  { languageCode :: Lude.Maybe LanguageCode,
    languageName :: Lude.Maybe Lude.Text,
    gender :: Lude.Maybe Gender,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe VoiceId,
    additionalLanguageCodes :: Lude.Maybe [LanguageCode],
    supportedEngines :: Lude.Maybe [Engine]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Voice' with the minimum fields required to make a request.
--
-- * 'additionalLanguageCodes' - Additional codes for languages available for the specified voice in addition to its default language.
--
-- For example, the default language for Aditi is Indian English (en-IN) because it was first used for that language. Since Aditi is bilingual and fluent in both Indian English and Hindi, this parameter would show the code @hi-IN@ .
-- * 'gender' - Gender of the voice.
-- * 'id' - Amazon Polly assigned voice ID. This is the ID that you specify when calling the @SynthesizeSpeech@ operation.
-- * 'languageCode' - Language code of the voice.
-- * 'languageName' - Human readable name of the language in English.
-- * 'name' - Name of the voice (for example, Salli, Kendra, etc.). This provides a human readable voice name that you might display in your application.
-- * 'supportedEngines' - Specifies which engines (@standard@ or @neural@ ) that are supported by a given voice.
mkVoice ::
  Voice
mkVoice =
  Voice'
    { languageCode = Lude.Nothing,
      languageName = Lude.Nothing,
      gender = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      additionalLanguageCodes = Lude.Nothing,
      supportedEngines = Lude.Nothing
    }

-- | Language code of the voice.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vLanguageCode :: Lens.Lens' Voice (Lude.Maybe LanguageCode)
vLanguageCode = Lens.lens (languageCode :: Voice -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: Voice)
{-# DEPRECATED vLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Human readable name of the language in English.
--
-- /Note:/ Consider using 'languageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vLanguageName :: Lens.Lens' Voice (Lude.Maybe Lude.Text)
vLanguageName = Lens.lens (languageName :: Voice -> Lude.Maybe Lude.Text) (\s a -> s {languageName = a} :: Voice)
{-# DEPRECATED vLanguageName "Use generic-lens or generic-optics with 'languageName' instead." #-}

-- | Gender of the voice.
--
-- /Note:/ Consider using 'gender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vGender :: Lens.Lens' Voice (Lude.Maybe Gender)
vGender = Lens.lens (gender :: Voice -> Lude.Maybe Gender) (\s a -> s {gender = a} :: Voice)
{-# DEPRECATED vGender "Use generic-lens or generic-optics with 'gender' instead." #-}

-- | Name of the voice (for example, Salli, Kendra, etc.). This provides a human readable voice name that you might display in your application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vName :: Lens.Lens' Voice (Lude.Maybe Lude.Text)
vName = Lens.lens (name :: Voice -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Voice)
{-# DEPRECATED vName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Amazon Polly assigned voice ID. This is the ID that you specify when calling the @SynthesizeSpeech@ operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vId :: Lens.Lens' Voice (Lude.Maybe VoiceId)
vId = Lens.lens (id :: Voice -> Lude.Maybe VoiceId) (\s a -> s {id = a} :: Voice)
{-# DEPRECATED vId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Additional codes for languages available for the specified voice in addition to its default language.
--
-- For example, the default language for Aditi is Indian English (en-IN) because it was first used for that language. Since Aditi is bilingual and fluent in both Indian English and Hindi, this parameter would show the code @hi-IN@ .
--
-- /Note:/ Consider using 'additionalLanguageCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vAdditionalLanguageCodes :: Lens.Lens' Voice (Lude.Maybe [LanguageCode])
vAdditionalLanguageCodes = Lens.lens (additionalLanguageCodes :: Voice -> Lude.Maybe [LanguageCode]) (\s a -> s {additionalLanguageCodes = a} :: Voice)
{-# DEPRECATED vAdditionalLanguageCodes "Use generic-lens or generic-optics with 'additionalLanguageCodes' instead." #-}

-- | Specifies which engines (@standard@ or @neural@ ) that are supported by a given voice.
--
-- /Note:/ Consider using 'supportedEngines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSupportedEngines :: Lens.Lens' Voice (Lude.Maybe [Engine])
vSupportedEngines = Lens.lens (supportedEngines :: Voice -> Lude.Maybe [Engine]) (\s a -> s {supportedEngines = a} :: Voice)
{-# DEPRECATED vSupportedEngines "Use generic-lens or generic-optics with 'supportedEngines' instead." #-}

instance Lude.FromJSON Voice where
  parseJSON =
    Lude.withObject
      "Voice"
      ( \x ->
          Voice'
            Lude.<$> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "LanguageName")
            Lude.<*> (x Lude..:? "Gender")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "AdditionalLanguageCodes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SupportedEngines" Lude..!= Lude.mempty)
      )
