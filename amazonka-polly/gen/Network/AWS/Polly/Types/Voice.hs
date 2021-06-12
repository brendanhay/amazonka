{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Voice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.Voice where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.Engine
import Network.AWS.Polly.Types.Gender
import Network.AWS.Polly.Types.LanguageCode
import Network.AWS.Polly.Types.VoiceId

-- | Description of the voice.
--
-- /See:/ 'newVoice' smart constructor.
data Voice = Voice'
  { -- | Language code of the voice.
    languageCode :: Core.Maybe LanguageCode,
    -- | Amazon Polly assigned voice ID. This is the ID that you specify when
    -- calling the @SynthesizeSpeech@ operation.
    id :: Core.Maybe VoiceId,
    -- | Gender of the voice.
    gender :: Core.Maybe Gender,
    -- | Name of the voice (for example, Salli, Kendra, etc.). This provides a
    -- human readable voice name that you might display in your application.
    name :: Core.Maybe Core.Text,
    -- | Specifies which engines (@standard@ or @neural@) that are supported by a
    -- given voice.
    supportedEngines :: Core.Maybe [Engine],
    -- | Additional codes for languages available for the specified voice in
    -- addition to its default language.
    --
    -- For example, the default language for Aditi is Indian English (en-IN)
    -- because it was first used for that language. Since Aditi is bilingual
    -- and fluent in both Indian English and Hindi, this parameter would show
    -- the code @hi-IN@.
    additionalLanguageCodes :: Core.Maybe [LanguageCode],
    -- | Human readable name of the language in English.
    languageName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Voice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'voice_languageCode' - Language code of the voice.
--
-- 'id', 'voice_id' - Amazon Polly assigned voice ID. This is the ID that you specify when
-- calling the @SynthesizeSpeech@ operation.
--
-- 'gender', 'voice_gender' - Gender of the voice.
--
-- 'name', 'voice_name' - Name of the voice (for example, Salli, Kendra, etc.). This provides a
-- human readable voice name that you might display in your application.
--
-- 'supportedEngines', 'voice_supportedEngines' - Specifies which engines (@standard@ or @neural@) that are supported by a
-- given voice.
--
-- 'additionalLanguageCodes', 'voice_additionalLanguageCodes' - Additional codes for languages available for the specified voice in
-- addition to its default language.
--
-- For example, the default language for Aditi is Indian English (en-IN)
-- because it was first used for that language. Since Aditi is bilingual
-- and fluent in both Indian English and Hindi, this parameter would show
-- the code @hi-IN@.
--
-- 'languageName', 'voice_languageName' - Human readable name of the language in English.
newVoice ::
  Voice
newVoice =
  Voice'
    { languageCode = Core.Nothing,
      id = Core.Nothing,
      gender = Core.Nothing,
      name = Core.Nothing,
      supportedEngines = Core.Nothing,
      additionalLanguageCodes = Core.Nothing,
      languageName = Core.Nothing
    }

-- | Language code of the voice.
voice_languageCode :: Lens.Lens' Voice (Core.Maybe LanguageCode)
voice_languageCode = Lens.lens (\Voice' {languageCode} -> languageCode) (\s@Voice' {} a -> s {languageCode = a} :: Voice)

-- | Amazon Polly assigned voice ID. This is the ID that you specify when
-- calling the @SynthesizeSpeech@ operation.
voice_id :: Lens.Lens' Voice (Core.Maybe VoiceId)
voice_id = Lens.lens (\Voice' {id} -> id) (\s@Voice' {} a -> s {id = a} :: Voice)

-- | Gender of the voice.
voice_gender :: Lens.Lens' Voice (Core.Maybe Gender)
voice_gender = Lens.lens (\Voice' {gender} -> gender) (\s@Voice' {} a -> s {gender = a} :: Voice)

-- | Name of the voice (for example, Salli, Kendra, etc.). This provides a
-- human readable voice name that you might display in your application.
voice_name :: Lens.Lens' Voice (Core.Maybe Core.Text)
voice_name = Lens.lens (\Voice' {name} -> name) (\s@Voice' {} a -> s {name = a} :: Voice)

-- | Specifies which engines (@standard@ or @neural@) that are supported by a
-- given voice.
voice_supportedEngines :: Lens.Lens' Voice (Core.Maybe [Engine])
voice_supportedEngines = Lens.lens (\Voice' {supportedEngines} -> supportedEngines) (\s@Voice' {} a -> s {supportedEngines = a} :: Voice) Core.. Lens.mapping Lens._Coerce

-- | Additional codes for languages available for the specified voice in
-- addition to its default language.
--
-- For example, the default language for Aditi is Indian English (en-IN)
-- because it was first used for that language. Since Aditi is bilingual
-- and fluent in both Indian English and Hindi, this parameter would show
-- the code @hi-IN@.
voice_additionalLanguageCodes :: Lens.Lens' Voice (Core.Maybe [LanguageCode])
voice_additionalLanguageCodes = Lens.lens (\Voice' {additionalLanguageCodes} -> additionalLanguageCodes) (\s@Voice' {} a -> s {additionalLanguageCodes = a} :: Voice) Core.. Lens.mapping Lens._Coerce

-- | Human readable name of the language in English.
voice_languageName :: Lens.Lens' Voice (Core.Maybe Core.Text)
voice_languageName = Lens.lens (\Voice' {languageName} -> languageName) (\s@Voice' {} a -> s {languageName = a} :: Voice)

instance Core.FromJSON Voice where
  parseJSON =
    Core.withObject
      "Voice"
      ( \x ->
          Voice'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Gender")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "SupportedEngines" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "AdditionalLanguageCodes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "LanguageName")
      )

instance Core.Hashable Voice

instance Core.NFData Voice
