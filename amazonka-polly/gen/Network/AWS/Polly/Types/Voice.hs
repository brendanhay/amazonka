{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.Engine
import Network.AWS.Polly.Types.Gender
import Network.AWS.Polly.Types.LanguageCode
import Network.AWS.Polly.Types.VoiceId
import qualified Network.AWS.Prelude as Prelude

-- | Description of the voice.
--
-- /See:/ 'newVoice' smart constructor.
data Voice = Voice'
  { -- | Language code of the voice.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Amazon Polly assigned voice ID. This is the ID that you specify when
    -- calling the @SynthesizeSpeech@ operation.
    id :: Prelude.Maybe VoiceId,
    -- | Gender of the voice.
    gender :: Prelude.Maybe Gender,
    -- | Name of the voice (for example, Salli, Kendra, etc.). This provides a
    -- human readable voice name that you might display in your application.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies which engines (@standard@ or @neural@) that are supported by a
    -- given voice.
    supportedEngines :: Prelude.Maybe [Engine],
    -- | Additional codes for languages available for the specified voice in
    -- addition to its default language.
    --
    -- For example, the default language for Aditi is Indian English (en-IN)
    -- because it was first used for that language. Since Aditi is bilingual
    -- and fluent in both Indian English and Hindi, this parameter would show
    -- the code @hi-IN@.
    additionalLanguageCodes :: Prelude.Maybe [LanguageCode],
    -- | Human readable name of the language in English.
    languageName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { languageCode = Prelude.Nothing,
      id = Prelude.Nothing,
      gender = Prelude.Nothing,
      name = Prelude.Nothing,
      supportedEngines = Prelude.Nothing,
      additionalLanguageCodes = Prelude.Nothing,
      languageName = Prelude.Nothing
    }

-- | Language code of the voice.
voice_languageCode :: Lens.Lens' Voice (Prelude.Maybe LanguageCode)
voice_languageCode = Lens.lens (\Voice' {languageCode} -> languageCode) (\s@Voice' {} a -> s {languageCode = a} :: Voice)

-- | Amazon Polly assigned voice ID. This is the ID that you specify when
-- calling the @SynthesizeSpeech@ operation.
voice_id :: Lens.Lens' Voice (Prelude.Maybe VoiceId)
voice_id = Lens.lens (\Voice' {id} -> id) (\s@Voice' {} a -> s {id = a} :: Voice)

-- | Gender of the voice.
voice_gender :: Lens.Lens' Voice (Prelude.Maybe Gender)
voice_gender = Lens.lens (\Voice' {gender} -> gender) (\s@Voice' {} a -> s {gender = a} :: Voice)

-- | Name of the voice (for example, Salli, Kendra, etc.). This provides a
-- human readable voice name that you might display in your application.
voice_name :: Lens.Lens' Voice (Prelude.Maybe Prelude.Text)
voice_name = Lens.lens (\Voice' {name} -> name) (\s@Voice' {} a -> s {name = a} :: Voice)

-- | Specifies which engines (@standard@ or @neural@) that are supported by a
-- given voice.
voice_supportedEngines :: Lens.Lens' Voice (Prelude.Maybe [Engine])
voice_supportedEngines = Lens.lens (\Voice' {supportedEngines} -> supportedEngines) (\s@Voice' {} a -> s {supportedEngines = a} :: Voice) Prelude.. Lens.mapping Prelude._Coerce

-- | Additional codes for languages available for the specified voice in
-- addition to its default language.
--
-- For example, the default language for Aditi is Indian English (en-IN)
-- because it was first used for that language. Since Aditi is bilingual
-- and fluent in both Indian English and Hindi, this parameter would show
-- the code @hi-IN@.
voice_additionalLanguageCodes :: Lens.Lens' Voice (Prelude.Maybe [LanguageCode])
voice_additionalLanguageCodes = Lens.lens (\Voice' {additionalLanguageCodes} -> additionalLanguageCodes) (\s@Voice' {} a -> s {additionalLanguageCodes = a} :: Voice) Prelude.. Lens.mapping Prelude._Coerce

-- | Human readable name of the language in English.
voice_languageName :: Lens.Lens' Voice (Prelude.Maybe Prelude.Text)
voice_languageName = Lens.lens (\Voice' {languageName} -> languageName) (\s@Voice' {} a -> s {languageName = a} :: Voice)

instance Prelude.FromJSON Voice where
  parseJSON =
    Prelude.withObject
      "Voice"
      ( \x ->
          Voice'
            Prelude.<$> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Gender")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> ( x Prelude..:? "SupportedEngines"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "AdditionalLanguageCodes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "LanguageName")
      )

instance Prelude.Hashable Voice

instance Prelude.NFData Voice
