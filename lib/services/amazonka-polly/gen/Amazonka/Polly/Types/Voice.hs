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
-- Module      : Amazonka.Polly.Types.Voice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Polly.Types.Voice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Polly.Types.Engine
import Amazonka.Polly.Types.Gender
import Amazonka.Polly.Types.LanguageCode
import Amazonka.Polly.Types.VoiceId
import qualified Amazonka.Prelude as Prelude

-- | Description of the voice.
--
-- /See:/ 'newVoice' smart constructor.
data Voice = Voice'
  { -- | Additional codes for languages available for the specified voice in
    -- addition to its default language.
    --
    -- For example, the default language for Aditi is Indian English (en-IN)
    -- because it was first used for that language. Since Aditi is bilingual
    -- and fluent in both Indian English and Hindi, this parameter would show
    -- the code @hi-IN@.
    additionalLanguageCodes :: Prelude.Maybe [LanguageCode],
    -- | Gender of the voice.
    gender :: Prelude.Maybe Gender,
    -- | Amazon Polly assigned voice ID. This is the ID that you specify when
    -- calling the @SynthesizeSpeech@ operation.
    id :: Prelude.Maybe VoiceId,
    -- | Language code of the voice.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Human readable name of the language in English.
    languageName :: Prelude.Maybe Prelude.Text,
    -- | Name of the voice (for example, Salli, Kendra, etc.). This provides a
    -- human readable voice name that you might display in your application.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies which engines (@standard@ or @neural@) that are supported by a
    -- given voice.
    supportedEngines :: Prelude.Maybe [Engine]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Voice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalLanguageCodes', 'voice_additionalLanguageCodes' - Additional codes for languages available for the specified voice in
-- addition to its default language.
--
-- For example, the default language for Aditi is Indian English (en-IN)
-- because it was first used for that language. Since Aditi is bilingual
-- and fluent in both Indian English and Hindi, this parameter would show
-- the code @hi-IN@.
--
-- 'gender', 'voice_gender' - Gender of the voice.
--
-- 'id', 'voice_id' - Amazon Polly assigned voice ID. This is the ID that you specify when
-- calling the @SynthesizeSpeech@ operation.
--
-- 'languageCode', 'voice_languageCode' - Language code of the voice.
--
-- 'languageName', 'voice_languageName' - Human readable name of the language in English.
--
-- 'name', 'voice_name' - Name of the voice (for example, Salli, Kendra, etc.). This provides a
-- human readable voice name that you might display in your application.
--
-- 'supportedEngines', 'voice_supportedEngines' - Specifies which engines (@standard@ or @neural@) that are supported by a
-- given voice.
newVoice ::
  Voice
newVoice =
  Voice'
    { additionalLanguageCodes = Prelude.Nothing,
      gender = Prelude.Nothing,
      id = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      languageName = Prelude.Nothing,
      name = Prelude.Nothing,
      supportedEngines = Prelude.Nothing
    }

-- | Additional codes for languages available for the specified voice in
-- addition to its default language.
--
-- For example, the default language for Aditi is Indian English (en-IN)
-- because it was first used for that language. Since Aditi is bilingual
-- and fluent in both Indian English and Hindi, this parameter would show
-- the code @hi-IN@.
voice_additionalLanguageCodes :: Lens.Lens' Voice (Prelude.Maybe [LanguageCode])
voice_additionalLanguageCodes = Lens.lens (\Voice' {additionalLanguageCodes} -> additionalLanguageCodes) (\s@Voice' {} a -> s {additionalLanguageCodes = a} :: Voice) Prelude.. Lens.mapping Lens.coerced

-- | Gender of the voice.
voice_gender :: Lens.Lens' Voice (Prelude.Maybe Gender)
voice_gender = Lens.lens (\Voice' {gender} -> gender) (\s@Voice' {} a -> s {gender = a} :: Voice)

-- | Amazon Polly assigned voice ID. This is the ID that you specify when
-- calling the @SynthesizeSpeech@ operation.
voice_id :: Lens.Lens' Voice (Prelude.Maybe VoiceId)
voice_id = Lens.lens (\Voice' {id} -> id) (\s@Voice' {} a -> s {id = a} :: Voice)

-- | Language code of the voice.
voice_languageCode :: Lens.Lens' Voice (Prelude.Maybe LanguageCode)
voice_languageCode = Lens.lens (\Voice' {languageCode} -> languageCode) (\s@Voice' {} a -> s {languageCode = a} :: Voice)

-- | Human readable name of the language in English.
voice_languageName :: Lens.Lens' Voice (Prelude.Maybe Prelude.Text)
voice_languageName = Lens.lens (\Voice' {languageName} -> languageName) (\s@Voice' {} a -> s {languageName = a} :: Voice)

-- | Name of the voice (for example, Salli, Kendra, etc.). This provides a
-- human readable voice name that you might display in your application.
voice_name :: Lens.Lens' Voice (Prelude.Maybe Prelude.Text)
voice_name = Lens.lens (\Voice' {name} -> name) (\s@Voice' {} a -> s {name = a} :: Voice)

-- | Specifies which engines (@standard@ or @neural@) that are supported by a
-- given voice.
voice_supportedEngines :: Lens.Lens' Voice (Prelude.Maybe [Engine])
voice_supportedEngines = Lens.lens (\Voice' {supportedEngines} -> supportedEngines) (\s@Voice' {} a -> s {supportedEngines = a} :: Voice) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Voice where
  parseJSON =
    Data.withObject
      "Voice"
      ( \x ->
          Voice'
            Prelude.<$> ( x Data..:? "AdditionalLanguageCodes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Gender")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "LanguageName")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> ( x Data..:? "SupportedEngines"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Voice where
  hashWithSalt _salt Voice' {..} =
    _salt
      `Prelude.hashWithSalt` additionalLanguageCodes
      `Prelude.hashWithSalt` gender
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` languageName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` supportedEngines

instance Prelude.NFData Voice where
  rnf Voice' {..} =
    Prelude.rnf additionalLanguageCodes
      `Prelude.seq` Prelude.rnf gender
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf languageName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf supportedEngines
