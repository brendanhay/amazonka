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
-- Module      : Amazonka.Chime.Types.EngineTranscribeMedicalSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.EngineTranscribeMedicalSettings where

import Amazonka.Chime.Types.TranscribeMedicalContentIdentificationType
import Amazonka.Chime.Types.TranscribeMedicalLanguageCode
import Amazonka.Chime.Types.TranscribeMedicalRegion
import Amazonka.Chime.Types.TranscribeMedicalSpecialty
import Amazonka.Chime.Types.TranscribeMedicalType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings specific to the Amazon Transcribe Medical engine.
--
-- /See:/ 'newEngineTranscribeMedicalSettings' smart constructor.
data EngineTranscribeMedicalSettings = EngineTranscribeMedicalSettings'
  { -- | Set this field to @PHI@ to identify personal health information in the
    -- transcription output.
    contentIdentificationType :: Prelude.Maybe TranscribeMedicalContentIdentificationType,
    -- | The AWS Region passed to Amazon Transcribe Medical. If you don\'t
    -- specify a Region, Amazon Chime uses the meeting\'s Region.
    region :: Prelude.Maybe TranscribeMedicalRegion,
    -- | The name of the vocabulary passed to Amazon Transcribe Medical.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The language code specified for the Amazon Transcribe Medical engine.
    languageCode :: TranscribeMedicalLanguageCode,
    -- | The specialty specified for the Amazon Transcribe Medical engine.
    specialty :: TranscribeMedicalSpecialty,
    -- | The type of transcription.
    type' :: TranscribeMedicalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EngineTranscribeMedicalSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentIdentificationType', 'engineTranscribeMedicalSettings_contentIdentificationType' - Set this field to @PHI@ to identify personal health information in the
-- transcription output.
--
-- 'region', 'engineTranscribeMedicalSettings_region' - The AWS Region passed to Amazon Transcribe Medical. If you don\'t
-- specify a Region, Amazon Chime uses the meeting\'s Region.
--
-- 'vocabularyName', 'engineTranscribeMedicalSettings_vocabularyName' - The name of the vocabulary passed to Amazon Transcribe Medical.
--
-- 'languageCode', 'engineTranscribeMedicalSettings_languageCode' - The language code specified for the Amazon Transcribe Medical engine.
--
-- 'specialty', 'engineTranscribeMedicalSettings_specialty' - The specialty specified for the Amazon Transcribe Medical engine.
--
-- 'type'', 'engineTranscribeMedicalSettings_type' - The type of transcription.
newEngineTranscribeMedicalSettings ::
  -- | 'languageCode'
  TranscribeMedicalLanguageCode ->
  -- | 'specialty'
  TranscribeMedicalSpecialty ->
  -- | 'type''
  TranscribeMedicalType ->
  EngineTranscribeMedicalSettings
newEngineTranscribeMedicalSettings
  pLanguageCode_
  pSpecialty_
  pType_ =
    EngineTranscribeMedicalSettings'
      { contentIdentificationType =
          Prelude.Nothing,
        region = Prelude.Nothing,
        vocabularyName = Prelude.Nothing,
        languageCode = pLanguageCode_,
        specialty = pSpecialty_,
        type' = pType_
      }

-- | Set this field to @PHI@ to identify personal health information in the
-- transcription output.
engineTranscribeMedicalSettings_contentIdentificationType :: Lens.Lens' EngineTranscribeMedicalSettings (Prelude.Maybe TranscribeMedicalContentIdentificationType)
engineTranscribeMedicalSettings_contentIdentificationType = Lens.lens (\EngineTranscribeMedicalSettings' {contentIdentificationType} -> contentIdentificationType) (\s@EngineTranscribeMedicalSettings' {} a -> s {contentIdentificationType = a} :: EngineTranscribeMedicalSettings)

-- | The AWS Region passed to Amazon Transcribe Medical. If you don\'t
-- specify a Region, Amazon Chime uses the meeting\'s Region.
engineTranscribeMedicalSettings_region :: Lens.Lens' EngineTranscribeMedicalSettings (Prelude.Maybe TranscribeMedicalRegion)
engineTranscribeMedicalSettings_region = Lens.lens (\EngineTranscribeMedicalSettings' {region} -> region) (\s@EngineTranscribeMedicalSettings' {} a -> s {region = a} :: EngineTranscribeMedicalSettings)

-- | The name of the vocabulary passed to Amazon Transcribe Medical.
engineTranscribeMedicalSettings_vocabularyName :: Lens.Lens' EngineTranscribeMedicalSettings (Prelude.Maybe Prelude.Text)
engineTranscribeMedicalSettings_vocabularyName = Lens.lens (\EngineTranscribeMedicalSettings' {vocabularyName} -> vocabularyName) (\s@EngineTranscribeMedicalSettings' {} a -> s {vocabularyName = a} :: EngineTranscribeMedicalSettings)

-- | The language code specified for the Amazon Transcribe Medical engine.
engineTranscribeMedicalSettings_languageCode :: Lens.Lens' EngineTranscribeMedicalSettings TranscribeMedicalLanguageCode
engineTranscribeMedicalSettings_languageCode = Lens.lens (\EngineTranscribeMedicalSettings' {languageCode} -> languageCode) (\s@EngineTranscribeMedicalSettings' {} a -> s {languageCode = a} :: EngineTranscribeMedicalSettings)

-- | The specialty specified for the Amazon Transcribe Medical engine.
engineTranscribeMedicalSettings_specialty :: Lens.Lens' EngineTranscribeMedicalSettings TranscribeMedicalSpecialty
engineTranscribeMedicalSettings_specialty = Lens.lens (\EngineTranscribeMedicalSettings' {specialty} -> specialty) (\s@EngineTranscribeMedicalSettings' {} a -> s {specialty = a} :: EngineTranscribeMedicalSettings)

-- | The type of transcription.
engineTranscribeMedicalSettings_type :: Lens.Lens' EngineTranscribeMedicalSettings TranscribeMedicalType
engineTranscribeMedicalSettings_type = Lens.lens (\EngineTranscribeMedicalSettings' {type'} -> type') (\s@EngineTranscribeMedicalSettings' {} a -> s {type' = a} :: EngineTranscribeMedicalSettings)

instance
  Prelude.Hashable
    EngineTranscribeMedicalSettings
  where
  hashWithSalt
    _salt
    EngineTranscribeMedicalSettings' {..} =
      _salt
        `Prelude.hashWithSalt` contentIdentificationType
        `Prelude.hashWithSalt` region
        `Prelude.hashWithSalt` vocabularyName
        `Prelude.hashWithSalt` languageCode
        `Prelude.hashWithSalt` specialty
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    EngineTranscribeMedicalSettings
  where
  rnf EngineTranscribeMedicalSettings' {..} =
    Prelude.rnf contentIdentificationType `Prelude.seq`
      Prelude.rnf region `Prelude.seq`
        Prelude.rnf vocabularyName `Prelude.seq`
          Prelude.rnf languageCode `Prelude.seq`
            Prelude.rnf specialty `Prelude.seq`
              Prelude.rnf type'

instance Data.ToJSON EngineTranscribeMedicalSettings where
  toJSON EngineTranscribeMedicalSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentIdentificationType" Data..=)
              Prelude.<$> contentIdentificationType,
            ("Region" Data..=) Prelude.<$> region,
            ("VocabularyName" Data..=)
              Prelude.<$> vocabularyName,
            Prelude.Just ("LanguageCode" Data..= languageCode),
            Prelude.Just ("Specialty" Data..= specialty),
            Prelude.Just ("Type" Data..= type')
          ]
      )
