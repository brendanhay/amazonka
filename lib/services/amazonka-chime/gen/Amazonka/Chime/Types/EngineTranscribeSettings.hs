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
-- Module      : Amazonka.Chime.Types.EngineTranscribeSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.EngineTranscribeSettings where

import Amazonka.Chime.Types.TranscribeContentIdentificationType
import Amazonka.Chime.Types.TranscribeContentRedactionType
import Amazonka.Chime.Types.TranscribeLanguageCode
import Amazonka.Chime.Types.TranscribePartialResultsStability
import Amazonka.Chime.Types.TranscribeRegion
import Amazonka.Chime.Types.TranscribeVocabularyFilterMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings specific to the Amazon Transcribe engine.
--
-- /See:/ 'newEngineTranscribeSettings' smart constructor.
data EngineTranscribeSettings = EngineTranscribeSettings'
  { -- | Set this field to @PII@ to identify personally identifiable information
    -- in the transcription output.
    contentIdentificationType :: Prelude.Maybe TranscribeContentIdentificationType,
    -- | Set this field to @PII@ to redact personally identifiable information in
    -- the transcription output. Content redaction is performed only upon
    -- complete transcription of the audio segments.
    contentRedactionType :: Prelude.Maybe TranscribeContentRedactionType,
    -- | Generates partial transcription results that are less likely to change
    -- as meeting attendees speak. It does so by only allowing the last few
    -- words from the partial results to change.
    enablePartialResultsStabilization :: Prelude.Maybe Prelude.Bool,
    -- | The name of the language model used during transcription.
    languageModelName :: Prelude.Maybe Prelude.Text,
    -- | The stabity level of a partial results transcription. Determines how
    -- stable you want the transcription results to be. A higher level means
    -- the transcription results are less likely to change.
    partialResultsStability :: Prelude.Maybe TranscribePartialResultsStability,
    -- | Lists the PII entity types you want to identify or redact. To specify
    -- entity types, you must enable @ContentIdentificationType@ or
    -- @ContentRedactionType@.
    --
    -- @PIIEntityTypes@ must be comma-separated. The available values are:
    -- @BANK_ACCOUNT_NUMBER@, @BANK_ROUTING, CREDIT_DEBIT_NUMBER@,
    -- @CREDIT_DEBIT_CVV@, @CREDIT_DEBIT_EXPIRY@, @PIN@, @EMAIL@, @ADDRESS@,
    -- @NAME@, @PHONE@, @SSN@, and @ALL@.
    --
    -- @PiiEntityTypes@ is an optional parameter with a default value of @ALL@.
    piiEntityTypes :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region passed to Amazon Transcribe. If you don\'t specify a
    -- Region, Amazon Chime uses the meeting\'s Region.
    region :: Prelude.Maybe TranscribeRegion,
    -- | The filtering method passed to Amazon Transcribe.
    vocabularyFilterMethod :: Prelude.Maybe TranscribeVocabularyFilterMethod,
    -- | The name of the vocabulary filter passed to Amazon Transcribe.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | The name of the vocabulary passed to Amazon Transcribe.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The language code specified for the Amazon Transcribe engine.
    languageCode :: TranscribeLanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EngineTranscribeSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentIdentificationType', 'engineTranscribeSettings_contentIdentificationType' - Set this field to @PII@ to identify personally identifiable information
-- in the transcription output.
--
-- 'contentRedactionType', 'engineTranscribeSettings_contentRedactionType' - Set this field to @PII@ to redact personally identifiable information in
-- the transcription output. Content redaction is performed only upon
-- complete transcription of the audio segments.
--
-- 'enablePartialResultsStabilization', 'engineTranscribeSettings_enablePartialResultsStabilization' - Generates partial transcription results that are less likely to change
-- as meeting attendees speak. It does so by only allowing the last few
-- words from the partial results to change.
--
-- 'languageModelName', 'engineTranscribeSettings_languageModelName' - The name of the language model used during transcription.
--
-- 'partialResultsStability', 'engineTranscribeSettings_partialResultsStability' - The stabity level of a partial results transcription. Determines how
-- stable you want the transcription results to be. A higher level means
-- the transcription results are less likely to change.
--
-- 'piiEntityTypes', 'engineTranscribeSettings_piiEntityTypes' - Lists the PII entity types you want to identify or redact. To specify
-- entity types, you must enable @ContentIdentificationType@ or
-- @ContentRedactionType@.
--
-- @PIIEntityTypes@ must be comma-separated. The available values are:
-- @BANK_ACCOUNT_NUMBER@, @BANK_ROUTING, CREDIT_DEBIT_NUMBER@,
-- @CREDIT_DEBIT_CVV@, @CREDIT_DEBIT_EXPIRY@, @PIN@, @EMAIL@, @ADDRESS@,
-- @NAME@, @PHONE@, @SSN@, and @ALL@.
--
-- @PiiEntityTypes@ is an optional parameter with a default value of @ALL@.
--
-- 'region', 'engineTranscribeSettings_region' - The AWS Region passed to Amazon Transcribe. If you don\'t specify a
-- Region, Amazon Chime uses the meeting\'s Region.
--
-- 'vocabularyFilterMethod', 'engineTranscribeSettings_vocabularyFilterMethod' - The filtering method passed to Amazon Transcribe.
--
-- 'vocabularyFilterName', 'engineTranscribeSettings_vocabularyFilterName' - The name of the vocabulary filter passed to Amazon Transcribe.
--
-- 'vocabularyName', 'engineTranscribeSettings_vocabularyName' - The name of the vocabulary passed to Amazon Transcribe.
--
-- 'languageCode', 'engineTranscribeSettings_languageCode' - The language code specified for the Amazon Transcribe engine.
newEngineTranscribeSettings ::
  -- | 'languageCode'
  TranscribeLanguageCode ->
  EngineTranscribeSettings
newEngineTranscribeSettings pLanguageCode_ =
  EngineTranscribeSettings'
    { contentIdentificationType =
        Prelude.Nothing,
      contentRedactionType = Prelude.Nothing,
      enablePartialResultsStabilization =
        Prelude.Nothing,
      languageModelName = Prelude.Nothing,
      partialResultsStability = Prelude.Nothing,
      piiEntityTypes = Prelude.Nothing,
      region = Prelude.Nothing,
      vocabularyFilterMethod = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      languageCode = pLanguageCode_
    }

-- | Set this field to @PII@ to identify personally identifiable information
-- in the transcription output.
engineTranscribeSettings_contentIdentificationType :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe TranscribeContentIdentificationType)
engineTranscribeSettings_contentIdentificationType = Lens.lens (\EngineTranscribeSettings' {contentIdentificationType} -> contentIdentificationType) (\s@EngineTranscribeSettings' {} a -> s {contentIdentificationType = a} :: EngineTranscribeSettings)

-- | Set this field to @PII@ to redact personally identifiable information in
-- the transcription output. Content redaction is performed only upon
-- complete transcription of the audio segments.
engineTranscribeSettings_contentRedactionType :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe TranscribeContentRedactionType)
engineTranscribeSettings_contentRedactionType = Lens.lens (\EngineTranscribeSettings' {contentRedactionType} -> contentRedactionType) (\s@EngineTranscribeSettings' {} a -> s {contentRedactionType = a} :: EngineTranscribeSettings)

-- | Generates partial transcription results that are less likely to change
-- as meeting attendees speak. It does so by only allowing the last few
-- words from the partial results to change.
engineTranscribeSettings_enablePartialResultsStabilization :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe Prelude.Bool)
engineTranscribeSettings_enablePartialResultsStabilization = Lens.lens (\EngineTranscribeSettings' {enablePartialResultsStabilization} -> enablePartialResultsStabilization) (\s@EngineTranscribeSettings' {} a -> s {enablePartialResultsStabilization = a} :: EngineTranscribeSettings)

-- | The name of the language model used during transcription.
engineTranscribeSettings_languageModelName :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe Prelude.Text)
engineTranscribeSettings_languageModelName = Lens.lens (\EngineTranscribeSettings' {languageModelName} -> languageModelName) (\s@EngineTranscribeSettings' {} a -> s {languageModelName = a} :: EngineTranscribeSettings)

-- | The stabity level of a partial results transcription. Determines how
-- stable you want the transcription results to be. A higher level means
-- the transcription results are less likely to change.
engineTranscribeSettings_partialResultsStability :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe TranscribePartialResultsStability)
engineTranscribeSettings_partialResultsStability = Lens.lens (\EngineTranscribeSettings' {partialResultsStability} -> partialResultsStability) (\s@EngineTranscribeSettings' {} a -> s {partialResultsStability = a} :: EngineTranscribeSettings)

-- | Lists the PII entity types you want to identify or redact. To specify
-- entity types, you must enable @ContentIdentificationType@ or
-- @ContentRedactionType@.
--
-- @PIIEntityTypes@ must be comma-separated. The available values are:
-- @BANK_ACCOUNT_NUMBER@, @BANK_ROUTING, CREDIT_DEBIT_NUMBER@,
-- @CREDIT_DEBIT_CVV@, @CREDIT_DEBIT_EXPIRY@, @PIN@, @EMAIL@, @ADDRESS@,
-- @NAME@, @PHONE@, @SSN@, and @ALL@.
--
-- @PiiEntityTypes@ is an optional parameter with a default value of @ALL@.
engineTranscribeSettings_piiEntityTypes :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe Prelude.Text)
engineTranscribeSettings_piiEntityTypes = Lens.lens (\EngineTranscribeSettings' {piiEntityTypes} -> piiEntityTypes) (\s@EngineTranscribeSettings' {} a -> s {piiEntityTypes = a} :: EngineTranscribeSettings)

-- | The AWS Region passed to Amazon Transcribe. If you don\'t specify a
-- Region, Amazon Chime uses the meeting\'s Region.
engineTranscribeSettings_region :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe TranscribeRegion)
engineTranscribeSettings_region = Lens.lens (\EngineTranscribeSettings' {region} -> region) (\s@EngineTranscribeSettings' {} a -> s {region = a} :: EngineTranscribeSettings)

-- | The filtering method passed to Amazon Transcribe.
engineTranscribeSettings_vocabularyFilterMethod :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe TranscribeVocabularyFilterMethod)
engineTranscribeSettings_vocabularyFilterMethod = Lens.lens (\EngineTranscribeSettings' {vocabularyFilterMethod} -> vocabularyFilterMethod) (\s@EngineTranscribeSettings' {} a -> s {vocabularyFilterMethod = a} :: EngineTranscribeSettings)

-- | The name of the vocabulary filter passed to Amazon Transcribe.
engineTranscribeSettings_vocabularyFilterName :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe Prelude.Text)
engineTranscribeSettings_vocabularyFilterName = Lens.lens (\EngineTranscribeSettings' {vocabularyFilterName} -> vocabularyFilterName) (\s@EngineTranscribeSettings' {} a -> s {vocabularyFilterName = a} :: EngineTranscribeSettings)

-- | The name of the vocabulary passed to Amazon Transcribe.
engineTranscribeSettings_vocabularyName :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe Prelude.Text)
engineTranscribeSettings_vocabularyName = Lens.lens (\EngineTranscribeSettings' {vocabularyName} -> vocabularyName) (\s@EngineTranscribeSettings' {} a -> s {vocabularyName = a} :: EngineTranscribeSettings)

-- | The language code specified for the Amazon Transcribe engine.
engineTranscribeSettings_languageCode :: Lens.Lens' EngineTranscribeSettings TranscribeLanguageCode
engineTranscribeSettings_languageCode = Lens.lens (\EngineTranscribeSettings' {languageCode} -> languageCode) (\s@EngineTranscribeSettings' {} a -> s {languageCode = a} :: EngineTranscribeSettings)

instance Prelude.Hashable EngineTranscribeSettings where
  hashWithSalt _salt EngineTranscribeSettings' {..} =
    _salt
      `Prelude.hashWithSalt` contentIdentificationType
      `Prelude.hashWithSalt` contentRedactionType
      `Prelude.hashWithSalt` enablePartialResultsStabilization
      `Prelude.hashWithSalt` languageModelName
      `Prelude.hashWithSalt` partialResultsStability
      `Prelude.hashWithSalt` piiEntityTypes
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` vocabularyFilterMethod
      `Prelude.hashWithSalt` vocabularyFilterName
      `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData EngineTranscribeSettings where
  rnf EngineTranscribeSettings' {..} =
    Prelude.rnf contentIdentificationType
      `Prelude.seq` Prelude.rnf contentRedactionType
      `Prelude.seq` Prelude.rnf enablePartialResultsStabilization
      `Prelude.seq` Prelude.rnf languageModelName
      `Prelude.seq` Prelude.rnf partialResultsStability
      `Prelude.seq` Prelude.rnf piiEntityTypes
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf vocabularyFilterMethod
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToJSON EngineTranscribeSettings where
  toJSON EngineTranscribeSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentIdentificationType" Data..=)
              Prelude.<$> contentIdentificationType,
            ("ContentRedactionType" Data..=)
              Prelude.<$> contentRedactionType,
            ("EnablePartialResultsStabilization" Data..=)
              Prelude.<$> enablePartialResultsStabilization,
            ("LanguageModelName" Data..=)
              Prelude.<$> languageModelName,
            ("PartialResultsStability" Data..=)
              Prelude.<$> partialResultsStability,
            ("PiiEntityTypes" Data..=)
              Prelude.<$> piiEntityTypes,
            ("Region" Data..=) Prelude.<$> region,
            ("VocabularyFilterMethod" Data..=)
              Prelude.<$> vocabularyFilterMethod,
            ("VocabularyFilterName" Data..=)
              Prelude.<$> vocabularyFilterName,
            ("VocabularyName" Data..=)
              Prelude.<$> vocabularyName,
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )
