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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.AmazonTranscribeProcessorConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.AmazonTranscribeProcessorConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.CallAnalyticsLanguageCode
import Amazonka.ChimeSdkMediaPipelines.Types.ContentType
import Amazonka.ChimeSdkMediaPipelines.Types.PartialResultsStability
import Amazonka.ChimeSdkMediaPipelines.Types.VocabularyFilterMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration settings for an Amazon
-- Transcribe processor.
--
-- /See:/ 'newAmazonTranscribeProcessorConfiguration' smart constructor.
data AmazonTranscribeProcessorConfiguration = AmazonTranscribeProcessorConfiguration'
  { -- | Labels all personally identifiable information (PII) identified in your
    -- transcript.
    --
    -- Content identification is performed at the segment level; PII specified
    -- in @PiiEntityTypes@ is flagged upon complete transcription of an audio
    -- segment.
    --
    -- You can’t set @ContentIdentificationType@ and @ContentRedactionType@ in
    -- the same request. If you set both, your request returns a
    -- @BadRequestException@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
    -- in the /Amazon Transcribe Developer Guide/.
    contentIdentificationType :: Prelude.Maybe ContentType,
    -- | Redacts all personally identifiable information (PII) identified in your
    -- transcript.
    --
    -- Content redaction is performed at the segment level; PII specified in
    -- PiiEntityTypes is redacted upon complete transcription of an audio
    -- segment.
    --
    -- You can’t set ContentRedactionType and ContentIdentificationType in the
    -- same request. If you set both, your request returns a
    -- @BadRequestException@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
    -- in the /Amazon Transcribe Developer Guide/.
    contentRedactionType :: Prelude.Maybe ContentType,
    -- | Enables partial result stabilization for your transcription. Partial
    -- result stabilization can reduce latency in your output, but may impact
    -- accuracy.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
    -- in the /Amazon Transcribe Developer Guide/.
    enablePartialResultsStabilization :: Prelude.Maybe Prelude.Bool,
    -- | If true, @TranscriptEvents@ with @IsPartial: true@ are filtered out of
    -- the insights target.
    filterPartialResults :: Prelude.Maybe Prelude.Bool,
    -- | The name of the custom language model that you want to use when
    -- processing your transcription. Note that language model names are case
    -- sensitive.
    --
    -- The language of the specified language model must match the language
    -- code you specify in your transcription request. If the languages don\'t
    -- match, the custom language model isn\'t applied. There are no errors or
    -- warnings associated with a language mismatch.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-language-models.html Custom language models>
    -- in the /Amazon Transcribe Developer Guide/.
    languageModelName :: Prelude.Maybe Prelude.Text,
    -- | The level of stability to use when you enable partial results
    -- stabilization (@EnablePartialResultsStabilization@).
    --
    -- Low stability provides the highest accuracy. High stability transcribes
    -- faster, but with slightly lower accuracy.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
    -- in the /Amazon Transcribe Developer Guide/.
    partialResultsStability :: Prelude.Maybe PartialResultsStability,
    -- | The types of personally identifiable information (PII) to redact from a
    -- transcript. You can include as many types as you\'d like, or you can
    -- select @ALL@.
    --
    -- To include @PiiEntityTypes@ in your Call Analytics request, you must
    -- also include @ContentIdentificationType@ or @ContentRedactionType@, but
    -- you can\'t include both.
    --
    -- Values must be comma-separated and can include: @ADDRESS@,
    -- @BANK_ACCOUNT_NUMBER@, @BANK_ROUTING@, @CREDIT_DEBIT_CVV@,
    -- @CREDIT_DEBIT_EXPIRY@, @CREDIT_DEBIT_NUMBER@, @EMAIL@, @NAME@, @PHONE@,
    -- @PIN@, @SSN@, or @ALL@.
    --
    -- If you leave this parameter empty, the default behavior is equivalent to
    -- @ALL@.
    piiEntityTypes :: Prelude.Maybe Prelude.Text,
    -- | Enables speaker partitioning (diarization) in your transcription output.
    -- Speaker partitioning labels the speech from individual speakers in your
    -- media file.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/diarization.html Partitioning speakers (diarization)>
    -- in the /Amazon Transcribe Developer Guide/.
    showSpeakerLabel :: Prelude.Maybe Prelude.Bool,
    -- | The vocabulary filtering method used in your Call Analytics
    -- transcription.
    vocabularyFilterMethod :: Prelude.Maybe VocabularyFilterMethod,
    -- | The name of the custom vocabulary filter that you specified in your Call
    -- Analytics request.
    --
    -- Length Constraints: Minimum length of 1. Maximum length of 200.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom vocabulary that you specified in your Call
    -- Analytics request.
    --
    -- Length Constraints: Minimum length of 1. Maximum length of 200.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The language code that represents the language spoken in your audio.
    --
    -- If you\'re unsure of the language spoken in your audio, consider using
    -- @IdentifyLanguage@ to enable automatic language identification.
    --
    -- For a list of languages that real-time Call Analytics supports, see the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages table>
    -- in the /Amazon Transcribe Developer Guide/.
    languageCode :: CallAnalyticsLanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonTranscribeProcessorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentIdentificationType', 'amazonTranscribeProcessorConfiguration_contentIdentificationType' - Labels all personally identifiable information (PII) identified in your
-- transcript.
--
-- Content identification is performed at the segment level; PII specified
-- in @PiiEntityTypes@ is flagged upon complete transcription of an audio
-- segment.
--
-- You can’t set @ContentIdentificationType@ and @ContentRedactionType@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'contentRedactionType', 'amazonTranscribeProcessorConfiguration_contentRedactionType' - Redacts all personally identifiable information (PII) identified in your
-- transcript.
--
-- Content redaction is performed at the segment level; PII specified in
-- PiiEntityTypes is redacted upon complete transcription of an audio
-- segment.
--
-- You can’t set ContentRedactionType and ContentIdentificationType in the
-- same request. If you set both, your request returns a
-- @BadRequestException@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'enablePartialResultsStabilization', 'amazonTranscribeProcessorConfiguration_enablePartialResultsStabilization' - Enables partial result stabilization for your transcription. Partial
-- result stabilization can reduce latency in your output, but may impact
-- accuracy.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'filterPartialResults', 'amazonTranscribeProcessorConfiguration_filterPartialResults' - If true, @TranscriptEvents@ with @IsPartial: true@ are filtered out of
-- the insights target.
--
-- 'languageModelName', 'amazonTranscribeProcessorConfiguration_languageModelName' - The name of the custom language model that you want to use when
-- processing your transcription. Note that language model names are case
-- sensitive.
--
-- The language of the specified language model must match the language
-- code you specify in your transcription request. If the languages don\'t
-- match, the custom language model isn\'t applied. There are no errors or
-- warnings associated with a language mismatch.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-language-models.html Custom language models>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'partialResultsStability', 'amazonTranscribeProcessorConfiguration_partialResultsStability' - The level of stability to use when you enable partial results
-- stabilization (@EnablePartialResultsStabilization@).
--
-- Low stability provides the highest accuracy. High stability transcribes
-- faster, but with slightly lower accuracy.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'piiEntityTypes', 'amazonTranscribeProcessorConfiguration_piiEntityTypes' - The types of personally identifiable information (PII) to redact from a
-- transcript. You can include as many types as you\'d like, or you can
-- select @ALL@.
--
-- To include @PiiEntityTypes@ in your Call Analytics request, you must
-- also include @ContentIdentificationType@ or @ContentRedactionType@, but
-- you can\'t include both.
--
-- Values must be comma-separated and can include: @ADDRESS@,
-- @BANK_ACCOUNT_NUMBER@, @BANK_ROUTING@, @CREDIT_DEBIT_CVV@,
-- @CREDIT_DEBIT_EXPIRY@, @CREDIT_DEBIT_NUMBER@, @EMAIL@, @NAME@, @PHONE@,
-- @PIN@, @SSN@, or @ALL@.
--
-- If you leave this parameter empty, the default behavior is equivalent to
-- @ALL@.
--
-- 'showSpeakerLabel', 'amazonTranscribeProcessorConfiguration_showSpeakerLabel' - Enables speaker partitioning (diarization) in your transcription output.
-- Speaker partitioning labels the speech from individual speakers in your
-- media file.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/diarization.html Partitioning speakers (diarization)>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'vocabularyFilterMethod', 'amazonTranscribeProcessorConfiguration_vocabularyFilterMethod' - The vocabulary filtering method used in your Call Analytics
-- transcription.
--
-- 'vocabularyFilterName', 'amazonTranscribeProcessorConfiguration_vocabularyFilterName' - The name of the custom vocabulary filter that you specified in your Call
-- Analytics request.
--
-- Length Constraints: Minimum length of 1. Maximum length of 200.
--
-- 'vocabularyName', 'amazonTranscribeProcessorConfiguration_vocabularyName' - The name of the custom vocabulary that you specified in your Call
-- Analytics request.
--
-- Length Constraints: Minimum length of 1. Maximum length of 200.
--
-- 'languageCode', 'amazonTranscribeProcessorConfiguration_languageCode' - The language code that represents the language spoken in your audio.
--
-- If you\'re unsure of the language spoken in your audio, consider using
-- @IdentifyLanguage@ to enable automatic language identification.
--
-- For a list of languages that real-time Call Analytics supports, see the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages table>
-- in the /Amazon Transcribe Developer Guide/.
newAmazonTranscribeProcessorConfiguration ::
  -- | 'languageCode'
  CallAnalyticsLanguageCode ->
  AmazonTranscribeProcessorConfiguration
newAmazonTranscribeProcessorConfiguration
  pLanguageCode_ =
    AmazonTranscribeProcessorConfiguration'
      { contentIdentificationType =
          Prelude.Nothing,
        contentRedactionType =
          Prelude.Nothing,
        enablePartialResultsStabilization =
          Prelude.Nothing,
        filterPartialResults =
          Prelude.Nothing,
        languageModelName = Prelude.Nothing,
        partialResultsStability =
          Prelude.Nothing,
        piiEntityTypes = Prelude.Nothing,
        showSpeakerLabel = Prelude.Nothing,
        vocabularyFilterMethod =
          Prelude.Nothing,
        vocabularyFilterName =
          Prelude.Nothing,
        vocabularyName = Prelude.Nothing,
        languageCode = pLanguageCode_
      }

-- | Labels all personally identifiable information (PII) identified in your
-- transcript.
--
-- Content identification is performed at the segment level; PII specified
-- in @PiiEntityTypes@ is flagged upon complete transcription of an audio
-- segment.
--
-- You can’t set @ContentIdentificationType@ and @ContentRedactionType@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeProcessorConfiguration_contentIdentificationType :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe ContentType)
amazonTranscribeProcessorConfiguration_contentIdentificationType = Lens.lens (\AmazonTranscribeProcessorConfiguration' {contentIdentificationType} -> contentIdentificationType) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {contentIdentificationType = a} :: AmazonTranscribeProcessorConfiguration)

-- | Redacts all personally identifiable information (PII) identified in your
-- transcript.
--
-- Content redaction is performed at the segment level; PII specified in
-- PiiEntityTypes is redacted upon complete transcription of an audio
-- segment.
--
-- You can’t set ContentRedactionType and ContentIdentificationType in the
-- same request. If you set both, your request returns a
-- @BadRequestException@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeProcessorConfiguration_contentRedactionType :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe ContentType)
amazonTranscribeProcessorConfiguration_contentRedactionType = Lens.lens (\AmazonTranscribeProcessorConfiguration' {contentRedactionType} -> contentRedactionType) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {contentRedactionType = a} :: AmazonTranscribeProcessorConfiguration)

-- | Enables partial result stabilization for your transcription. Partial
-- result stabilization can reduce latency in your output, but may impact
-- accuracy.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeProcessorConfiguration_enablePartialResultsStabilization :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe Prelude.Bool)
amazonTranscribeProcessorConfiguration_enablePartialResultsStabilization = Lens.lens (\AmazonTranscribeProcessorConfiguration' {enablePartialResultsStabilization} -> enablePartialResultsStabilization) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {enablePartialResultsStabilization = a} :: AmazonTranscribeProcessorConfiguration)

-- | If true, @TranscriptEvents@ with @IsPartial: true@ are filtered out of
-- the insights target.
amazonTranscribeProcessorConfiguration_filterPartialResults :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe Prelude.Bool)
amazonTranscribeProcessorConfiguration_filterPartialResults = Lens.lens (\AmazonTranscribeProcessorConfiguration' {filterPartialResults} -> filterPartialResults) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {filterPartialResults = a} :: AmazonTranscribeProcessorConfiguration)

-- | The name of the custom language model that you want to use when
-- processing your transcription. Note that language model names are case
-- sensitive.
--
-- The language of the specified language model must match the language
-- code you specify in your transcription request. If the languages don\'t
-- match, the custom language model isn\'t applied. There are no errors or
-- warnings associated with a language mismatch.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-language-models.html Custom language models>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeProcessorConfiguration_languageModelName :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe Prelude.Text)
amazonTranscribeProcessorConfiguration_languageModelName = Lens.lens (\AmazonTranscribeProcessorConfiguration' {languageModelName} -> languageModelName) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {languageModelName = a} :: AmazonTranscribeProcessorConfiguration)

-- | The level of stability to use when you enable partial results
-- stabilization (@EnablePartialResultsStabilization@).
--
-- Low stability provides the highest accuracy. High stability transcribes
-- faster, but with slightly lower accuracy.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeProcessorConfiguration_partialResultsStability :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe PartialResultsStability)
amazonTranscribeProcessorConfiguration_partialResultsStability = Lens.lens (\AmazonTranscribeProcessorConfiguration' {partialResultsStability} -> partialResultsStability) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {partialResultsStability = a} :: AmazonTranscribeProcessorConfiguration)

-- | The types of personally identifiable information (PII) to redact from a
-- transcript. You can include as many types as you\'d like, or you can
-- select @ALL@.
--
-- To include @PiiEntityTypes@ in your Call Analytics request, you must
-- also include @ContentIdentificationType@ or @ContentRedactionType@, but
-- you can\'t include both.
--
-- Values must be comma-separated and can include: @ADDRESS@,
-- @BANK_ACCOUNT_NUMBER@, @BANK_ROUTING@, @CREDIT_DEBIT_CVV@,
-- @CREDIT_DEBIT_EXPIRY@, @CREDIT_DEBIT_NUMBER@, @EMAIL@, @NAME@, @PHONE@,
-- @PIN@, @SSN@, or @ALL@.
--
-- If you leave this parameter empty, the default behavior is equivalent to
-- @ALL@.
amazonTranscribeProcessorConfiguration_piiEntityTypes :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe Prelude.Text)
amazonTranscribeProcessorConfiguration_piiEntityTypes = Lens.lens (\AmazonTranscribeProcessorConfiguration' {piiEntityTypes} -> piiEntityTypes) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {piiEntityTypes = a} :: AmazonTranscribeProcessorConfiguration)

-- | Enables speaker partitioning (diarization) in your transcription output.
-- Speaker partitioning labels the speech from individual speakers in your
-- media file.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/diarization.html Partitioning speakers (diarization)>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeProcessorConfiguration_showSpeakerLabel :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe Prelude.Bool)
amazonTranscribeProcessorConfiguration_showSpeakerLabel = Lens.lens (\AmazonTranscribeProcessorConfiguration' {showSpeakerLabel} -> showSpeakerLabel) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {showSpeakerLabel = a} :: AmazonTranscribeProcessorConfiguration)

-- | The vocabulary filtering method used in your Call Analytics
-- transcription.
amazonTranscribeProcessorConfiguration_vocabularyFilterMethod :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe VocabularyFilterMethod)
amazonTranscribeProcessorConfiguration_vocabularyFilterMethod = Lens.lens (\AmazonTranscribeProcessorConfiguration' {vocabularyFilterMethod} -> vocabularyFilterMethod) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {vocabularyFilterMethod = a} :: AmazonTranscribeProcessorConfiguration)

-- | The name of the custom vocabulary filter that you specified in your Call
-- Analytics request.
--
-- Length Constraints: Minimum length of 1. Maximum length of 200.
amazonTranscribeProcessorConfiguration_vocabularyFilterName :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe Prelude.Text)
amazonTranscribeProcessorConfiguration_vocabularyFilterName = Lens.lens (\AmazonTranscribeProcessorConfiguration' {vocabularyFilterName} -> vocabularyFilterName) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {vocabularyFilterName = a} :: AmazonTranscribeProcessorConfiguration)

-- | The name of the custom vocabulary that you specified in your Call
-- Analytics request.
--
-- Length Constraints: Minimum length of 1. Maximum length of 200.
amazonTranscribeProcessorConfiguration_vocabularyName :: Lens.Lens' AmazonTranscribeProcessorConfiguration (Prelude.Maybe Prelude.Text)
amazonTranscribeProcessorConfiguration_vocabularyName = Lens.lens (\AmazonTranscribeProcessorConfiguration' {vocabularyName} -> vocabularyName) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {vocabularyName = a} :: AmazonTranscribeProcessorConfiguration)

-- | The language code that represents the language spoken in your audio.
--
-- If you\'re unsure of the language spoken in your audio, consider using
-- @IdentifyLanguage@ to enable automatic language identification.
--
-- For a list of languages that real-time Call Analytics supports, see the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages table>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeProcessorConfiguration_languageCode :: Lens.Lens' AmazonTranscribeProcessorConfiguration CallAnalyticsLanguageCode
amazonTranscribeProcessorConfiguration_languageCode = Lens.lens (\AmazonTranscribeProcessorConfiguration' {languageCode} -> languageCode) (\s@AmazonTranscribeProcessorConfiguration' {} a -> s {languageCode = a} :: AmazonTranscribeProcessorConfiguration)

instance
  Data.FromJSON
    AmazonTranscribeProcessorConfiguration
  where
  parseJSON =
    Data.withObject
      "AmazonTranscribeProcessorConfiguration"
      ( \x ->
          AmazonTranscribeProcessorConfiguration'
            Prelude.<$> (x Data..:? "ContentIdentificationType")
            Prelude.<*> (x Data..:? "ContentRedactionType")
            Prelude.<*> (x Data..:? "EnablePartialResultsStabilization")
            Prelude.<*> (x Data..:? "FilterPartialResults")
            Prelude.<*> (x Data..:? "LanguageModelName")
            Prelude.<*> (x Data..:? "PartialResultsStability")
            Prelude.<*> (x Data..:? "PiiEntityTypes")
            Prelude.<*> (x Data..:? "ShowSpeakerLabel")
            Prelude.<*> (x Data..:? "VocabularyFilterMethod")
            Prelude.<*> (x Data..:? "VocabularyFilterName")
            Prelude.<*> (x Data..:? "VocabularyName")
            Prelude.<*> (x Data..: "LanguageCode")
      )

instance
  Prelude.Hashable
    AmazonTranscribeProcessorConfiguration
  where
  hashWithSalt
    _salt
    AmazonTranscribeProcessorConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` contentIdentificationType
        `Prelude.hashWithSalt` contentRedactionType
        `Prelude.hashWithSalt` enablePartialResultsStabilization
        `Prelude.hashWithSalt` filterPartialResults
        `Prelude.hashWithSalt` languageModelName
        `Prelude.hashWithSalt` partialResultsStability
        `Prelude.hashWithSalt` piiEntityTypes
        `Prelude.hashWithSalt` showSpeakerLabel
        `Prelude.hashWithSalt` vocabularyFilterMethod
        `Prelude.hashWithSalt` vocabularyFilterName
        `Prelude.hashWithSalt` vocabularyName
        `Prelude.hashWithSalt` languageCode

instance
  Prelude.NFData
    AmazonTranscribeProcessorConfiguration
  where
  rnf AmazonTranscribeProcessorConfiguration' {..} =
    Prelude.rnf contentIdentificationType
      `Prelude.seq` Prelude.rnf contentRedactionType
      `Prelude.seq` Prelude.rnf enablePartialResultsStabilization
      `Prelude.seq` Prelude.rnf filterPartialResults
      `Prelude.seq` Prelude.rnf languageModelName
      `Prelude.seq` Prelude.rnf partialResultsStability
      `Prelude.seq` Prelude.rnf piiEntityTypes
      `Prelude.seq` Prelude.rnf showSpeakerLabel
      `Prelude.seq` Prelude.rnf vocabularyFilterMethod
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf languageCode

instance
  Data.ToJSON
    AmazonTranscribeProcessorConfiguration
  where
  toJSON AmazonTranscribeProcessorConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentIdentificationType" Data..=)
              Prelude.<$> contentIdentificationType,
            ("ContentRedactionType" Data..=)
              Prelude.<$> contentRedactionType,
            ("EnablePartialResultsStabilization" Data..=)
              Prelude.<$> enablePartialResultsStabilization,
            ("FilterPartialResults" Data..=)
              Prelude.<$> filterPartialResults,
            ("LanguageModelName" Data..=)
              Prelude.<$> languageModelName,
            ("PartialResultsStability" Data..=)
              Prelude.<$> partialResultsStability,
            ("PiiEntityTypes" Data..=)
              Prelude.<$> piiEntityTypes,
            ("ShowSpeakerLabel" Data..=)
              Prelude.<$> showSpeakerLabel,
            ("VocabularyFilterMethod" Data..=)
              Prelude.<$> vocabularyFilterMethod,
            ("VocabularyFilterName" Data..=)
              Prelude.<$> vocabularyFilterName,
            ("VocabularyName" Data..=)
              Prelude.<$> vocabularyName,
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )
