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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.AmazonTranscribeCallAnalyticsProcessorConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.AmazonTranscribeCallAnalyticsProcessorConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.CallAnalyticsLanguageCode
import Amazonka.ChimeSdkMediaPipelines.Types.ContentType
import Amazonka.ChimeSdkMediaPipelines.Types.PartialResultsStability
import Amazonka.ChimeSdkMediaPipelines.Types.PostCallAnalyticsSettings
import Amazonka.ChimeSdkMediaPipelines.Types.VocabularyFilterMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration settings for an Amazon
-- Transcribe call analytics processor.
--
-- /See:/ 'newAmazonTranscribeCallAnalyticsProcessorConfiguration' smart constructor.
data AmazonTranscribeCallAnalyticsProcessorConfiguration = AmazonTranscribeCallAnalyticsProcessorConfiguration'
  { -- | By default, all @CategoryEvents@ are sent to the insights target. If
    -- this parameter is specified, only included categories are sent to the
    -- insights target.
    callAnalyticsStreamCategories :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Labels all personally identifiable information (PII) identified in your
    -- transcript.
    --
    -- Content identification is performed at the segment level; PII specified
    -- in @PiiEntityTypes@ is flagged upon complete transcription of an audio
    -- segment.
    --
    -- You can’t set @ContentIdentificationType@ and @ContentRedactionType@ in
    -- the same request. If you do, your request returns a
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
    -- @PiiEntityTypes@ is redacted upon complete transcription of an audio
    -- segment.
    --
    -- You can’t set @ContentRedactionType@ and @ContentIdentificationType@ in
    -- the same request. If you do, your request returns a
    -- @BadRequestException@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
    -- in the /Amazon Transcribe Developer Guide/.
    contentRedactionType :: Prelude.Maybe ContentType,
    -- | Enables partial result stabilization for your transcription. Partial
    -- result stabilization can reduce latency in your output, but may impact
    -- accuracy. For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
    -- in the /Amazon Transcribe Developer Guide/.
    enablePartialResultsStabilization :: Prelude.Maybe Prelude.Bool,
    -- | If true, @UtteranceEvents@ with @IsPartial: true@ are filtered out of
    -- the insights target.
    filterPartialResults :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the custom language model to use when processing a
    -- transcription. Note that language model names are case sensitive.
    --
    -- The language of the specified language model must match the language
    -- code specified in the transcription request. If the languages don\'t
    -- match, the custom language model isn\'t applied. Language mismatches
    -- don\'t generate errors or warnings.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-language-models.html Custom language models>
    -- in the /Amazon Transcribe Developer Guide/.
    languageModelName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the level of stability to use when you enable partial results
    -- stabilization (@EnablePartialResultsStabilization@).
    --
    -- Low stability provides the highest accuracy. High stability transcribes
    -- faster, but with slightly lower accuracy.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
    -- in the /Amazon Transcribe Developer Guide/.
    partialResultsStability :: Prelude.Maybe PartialResultsStability,
    -- | Specifies the types of personally identifiable information (PII) to
    -- redact from a transcript. You can include as many types as you\'d like,
    -- or you can select @ALL@.
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
    -- Length Constraints: Minimum length of 1. Maximum length of 300.
    piiEntityTypes :: Prelude.Maybe Prelude.Text,
    -- | The settings for a post-call analysis task in an analytics
    -- configuration.
    postCallAnalyticsSettings :: Prelude.Maybe PostCallAnalyticsSettings,
    -- | Specifies how to apply a vocabulary filter to a transcript.
    --
    -- To replace words with __***__, choose @mask@.
    --
    -- To delete words, choose @remove@.
    --
    -- To flag words without changing them, choose @tag@.
    vocabularyFilterMethod :: Prelude.Maybe VocabularyFilterMethod,
    -- | Specifies the name of the custom vocabulary filter to use when
    -- processing a transcription. Note that vocabulary filter names are case
    -- sensitive.
    --
    -- If the language of the specified custom vocabulary filter doesn\'t match
    -- the language identified in your media, the vocabulary filter is not
    -- applied to your transcription.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/vocabulary-filtering.html Using vocabulary filtering with unwanted words>
    -- in the /Amazon Transcribe Developer Guide/.
    --
    -- Length Constraints: Minimum length of 1. Maximum length of 200.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the custom vocabulary to use when processing a
    -- transcription. Note that vocabulary names are case sensitive.
    --
    -- If the language of the specified custom vocabulary doesn\'t match the
    -- language identified in your media, the custom vocabulary is not applied
    -- to your transcription.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-vocabulary.html Custom vocabularies>
    -- in the /Amazon Transcribe Developer Guide/.
    --
    -- Length Constraints: Minimum length of 1. Maximum length of 200.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The language code in the configuration.
    languageCode :: CallAnalyticsLanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonTranscribeCallAnalyticsProcessorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAnalyticsStreamCategories', 'amazonTranscribeCallAnalyticsProcessorConfiguration_callAnalyticsStreamCategories' - By default, all @CategoryEvents@ are sent to the insights target. If
-- this parameter is specified, only included categories are sent to the
-- insights target.
--
-- 'contentIdentificationType', 'amazonTranscribeCallAnalyticsProcessorConfiguration_contentIdentificationType' - Labels all personally identifiable information (PII) identified in your
-- transcript.
--
-- Content identification is performed at the segment level; PII specified
-- in @PiiEntityTypes@ is flagged upon complete transcription of an audio
-- segment.
--
-- You can’t set @ContentIdentificationType@ and @ContentRedactionType@ in
-- the same request. If you do, your request returns a
-- @BadRequestException@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'contentRedactionType', 'amazonTranscribeCallAnalyticsProcessorConfiguration_contentRedactionType' - Redacts all personally identifiable information (PII) identified in your
-- transcript.
--
-- Content redaction is performed at the segment level; PII specified in
-- @PiiEntityTypes@ is redacted upon complete transcription of an audio
-- segment.
--
-- You can’t set @ContentRedactionType@ and @ContentIdentificationType@ in
-- the same request. If you do, your request returns a
-- @BadRequestException@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'enablePartialResultsStabilization', 'amazonTranscribeCallAnalyticsProcessorConfiguration_enablePartialResultsStabilization' - Enables partial result stabilization for your transcription. Partial
-- result stabilization can reduce latency in your output, but may impact
-- accuracy. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'filterPartialResults', 'amazonTranscribeCallAnalyticsProcessorConfiguration_filterPartialResults' - If true, @UtteranceEvents@ with @IsPartial: true@ are filtered out of
-- the insights target.
--
-- 'languageModelName', 'amazonTranscribeCallAnalyticsProcessorConfiguration_languageModelName' - Specifies the name of the custom language model to use when processing a
-- transcription. Note that language model names are case sensitive.
--
-- The language of the specified language model must match the language
-- code specified in the transcription request. If the languages don\'t
-- match, the custom language model isn\'t applied. Language mismatches
-- don\'t generate errors or warnings.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-language-models.html Custom language models>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'partialResultsStability', 'amazonTranscribeCallAnalyticsProcessorConfiguration_partialResultsStability' - Specifies the level of stability to use when you enable partial results
-- stabilization (@EnablePartialResultsStabilization@).
--
-- Low stability provides the highest accuracy. High stability transcribes
-- faster, but with slightly lower accuracy.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'piiEntityTypes', 'amazonTranscribeCallAnalyticsProcessorConfiguration_piiEntityTypes' - Specifies the types of personally identifiable information (PII) to
-- redact from a transcript. You can include as many types as you\'d like,
-- or you can select @ALL@.
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
-- Length Constraints: Minimum length of 1. Maximum length of 300.
--
-- 'postCallAnalyticsSettings', 'amazonTranscribeCallAnalyticsProcessorConfiguration_postCallAnalyticsSettings' - The settings for a post-call analysis task in an analytics
-- configuration.
--
-- 'vocabularyFilterMethod', 'amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyFilterMethod' - Specifies how to apply a vocabulary filter to a transcript.
--
-- To replace words with __***__, choose @mask@.
--
-- To delete words, choose @remove@.
--
-- To flag words without changing them, choose @tag@.
--
-- 'vocabularyFilterName', 'amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyFilterName' - Specifies the name of the custom vocabulary filter to use when
-- processing a transcription. Note that vocabulary filter names are case
-- sensitive.
--
-- If the language of the specified custom vocabulary filter doesn\'t match
-- the language identified in your media, the vocabulary filter is not
-- applied to your transcription.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/vocabulary-filtering.html Using vocabulary filtering with unwanted words>
-- in the /Amazon Transcribe Developer Guide/.
--
-- Length Constraints: Minimum length of 1. Maximum length of 200.
--
-- 'vocabularyName', 'amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyName' - Specifies the name of the custom vocabulary to use when processing a
-- transcription. Note that vocabulary names are case sensitive.
--
-- If the language of the specified custom vocabulary doesn\'t match the
-- language identified in your media, the custom vocabulary is not applied
-- to your transcription.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-vocabulary.html Custom vocabularies>
-- in the /Amazon Transcribe Developer Guide/.
--
-- Length Constraints: Minimum length of 1. Maximum length of 200.
--
-- 'languageCode', 'amazonTranscribeCallAnalyticsProcessorConfiguration_languageCode' - The language code in the configuration.
newAmazonTranscribeCallAnalyticsProcessorConfiguration ::
  -- | 'languageCode'
  CallAnalyticsLanguageCode ->
  AmazonTranscribeCallAnalyticsProcessorConfiguration
newAmazonTranscribeCallAnalyticsProcessorConfiguration
  pLanguageCode_ =
    AmazonTranscribeCallAnalyticsProcessorConfiguration'
      { callAnalyticsStreamCategories =
          Prelude.Nothing,
        contentIdentificationType =
          Prelude.Nothing,
        contentRedactionType =
          Prelude.Nothing,
        enablePartialResultsStabilization =
          Prelude.Nothing,
        filterPartialResults =
          Prelude.Nothing,
        languageModelName =
          Prelude.Nothing,
        partialResultsStability =
          Prelude.Nothing,
        piiEntityTypes =
          Prelude.Nothing,
        postCallAnalyticsSettings =
          Prelude.Nothing,
        vocabularyFilterMethod =
          Prelude.Nothing,
        vocabularyFilterName =
          Prelude.Nothing,
        vocabularyName =
          Prelude.Nothing,
        languageCode =
          pLanguageCode_
      }

-- | By default, all @CategoryEvents@ are sent to the insights target. If
-- this parameter is specified, only included categories are sent to the
-- insights target.
amazonTranscribeCallAnalyticsProcessorConfiguration_callAnalyticsStreamCategories :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
amazonTranscribeCallAnalyticsProcessorConfiguration_callAnalyticsStreamCategories = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {callAnalyticsStreamCategories} -> callAnalyticsStreamCategories) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {callAnalyticsStreamCategories = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Labels all personally identifiable information (PII) identified in your
-- transcript.
--
-- Content identification is performed at the segment level; PII specified
-- in @PiiEntityTypes@ is flagged upon complete transcription of an audio
-- segment.
--
-- You can’t set @ContentIdentificationType@ and @ContentRedactionType@ in
-- the same request. If you do, your request returns a
-- @BadRequestException@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeCallAnalyticsProcessorConfiguration_contentIdentificationType :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe ContentType)
amazonTranscribeCallAnalyticsProcessorConfiguration_contentIdentificationType = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {contentIdentificationType} -> contentIdentificationType) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {contentIdentificationType = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | Redacts all personally identifiable information (PII) identified in your
-- transcript.
--
-- Content redaction is performed at the segment level; PII specified in
-- @PiiEntityTypes@ is redacted upon complete transcription of an audio
-- segment.
--
-- You can’t set @ContentRedactionType@ and @ContentIdentificationType@ in
-- the same request. If you do, your request returns a
-- @BadRequestException@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/pii-redaction.html Redacting or identifying personally identifiable information>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeCallAnalyticsProcessorConfiguration_contentRedactionType :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe ContentType)
amazonTranscribeCallAnalyticsProcessorConfiguration_contentRedactionType = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {contentRedactionType} -> contentRedactionType) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {contentRedactionType = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | Enables partial result stabilization for your transcription. Partial
-- result stabilization can reduce latency in your output, but may impact
-- accuracy. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeCallAnalyticsProcessorConfiguration_enablePartialResultsStabilization :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe Prelude.Bool)
amazonTranscribeCallAnalyticsProcessorConfiguration_enablePartialResultsStabilization = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {enablePartialResultsStabilization} -> enablePartialResultsStabilization) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {enablePartialResultsStabilization = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | If true, @UtteranceEvents@ with @IsPartial: true@ are filtered out of
-- the insights target.
amazonTranscribeCallAnalyticsProcessorConfiguration_filterPartialResults :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe Prelude.Bool)
amazonTranscribeCallAnalyticsProcessorConfiguration_filterPartialResults = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {filterPartialResults} -> filterPartialResults) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {filterPartialResults = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | Specifies the name of the custom language model to use when processing a
-- transcription. Note that language model names are case sensitive.
--
-- The language of the specified language model must match the language
-- code specified in the transcription request. If the languages don\'t
-- match, the custom language model isn\'t applied. Language mismatches
-- don\'t generate errors or warnings.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-language-models.html Custom language models>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeCallAnalyticsProcessorConfiguration_languageModelName :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe Prelude.Text)
amazonTranscribeCallAnalyticsProcessorConfiguration_languageModelName = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {languageModelName} -> languageModelName) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {languageModelName = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | Specifies the level of stability to use when you enable partial results
-- stabilization (@EnablePartialResultsStabilization@).
--
-- Low stability provides the highest accuracy. High stability transcribes
-- faster, but with slightly lower accuracy.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/streaming.html#streaming-partial-result-stabilization Partial-result stabilization>
-- in the /Amazon Transcribe Developer Guide/.
amazonTranscribeCallAnalyticsProcessorConfiguration_partialResultsStability :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe PartialResultsStability)
amazonTranscribeCallAnalyticsProcessorConfiguration_partialResultsStability = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {partialResultsStability} -> partialResultsStability) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {partialResultsStability = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | Specifies the types of personally identifiable information (PII) to
-- redact from a transcript. You can include as many types as you\'d like,
-- or you can select @ALL@.
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
-- Length Constraints: Minimum length of 1. Maximum length of 300.
amazonTranscribeCallAnalyticsProcessorConfiguration_piiEntityTypes :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe Prelude.Text)
amazonTranscribeCallAnalyticsProcessorConfiguration_piiEntityTypes = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {piiEntityTypes} -> piiEntityTypes) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {piiEntityTypes = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | The settings for a post-call analysis task in an analytics
-- configuration.
amazonTranscribeCallAnalyticsProcessorConfiguration_postCallAnalyticsSettings :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe PostCallAnalyticsSettings)
amazonTranscribeCallAnalyticsProcessorConfiguration_postCallAnalyticsSettings = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {postCallAnalyticsSettings} -> postCallAnalyticsSettings) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {postCallAnalyticsSettings = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | Specifies how to apply a vocabulary filter to a transcript.
--
-- To replace words with __***__, choose @mask@.
--
-- To delete words, choose @remove@.
--
-- To flag words without changing them, choose @tag@.
amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyFilterMethod :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe VocabularyFilterMethod)
amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyFilterMethod = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {vocabularyFilterMethod} -> vocabularyFilterMethod) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {vocabularyFilterMethod = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | Specifies the name of the custom vocabulary filter to use when
-- processing a transcription. Note that vocabulary filter names are case
-- sensitive.
--
-- If the language of the specified custom vocabulary filter doesn\'t match
-- the language identified in your media, the vocabulary filter is not
-- applied to your transcription.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/vocabulary-filtering.html Using vocabulary filtering with unwanted words>
-- in the /Amazon Transcribe Developer Guide/.
--
-- Length Constraints: Minimum length of 1. Maximum length of 200.
amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyFilterName :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe Prelude.Text)
amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyFilterName = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {vocabularyFilterName} -> vocabularyFilterName) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {vocabularyFilterName = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | Specifies the name of the custom vocabulary to use when processing a
-- transcription. Note that vocabulary names are case sensitive.
--
-- If the language of the specified custom vocabulary doesn\'t match the
-- language identified in your media, the custom vocabulary is not applied
-- to your transcription.
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-vocabulary.html Custom vocabularies>
-- in the /Amazon Transcribe Developer Guide/.
--
-- Length Constraints: Minimum length of 1. Maximum length of 200.
amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyName :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration (Prelude.Maybe Prelude.Text)
amazonTranscribeCallAnalyticsProcessorConfiguration_vocabularyName = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {vocabularyName} -> vocabularyName) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {vocabularyName = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

-- | The language code in the configuration.
amazonTranscribeCallAnalyticsProcessorConfiguration_languageCode :: Lens.Lens' AmazonTranscribeCallAnalyticsProcessorConfiguration CallAnalyticsLanguageCode
amazonTranscribeCallAnalyticsProcessorConfiguration_languageCode = Lens.lens (\AmazonTranscribeCallAnalyticsProcessorConfiguration' {languageCode} -> languageCode) (\s@AmazonTranscribeCallAnalyticsProcessorConfiguration' {} a -> s {languageCode = a} :: AmazonTranscribeCallAnalyticsProcessorConfiguration)

instance
  Data.FromJSON
    AmazonTranscribeCallAnalyticsProcessorConfiguration
  where
  parseJSON =
    Data.withObject
      "AmazonTranscribeCallAnalyticsProcessorConfiguration"
      ( \x ->
          AmazonTranscribeCallAnalyticsProcessorConfiguration'
            Prelude.<$> (x Data..:? "CallAnalyticsStreamCategories")
            Prelude.<*> (x Data..:? "ContentIdentificationType")
            Prelude.<*> (x Data..:? "ContentRedactionType")
            Prelude.<*> (x Data..:? "EnablePartialResultsStabilization")
            Prelude.<*> (x Data..:? "FilterPartialResults")
            Prelude.<*> (x Data..:? "LanguageModelName")
            Prelude.<*> (x Data..:? "PartialResultsStability")
            Prelude.<*> (x Data..:? "PiiEntityTypes")
            Prelude.<*> (x Data..:? "PostCallAnalyticsSettings")
            Prelude.<*> (x Data..:? "VocabularyFilterMethod")
            Prelude.<*> (x Data..:? "VocabularyFilterName")
            Prelude.<*> (x Data..:? "VocabularyName")
            Prelude.<*> (x Data..: "LanguageCode")
      )

instance
  Prelude.Hashable
    AmazonTranscribeCallAnalyticsProcessorConfiguration
  where
  hashWithSalt
    _salt
    AmazonTranscribeCallAnalyticsProcessorConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` callAnalyticsStreamCategories
        `Prelude.hashWithSalt` contentIdentificationType
        `Prelude.hashWithSalt` contentRedactionType
        `Prelude.hashWithSalt` enablePartialResultsStabilization
        `Prelude.hashWithSalt` filterPartialResults
        `Prelude.hashWithSalt` languageModelName
        `Prelude.hashWithSalt` partialResultsStability
        `Prelude.hashWithSalt` piiEntityTypes
        `Prelude.hashWithSalt` postCallAnalyticsSettings
        `Prelude.hashWithSalt` vocabularyFilterMethod
        `Prelude.hashWithSalt` vocabularyFilterName
        `Prelude.hashWithSalt` vocabularyName
        `Prelude.hashWithSalt` languageCode

instance
  Prelude.NFData
    AmazonTranscribeCallAnalyticsProcessorConfiguration
  where
  rnf
    AmazonTranscribeCallAnalyticsProcessorConfiguration' {..} =
      Prelude.rnf callAnalyticsStreamCategories
        `Prelude.seq` Prelude.rnf contentIdentificationType
        `Prelude.seq` Prelude.rnf contentRedactionType
        `Prelude.seq` Prelude.rnf enablePartialResultsStabilization
        `Prelude.seq` Prelude.rnf filterPartialResults
        `Prelude.seq` Prelude.rnf languageModelName
        `Prelude.seq` Prelude.rnf partialResultsStability
        `Prelude.seq` Prelude.rnf piiEntityTypes
        `Prelude.seq` Prelude.rnf postCallAnalyticsSettings
        `Prelude.seq` Prelude.rnf vocabularyFilterMethod
        `Prelude.seq` Prelude.rnf vocabularyFilterName
        `Prelude.seq` Prelude.rnf vocabularyName
        `Prelude.seq` Prelude.rnf languageCode

instance
  Data.ToJSON
    AmazonTranscribeCallAnalyticsProcessorConfiguration
  where
  toJSON
    AmazonTranscribeCallAnalyticsProcessorConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("CallAnalyticsStreamCategories" Data..=)
                Prelude.<$> callAnalyticsStreamCategories,
              ("ContentIdentificationType" Data..=)
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
              ("PostCallAnalyticsSettings" Data..=)
                Prelude.<$> postCallAnalyticsSettings,
              ("VocabularyFilterMethod" Data..=)
                Prelude.<$> vocabularyFilterMethod,
              ("VocabularyFilterName" Data..=)
                Prelude.<$> vocabularyFilterName,
              ("VocabularyName" Data..=)
                Prelude.<$> vocabularyName,
              Prelude.Just ("LanguageCode" Data..= languageCode)
            ]
        )
