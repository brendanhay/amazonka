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
-- Module      : Amazonka.Transcribe.Types.TranscriptionJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.TranscriptionJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.ContentRedaction
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.LanguageCodeItem
import Amazonka.Transcribe.Types.ModelSettings
import Amazonka.Transcribe.Types.OutputLocationType
import Amazonka.Transcribe.Types.TranscriptionJobStatus

-- | Provides detailed information about a specific transcription job.
--
-- /See:/ 'newTranscriptionJobSummary' smart constructor.
data TranscriptionJobSummary = TranscriptionJobSummary'
  { -- | The date and time the specified transcription job finished processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
    -- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | The content redaction settings of the transcription job.
    contentRedaction :: Prelude.Maybe ContentRedaction,
    -- | The date and time the specified transcription job request was made.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | If @TranscriptionJobStatus@ is @FAILED@, @FailureReason@ contains
    -- information about why the transcription job failed. See also:
    -- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The confidence score associated with the language identified in your
    -- media file.
    --
    -- Confidence scores are values between 0 and 1; a larger value indicates a
    -- higher probability that the identified language correctly matches the
    -- language spoken in your media.
    identifiedLanguageScore :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether automatic language identification was enabled (@TRUE@)
    -- for the specified transcription job.
    identifyLanguage :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether automatic multi-language identification was enabled
    -- (@TRUE@) for the specified transcription job.
    identifyMultipleLanguages :: Prelude.Maybe Prelude.Bool,
    -- | The language code used to create your transcription.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The language codes used to create your transcription job. This parameter
    -- is used with multi-language identification. For single-language
    -- identification, the singular version of this parameter, @LanguageCode@,
    -- is present.
    languageCodes :: Prelude.Maybe [LanguageCodeItem],
    modelSettings :: Prelude.Maybe ModelSettings,
    -- | Indicates where the specified transcription output is stored.
    --
    -- If the value is @CUSTOMER_BUCKET@, the location is the Amazon S3 bucket
    -- you specified using the @OutputBucketName@ parameter in your request. If
    -- you also included @OutputKey@ in your request, your output is located in
    -- the path you specified in your request.
    --
    -- If the value is @SERVICE_BUCKET@, the location is a service-managed
    -- Amazon S3 bucket. To access a transcript stored in a service-managed
    -- bucket, use the URI shown in the @TranscriptFileUri@ or
    -- @RedactedTranscriptFileUri@ field.
    outputLocationType :: Prelude.Maybe OutputLocationType,
    -- | The date and time your transcription job began processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the transcription job. Job names are case sensitive and must
    -- be unique within an Amazon Web Services account.
    transcriptionJobName :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of your transcription job.
    --
    -- If the status is @COMPLETED@, the job is finished and you can find the
    -- results at the location specified in @TranscriptFileUri@ (or
    -- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
    -- the status is @FAILED@, @FailureReason@ provides details on why your
    -- transcription job failed.
    transcriptionJobStatus :: Prelude.Maybe TranscriptionJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranscriptionJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'transcriptionJobSummary_completionTime' - The date and time the specified transcription job finished processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
--
-- 'contentRedaction', 'transcriptionJobSummary_contentRedaction' - The content redaction settings of the transcription job.
--
-- 'creationTime', 'transcriptionJobSummary_creationTime' - The date and time the specified transcription job request was made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'failureReason', 'transcriptionJobSummary_failureReason' - If @TranscriptionJobStatus@ is @FAILED@, @FailureReason@ contains
-- information about why the transcription job failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
--
-- 'identifiedLanguageScore', 'transcriptionJobSummary_identifiedLanguageScore' - The confidence score associated with the language identified in your
-- media file.
--
-- Confidence scores are values between 0 and 1; a larger value indicates a
-- higher probability that the identified language correctly matches the
-- language spoken in your media.
--
-- 'identifyLanguage', 'transcriptionJobSummary_identifyLanguage' - Indicates whether automatic language identification was enabled (@TRUE@)
-- for the specified transcription job.
--
-- 'identifyMultipleLanguages', 'transcriptionJobSummary_identifyMultipleLanguages' - Indicates whether automatic multi-language identification was enabled
-- (@TRUE@) for the specified transcription job.
--
-- 'languageCode', 'transcriptionJobSummary_languageCode' - The language code used to create your transcription.
--
-- 'languageCodes', 'transcriptionJobSummary_languageCodes' - The language codes used to create your transcription job. This parameter
-- is used with multi-language identification. For single-language
-- identification, the singular version of this parameter, @LanguageCode@,
-- is present.
--
-- 'modelSettings', 'transcriptionJobSummary_modelSettings' - Undocumented member.
--
-- 'outputLocationType', 'transcriptionJobSummary_outputLocationType' - Indicates where the specified transcription output is stored.
--
-- If the value is @CUSTOMER_BUCKET@, the location is the Amazon S3 bucket
-- you specified using the @OutputBucketName@ parameter in your request. If
-- you also included @OutputKey@ in your request, your output is located in
-- the path you specified in your request.
--
-- If the value is @SERVICE_BUCKET@, the location is a service-managed
-- Amazon S3 bucket. To access a transcript stored in a service-managed
-- bucket, use the URI shown in the @TranscriptFileUri@ or
-- @RedactedTranscriptFileUri@ field.
--
-- 'startTime', 'transcriptionJobSummary_startTime' - The date and time your transcription job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'transcriptionJobName', 'transcriptionJobSummary_transcriptionJobName' - The name of the transcription job. Job names are case sensitive and must
-- be unique within an Amazon Web Services account.
--
-- 'transcriptionJobStatus', 'transcriptionJobSummary_transcriptionJobStatus' - Provides the status of your transcription job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@ (or
-- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
-- the status is @FAILED@, @FailureReason@ provides details on why your
-- transcription job failed.
newTranscriptionJobSummary ::
  TranscriptionJobSummary
newTranscriptionJobSummary =
  TranscriptionJobSummary'
    { completionTime =
        Prelude.Nothing,
      contentRedaction = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      identifiedLanguageScore = Prelude.Nothing,
      identifyLanguage = Prelude.Nothing,
      identifyMultipleLanguages = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      languageCodes = Prelude.Nothing,
      modelSettings = Prelude.Nothing,
      outputLocationType = Prelude.Nothing,
      startTime = Prelude.Nothing,
      transcriptionJobName = Prelude.Nothing,
      transcriptionJobStatus = Prelude.Nothing
    }

-- | The date and time the specified transcription job finished processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
transcriptionJobSummary_completionTime :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
transcriptionJobSummary_completionTime = Lens.lens (\TranscriptionJobSummary' {completionTime} -> completionTime) (\s@TranscriptionJobSummary' {} a -> s {completionTime = a} :: TranscriptionJobSummary) Prelude.. Lens.mapping Data._Time

-- | The content redaction settings of the transcription job.
transcriptionJobSummary_contentRedaction :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe ContentRedaction)
transcriptionJobSummary_contentRedaction = Lens.lens (\TranscriptionJobSummary' {contentRedaction} -> contentRedaction) (\s@TranscriptionJobSummary' {} a -> s {contentRedaction = a} :: TranscriptionJobSummary)

-- | The date and time the specified transcription job request was made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
transcriptionJobSummary_creationTime :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
transcriptionJobSummary_creationTime = Lens.lens (\TranscriptionJobSummary' {creationTime} -> creationTime) (\s@TranscriptionJobSummary' {} a -> s {creationTime = a} :: TranscriptionJobSummary) Prelude.. Lens.mapping Data._Time

-- | If @TranscriptionJobStatus@ is @FAILED@, @FailureReason@ contains
-- information about why the transcription job failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
transcriptionJobSummary_failureReason :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Text)
transcriptionJobSummary_failureReason = Lens.lens (\TranscriptionJobSummary' {failureReason} -> failureReason) (\s@TranscriptionJobSummary' {} a -> s {failureReason = a} :: TranscriptionJobSummary)

-- | The confidence score associated with the language identified in your
-- media file.
--
-- Confidence scores are values between 0 and 1; a larger value indicates a
-- higher probability that the identified language correctly matches the
-- language spoken in your media.
transcriptionJobSummary_identifiedLanguageScore :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Double)
transcriptionJobSummary_identifiedLanguageScore = Lens.lens (\TranscriptionJobSummary' {identifiedLanguageScore} -> identifiedLanguageScore) (\s@TranscriptionJobSummary' {} a -> s {identifiedLanguageScore = a} :: TranscriptionJobSummary)

-- | Indicates whether automatic language identification was enabled (@TRUE@)
-- for the specified transcription job.
transcriptionJobSummary_identifyLanguage :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Bool)
transcriptionJobSummary_identifyLanguage = Lens.lens (\TranscriptionJobSummary' {identifyLanguage} -> identifyLanguage) (\s@TranscriptionJobSummary' {} a -> s {identifyLanguage = a} :: TranscriptionJobSummary)

-- | Indicates whether automatic multi-language identification was enabled
-- (@TRUE@) for the specified transcription job.
transcriptionJobSummary_identifyMultipleLanguages :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Bool)
transcriptionJobSummary_identifyMultipleLanguages = Lens.lens (\TranscriptionJobSummary' {identifyMultipleLanguages} -> identifyMultipleLanguages) (\s@TranscriptionJobSummary' {} a -> s {identifyMultipleLanguages = a} :: TranscriptionJobSummary)

-- | The language code used to create your transcription.
transcriptionJobSummary_languageCode :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe LanguageCode)
transcriptionJobSummary_languageCode = Lens.lens (\TranscriptionJobSummary' {languageCode} -> languageCode) (\s@TranscriptionJobSummary' {} a -> s {languageCode = a} :: TranscriptionJobSummary)

-- | The language codes used to create your transcription job. This parameter
-- is used with multi-language identification. For single-language
-- identification, the singular version of this parameter, @LanguageCode@,
-- is present.
transcriptionJobSummary_languageCodes :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe [LanguageCodeItem])
transcriptionJobSummary_languageCodes = Lens.lens (\TranscriptionJobSummary' {languageCodes} -> languageCodes) (\s@TranscriptionJobSummary' {} a -> s {languageCodes = a} :: TranscriptionJobSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
transcriptionJobSummary_modelSettings :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe ModelSettings)
transcriptionJobSummary_modelSettings = Lens.lens (\TranscriptionJobSummary' {modelSettings} -> modelSettings) (\s@TranscriptionJobSummary' {} a -> s {modelSettings = a} :: TranscriptionJobSummary)

-- | Indicates where the specified transcription output is stored.
--
-- If the value is @CUSTOMER_BUCKET@, the location is the Amazon S3 bucket
-- you specified using the @OutputBucketName@ parameter in your request. If
-- you also included @OutputKey@ in your request, your output is located in
-- the path you specified in your request.
--
-- If the value is @SERVICE_BUCKET@, the location is a service-managed
-- Amazon S3 bucket. To access a transcript stored in a service-managed
-- bucket, use the URI shown in the @TranscriptFileUri@ or
-- @RedactedTranscriptFileUri@ field.
transcriptionJobSummary_outputLocationType :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe OutputLocationType)
transcriptionJobSummary_outputLocationType = Lens.lens (\TranscriptionJobSummary' {outputLocationType} -> outputLocationType) (\s@TranscriptionJobSummary' {} a -> s {outputLocationType = a} :: TranscriptionJobSummary)

-- | The date and time your transcription job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
transcriptionJobSummary_startTime :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
transcriptionJobSummary_startTime = Lens.lens (\TranscriptionJobSummary' {startTime} -> startTime) (\s@TranscriptionJobSummary' {} a -> s {startTime = a} :: TranscriptionJobSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the transcription job. Job names are case sensitive and must
-- be unique within an Amazon Web Services account.
transcriptionJobSummary_transcriptionJobName :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Text)
transcriptionJobSummary_transcriptionJobName = Lens.lens (\TranscriptionJobSummary' {transcriptionJobName} -> transcriptionJobName) (\s@TranscriptionJobSummary' {} a -> s {transcriptionJobName = a} :: TranscriptionJobSummary)

-- | Provides the status of your transcription job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@ (or
-- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
-- the status is @FAILED@, @FailureReason@ provides details on why your
-- transcription job failed.
transcriptionJobSummary_transcriptionJobStatus :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe TranscriptionJobStatus)
transcriptionJobSummary_transcriptionJobStatus = Lens.lens (\TranscriptionJobSummary' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@TranscriptionJobSummary' {} a -> s {transcriptionJobStatus = a} :: TranscriptionJobSummary)

instance Data.FromJSON TranscriptionJobSummary where
  parseJSON =
    Data.withObject
      "TranscriptionJobSummary"
      ( \x ->
          TranscriptionJobSummary'
            Prelude.<$> (x Data..:? "CompletionTime")
            Prelude.<*> (x Data..:? "ContentRedaction")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "IdentifiedLanguageScore")
            Prelude.<*> (x Data..:? "IdentifyLanguage")
            Prelude.<*> (x Data..:? "IdentifyMultipleLanguages")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "LanguageCodes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ModelSettings")
            Prelude.<*> (x Data..:? "OutputLocationType")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "TranscriptionJobName")
            Prelude.<*> (x Data..:? "TranscriptionJobStatus")
      )

instance Prelude.Hashable TranscriptionJobSummary where
  hashWithSalt _salt TranscriptionJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` contentRedaction
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` identifiedLanguageScore
      `Prelude.hashWithSalt` identifyLanguage
      `Prelude.hashWithSalt` identifyMultipleLanguages
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` languageCodes
      `Prelude.hashWithSalt` modelSettings
      `Prelude.hashWithSalt` outputLocationType
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` transcriptionJobName
      `Prelude.hashWithSalt` transcriptionJobStatus

instance Prelude.NFData TranscriptionJobSummary where
  rnf TranscriptionJobSummary' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf contentRedaction
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf identifiedLanguageScore
      `Prelude.seq` Prelude.rnf identifyLanguage
      `Prelude.seq` Prelude.rnf identifyMultipleLanguages
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf languageCodes
      `Prelude.seq` Prelude.rnf modelSettings
      `Prelude.seq` Prelude.rnf outputLocationType
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf transcriptionJobName
      `Prelude.seq` Prelude.rnf transcriptionJobStatus
