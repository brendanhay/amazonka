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
-- Module      : Amazonka.Transcribe.Types.MedicalTranscriptionJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.MedicalTranscriptionJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.MedicalContentIdentificationType
import Amazonka.Transcribe.Types.OutputLocationType
import Amazonka.Transcribe.Types.Specialty
import Amazonka.Transcribe.Types.TranscriptionJobStatus
import Amazonka.Transcribe.Types.Type

-- | Provides detailed information about a specific medical transcription
-- job.
--
-- /See:/ 'newMedicalTranscriptionJobSummary' smart constructor.
data MedicalTranscriptionJobSummary = MedicalTranscriptionJobSummary'
  { -- | The date and time the specified medical transcription job finished
    -- processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
    -- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | Labels all personal health information (PHI) identified in your
    -- transcript. For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/phi-id.html Identifying personal health information (PHI) in a transcription>.
    contentIdentificationType :: Prelude.Maybe MedicalContentIdentificationType,
    -- | The date and time the specified medical transcription job request was
    -- made.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | If @TranscriptionJobStatus@ is @FAILED@, @FailureReason@ contains
    -- information about why the transcription job failed. See also:
    -- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The language code used to create your medical transcription. US English
    -- (@en-US@) is the only supported language for medical transcriptions.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The name of the medical transcription job. Job names are case sensitive
    -- and must be unique within an Amazon Web Services account.
    medicalTranscriptionJobName :: Prelude.Maybe Prelude.Text,
    -- | Indicates where the specified medical transcription output is stored.
    --
    -- If the value is @CUSTOMER_BUCKET@, the location is the Amazon S3 bucket
    -- you specified using the @OutputBucketName@ parameter in your request. If
    -- you also included @OutputKey@ in your request, your output is located in
    -- the path you specified in your request.
    --
    -- If the value is @SERVICE_BUCKET@, the location is a service-managed
    -- Amazon S3 bucket. To access a transcript stored in a service-managed
    -- bucket, use the URI shown in the @TranscriptFileUri@ field.
    outputLocationType :: Prelude.Maybe OutputLocationType,
    -- | Provides the medical specialty represented in your media.
    specialty :: Prelude.Maybe Specialty,
    -- | The date and time your medical transcription job began processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Provides the status of your medical transcription job.
    --
    -- If the status is @COMPLETED@, the job is finished and you can find the
    -- results at the location specified in @TranscriptFileUri@. If the status
    -- is @FAILED@, @FailureReason@ provides details on why your transcription
    -- job failed.
    transcriptionJobStatus :: Prelude.Maybe TranscriptionJobStatus,
    -- | Indicates whether the input media is a dictation or a conversation, as
    -- specified in the @StartMedicalTranscriptionJob@ request.
    type' :: Prelude.Maybe Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MedicalTranscriptionJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'medicalTranscriptionJobSummary_completionTime' - The date and time the specified medical transcription job finished
-- processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
--
-- 'contentIdentificationType', 'medicalTranscriptionJobSummary_contentIdentificationType' - Labels all personal health information (PHI) identified in your
-- transcript. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/phi-id.html Identifying personal health information (PHI) in a transcription>.
--
-- 'creationTime', 'medicalTranscriptionJobSummary_creationTime' - The date and time the specified medical transcription job request was
-- made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'failureReason', 'medicalTranscriptionJobSummary_failureReason' - If @TranscriptionJobStatus@ is @FAILED@, @FailureReason@ contains
-- information about why the transcription job failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
--
-- 'languageCode', 'medicalTranscriptionJobSummary_languageCode' - The language code used to create your medical transcription. US English
-- (@en-US@) is the only supported language for medical transcriptions.
--
-- 'medicalTranscriptionJobName', 'medicalTranscriptionJobSummary_medicalTranscriptionJobName' - The name of the medical transcription job. Job names are case sensitive
-- and must be unique within an Amazon Web Services account.
--
-- 'outputLocationType', 'medicalTranscriptionJobSummary_outputLocationType' - Indicates where the specified medical transcription output is stored.
--
-- If the value is @CUSTOMER_BUCKET@, the location is the Amazon S3 bucket
-- you specified using the @OutputBucketName@ parameter in your request. If
-- you also included @OutputKey@ in your request, your output is located in
-- the path you specified in your request.
--
-- If the value is @SERVICE_BUCKET@, the location is a service-managed
-- Amazon S3 bucket. To access a transcript stored in a service-managed
-- bucket, use the URI shown in the @TranscriptFileUri@ field.
--
-- 'specialty', 'medicalTranscriptionJobSummary_specialty' - Provides the medical specialty represented in your media.
--
-- 'startTime', 'medicalTranscriptionJobSummary_startTime' - The date and time your medical transcription job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'transcriptionJobStatus', 'medicalTranscriptionJobSummary_transcriptionJobStatus' - Provides the status of your medical transcription job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@. If the status
-- is @FAILED@, @FailureReason@ provides details on why your transcription
-- job failed.
--
-- 'type'', 'medicalTranscriptionJobSummary_type' - Indicates whether the input media is a dictation or a conversation, as
-- specified in the @StartMedicalTranscriptionJob@ request.
newMedicalTranscriptionJobSummary ::
  MedicalTranscriptionJobSummary
newMedicalTranscriptionJobSummary =
  MedicalTranscriptionJobSummary'
    { completionTime =
        Prelude.Nothing,
      contentIdentificationType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      medicalTranscriptionJobName =
        Prelude.Nothing,
      outputLocationType = Prelude.Nothing,
      specialty = Prelude.Nothing,
      startTime = Prelude.Nothing,
      transcriptionJobStatus = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date and time the specified medical transcription job finished
-- processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
medicalTranscriptionJobSummary_completionTime :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJobSummary_completionTime = Lens.lens (\MedicalTranscriptionJobSummary' {completionTime} -> completionTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {completionTime = a} :: MedicalTranscriptionJobSummary) Prelude.. Lens.mapping Data._Time

-- | Labels all personal health information (PHI) identified in your
-- transcript. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/phi-id.html Identifying personal health information (PHI) in a transcription>.
medicalTranscriptionJobSummary_contentIdentificationType :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe MedicalContentIdentificationType)
medicalTranscriptionJobSummary_contentIdentificationType = Lens.lens (\MedicalTranscriptionJobSummary' {contentIdentificationType} -> contentIdentificationType) (\s@MedicalTranscriptionJobSummary' {} a -> s {contentIdentificationType = a} :: MedicalTranscriptionJobSummary)

-- | The date and time the specified medical transcription job request was
-- made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
medicalTranscriptionJobSummary_creationTime :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJobSummary_creationTime = Lens.lens (\MedicalTranscriptionJobSummary' {creationTime} -> creationTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {creationTime = a} :: MedicalTranscriptionJobSummary) Prelude.. Lens.mapping Data._Time

-- | If @TranscriptionJobStatus@ is @FAILED@, @FailureReason@ contains
-- information about why the transcription job failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
medicalTranscriptionJobSummary_failureReason :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.Text)
medicalTranscriptionJobSummary_failureReason = Lens.lens (\MedicalTranscriptionJobSummary' {failureReason} -> failureReason) (\s@MedicalTranscriptionJobSummary' {} a -> s {failureReason = a} :: MedicalTranscriptionJobSummary)

-- | The language code used to create your medical transcription. US English
-- (@en-US@) is the only supported language for medical transcriptions.
medicalTranscriptionJobSummary_languageCode :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe LanguageCode)
medicalTranscriptionJobSummary_languageCode = Lens.lens (\MedicalTranscriptionJobSummary' {languageCode} -> languageCode) (\s@MedicalTranscriptionJobSummary' {} a -> s {languageCode = a} :: MedicalTranscriptionJobSummary)

-- | The name of the medical transcription job. Job names are case sensitive
-- and must be unique within an Amazon Web Services account.
medicalTranscriptionJobSummary_medicalTranscriptionJobName :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.Text)
medicalTranscriptionJobSummary_medicalTranscriptionJobName = Lens.lens (\MedicalTranscriptionJobSummary' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@MedicalTranscriptionJobSummary' {} a -> s {medicalTranscriptionJobName = a} :: MedicalTranscriptionJobSummary)

-- | Indicates where the specified medical transcription output is stored.
--
-- If the value is @CUSTOMER_BUCKET@, the location is the Amazon S3 bucket
-- you specified using the @OutputBucketName@ parameter in your request. If
-- you also included @OutputKey@ in your request, your output is located in
-- the path you specified in your request.
--
-- If the value is @SERVICE_BUCKET@, the location is a service-managed
-- Amazon S3 bucket. To access a transcript stored in a service-managed
-- bucket, use the URI shown in the @TranscriptFileUri@ field.
medicalTranscriptionJobSummary_outputLocationType :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe OutputLocationType)
medicalTranscriptionJobSummary_outputLocationType = Lens.lens (\MedicalTranscriptionJobSummary' {outputLocationType} -> outputLocationType) (\s@MedicalTranscriptionJobSummary' {} a -> s {outputLocationType = a} :: MedicalTranscriptionJobSummary)

-- | Provides the medical specialty represented in your media.
medicalTranscriptionJobSummary_specialty :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Specialty)
medicalTranscriptionJobSummary_specialty = Lens.lens (\MedicalTranscriptionJobSummary' {specialty} -> specialty) (\s@MedicalTranscriptionJobSummary' {} a -> s {specialty = a} :: MedicalTranscriptionJobSummary)

-- | The date and time your medical transcription job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
medicalTranscriptionJobSummary_startTime :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJobSummary_startTime = Lens.lens (\MedicalTranscriptionJobSummary' {startTime} -> startTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {startTime = a} :: MedicalTranscriptionJobSummary) Prelude.. Lens.mapping Data._Time

-- | Provides the status of your medical transcription job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@. If the status
-- is @FAILED@, @FailureReason@ provides details on why your transcription
-- job failed.
medicalTranscriptionJobSummary_transcriptionJobStatus :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe TranscriptionJobStatus)
medicalTranscriptionJobSummary_transcriptionJobStatus = Lens.lens (\MedicalTranscriptionJobSummary' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@MedicalTranscriptionJobSummary' {} a -> s {transcriptionJobStatus = a} :: MedicalTranscriptionJobSummary)

-- | Indicates whether the input media is a dictation or a conversation, as
-- specified in the @StartMedicalTranscriptionJob@ request.
medicalTranscriptionJobSummary_type :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Type)
medicalTranscriptionJobSummary_type = Lens.lens (\MedicalTranscriptionJobSummary' {type'} -> type') (\s@MedicalTranscriptionJobSummary' {} a -> s {type' = a} :: MedicalTranscriptionJobSummary)

instance Data.FromJSON MedicalTranscriptionJobSummary where
  parseJSON =
    Data.withObject
      "MedicalTranscriptionJobSummary"
      ( \x ->
          MedicalTranscriptionJobSummary'
            Prelude.<$> (x Data..:? "CompletionTime")
            Prelude.<*> (x Data..:? "ContentIdentificationType")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "MedicalTranscriptionJobName")
            Prelude.<*> (x Data..:? "OutputLocationType")
            Prelude.<*> (x Data..:? "Specialty")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "TranscriptionJobStatus")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    MedicalTranscriptionJobSummary
  where
  hashWithSalt
    _salt
    MedicalTranscriptionJobSummary' {..} =
      _salt
        `Prelude.hashWithSalt` completionTime
        `Prelude.hashWithSalt` contentIdentificationType
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` languageCode
        `Prelude.hashWithSalt` medicalTranscriptionJobName
        `Prelude.hashWithSalt` outputLocationType
        `Prelude.hashWithSalt` specialty
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` transcriptionJobStatus
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    MedicalTranscriptionJobSummary
  where
  rnf MedicalTranscriptionJobSummary' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf contentIdentificationType
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf medicalTranscriptionJobName
      `Prelude.seq` Prelude.rnf outputLocationType
      `Prelude.seq` Prelude.rnf specialty
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf transcriptionJobStatus
      `Prelude.seq` Prelude.rnf type'
