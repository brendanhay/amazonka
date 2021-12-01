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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.MedicalTranscriptionJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.MedicalContentIdentificationType
import Amazonka.Transcribe.Types.OutputLocationType
import Amazonka.Transcribe.Types.Specialty
import Amazonka.Transcribe.Types.TranscriptionJobStatus
import Amazonka.Transcribe.Types.Type

-- | Provides summary information about a transcription job.
--
-- /See:/ 'newMedicalTranscriptionJobSummary' smart constructor.
data MedicalTranscriptionJobSummary = MedicalTranscriptionJobSummary'
  { -- | A timestamp that shows when the medical transcription job was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The medical specialty of the transcription job. Refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-medical-conversation.html Transcribing a medical conversation>for
    -- a list of supported specialties.
    specialty :: Prelude.Maybe Specialty,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
    -- error.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The language of the transcript in the source audio file.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Indicates the location of the transcription job\'s output. This field
    -- must be the path of an S3 bucket; if you don\'t already have an S3
    -- bucket, one is created based on the path you add.
    outputLocationType :: Prelude.Maybe OutputLocationType,
    -- | A timestamp that shows when the job began processing.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | The name of a medical transcription job.
    medicalTranscriptionJobName :: Prelude.Maybe Prelude.Text,
    -- | The status of the medical transcription job.
    transcriptionJobStatus :: Prelude.Maybe TranscriptionJobStatus,
    -- | The speech of the clinician in the input audio.
    type' :: Prelude.Maybe Type,
    -- | Shows the type of information you\'ve configured Amazon Transcribe
    -- Medical to identify in a transcription job. If the value is @PHI@,
    -- you\'ve configured the transcription job to identify personal health
    -- information (PHI).
    contentIdentificationType :: Prelude.Maybe MedicalContentIdentificationType
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
-- 'creationTime', 'medicalTranscriptionJobSummary_creationTime' - A timestamp that shows when the medical transcription job was created.
--
-- 'specialty', 'medicalTranscriptionJobSummary_specialty' - The medical specialty of the transcription job. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-medical-conversation.html Transcribing a medical conversation>for
-- a list of supported specialties.
--
-- 'failureReason', 'medicalTranscriptionJobSummary_failureReason' - If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
-- error.
--
-- 'languageCode', 'medicalTranscriptionJobSummary_languageCode' - The language of the transcript in the source audio file.
--
-- 'outputLocationType', 'medicalTranscriptionJobSummary_outputLocationType' - Indicates the location of the transcription job\'s output. This field
-- must be the path of an S3 bucket; if you don\'t already have an S3
-- bucket, one is created based on the path you add.
--
-- 'startTime', 'medicalTranscriptionJobSummary_startTime' - A timestamp that shows when the job began processing.
--
-- 'completionTime', 'medicalTranscriptionJobSummary_completionTime' - A timestamp that shows when the job was completed.
--
-- 'medicalTranscriptionJobName', 'medicalTranscriptionJobSummary_medicalTranscriptionJobName' - The name of a medical transcription job.
--
-- 'transcriptionJobStatus', 'medicalTranscriptionJobSummary_transcriptionJobStatus' - The status of the medical transcription job.
--
-- 'type'', 'medicalTranscriptionJobSummary_type' - The speech of the clinician in the input audio.
--
-- 'contentIdentificationType', 'medicalTranscriptionJobSummary_contentIdentificationType' - Shows the type of information you\'ve configured Amazon Transcribe
-- Medical to identify in a transcription job. If the value is @PHI@,
-- you\'ve configured the transcription job to identify personal health
-- information (PHI).
newMedicalTranscriptionJobSummary ::
  MedicalTranscriptionJobSummary
newMedicalTranscriptionJobSummary =
  MedicalTranscriptionJobSummary'
    { creationTime =
        Prelude.Nothing,
      specialty = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      outputLocationType = Prelude.Nothing,
      startTime = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      medicalTranscriptionJobName =
        Prelude.Nothing,
      transcriptionJobStatus = Prelude.Nothing,
      type' = Prelude.Nothing,
      contentIdentificationType = Prelude.Nothing
    }

-- | A timestamp that shows when the medical transcription job was created.
medicalTranscriptionJobSummary_creationTime :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJobSummary_creationTime = Lens.lens (\MedicalTranscriptionJobSummary' {creationTime} -> creationTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {creationTime = a} :: MedicalTranscriptionJobSummary) Prelude.. Lens.mapping Core._Time

-- | The medical specialty of the transcription job. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-medical-conversation.html Transcribing a medical conversation>for
-- a list of supported specialties.
medicalTranscriptionJobSummary_specialty :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Specialty)
medicalTranscriptionJobSummary_specialty = Lens.lens (\MedicalTranscriptionJobSummary' {specialty} -> specialty) (\s@MedicalTranscriptionJobSummary' {} a -> s {specialty = a} :: MedicalTranscriptionJobSummary)

-- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
-- error.
medicalTranscriptionJobSummary_failureReason :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.Text)
medicalTranscriptionJobSummary_failureReason = Lens.lens (\MedicalTranscriptionJobSummary' {failureReason} -> failureReason) (\s@MedicalTranscriptionJobSummary' {} a -> s {failureReason = a} :: MedicalTranscriptionJobSummary)

-- | The language of the transcript in the source audio file.
medicalTranscriptionJobSummary_languageCode :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe LanguageCode)
medicalTranscriptionJobSummary_languageCode = Lens.lens (\MedicalTranscriptionJobSummary' {languageCode} -> languageCode) (\s@MedicalTranscriptionJobSummary' {} a -> s {languageCode = a} :: MedicalTranscriptionJobSummary)

-- | Indicates the location of the transcription job\'s output. This field
-- must be the path of an S3 bucket; if you don\'t already have an S3
-- bucket, one is created based on the path you add.
medicalTranscriptionJobSummary_outputLocationType :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe OutputLocationType)
medicalTranscriptionJobSummary_outputLocationType = Lens.lens (\MedicalTranscriptionJobSummary' {outputLocationType} -> outputLocationType) (\s@MedicalTranscriptionJobSummary' {} a -> s {outputLocationType = a} :: MedicalTranscriptionJobSummary)

-- | A timestamp that shows when the job began processing.
medicalTranscriptionJobSummary_startTime :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJobSummary_startTime = Lens.lens (\MedicalTranscriptionJobSummary' {startTime} -> startTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {startTime = a} :: MedicalTranscriptionJobSummary) Prelude.. Lens.mapping Core._Time

-- | A timestamp that shows when the job was completed.
medicalTranscriptionJobSummary_completionTime :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJobSummary_completionTime = Lens.lens (\MedicalTranscriptionJobSummary' {completionTime} -> completionTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {completionTime = a} :: MedicalTranscriptionJobSummary) Prelude.. Lens.mapping Core._Time

-- | The name of a medical transcription job.
medicalTranscriptionJobSummary_medicalTranscriptionJobName :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.Text)
medicalTranscriptionJobSummary_medicalTranscriptionJobName = Lens.lens (\MedicalTranscriptionJobSummary' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@MedicalTranscriptionJobSummary' {} a -> s {medicalTranscriptionJobName = a} :: MedicalTranscriptionJobSummary)

-- | The status of the medical transcription job.
medicalTranscriptionJobSummary_transcriptionJobStatus :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe TranscriptionJobStatus)
medicalTranscriptionJobSummary_transcriptionJobStatus = Lens.lens (\MedicalTranscriptionJobSummary' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@MedicalTranscriptionJobSummary' {} a -> s {transcriptionJobStatus = a} :: MedicalTranscriptionJobSummary)

-- | The speech of the clinician in the input audio.
medicalTranscriptionJobSummary_type :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Type)
medicalTranscriptionJobSummary_type = Lens.lens (\MedicalTranscriptionJobSummary' {type'} -> type') (\s@MedicalTranscriptionJobSummary' {} a -> s {type' = a} :: MedicalTranscriptionJobSummary)

-- | Shows the type of information you\'ve configured Amazon Transcribe
-- Medical to identify in a transcription job. If the value is @PHI@,
-- you\'ve configured the transcription job to identify personal health
-- information (PHI).
medicalTranscriptionJobSummary_contentIdentificationType :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe MedicalContentIdentificationType)
medicalTranscriptionJobSummary_contentIdentificationType = Lens.lens (\MedicalTranscriptionJobSummary' {contentIdentificationType} -> contentIdentificationType) (\s@MedicalTranscriptionJobSummary' {} a -> s {contentIdentificationType = a} :: MedicalTranscriptionJobSummary)

instance Core.FromJSON MedicalTranscriptionJobSummary where
  parseJSON =
    Core.withObject
      "MedicalTranscriptionJobSummary"
      ( \x ->
          MedicalTranscriptionJobSummary'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Specialty")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "OutputLocationType")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "CompletionTime")
            Prelude.<*> (x Core..:? "MedicalTranscriptionJobName")
            Prelude.<*> (x Core..:? "TranscriptionJobStatus")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "ContentIdentificationType")
      )

instance
  Prelude.Hashable
    MedicalTranscriptionJobSummary
  where
  hashWithSalt
    salt'
    MedicalTranscriptionJobSummary' {..} =
      salt'
        `Prelude.hashWithSalt` contentIdentificationType
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` transcriptionJobStatus
        `Prelude.hashWithSalt` medicalTranscriptionJobName
        `Prelude.hashWithSalt` completionTime
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` outputLocationType
        `Prelude.hashWithSalt` languageCode
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` specialty
        `Prelude.hashWithSalt` creationTime

instance
  Prelude.NFData
    MedicalTranscriptionJobSummary
  where
  rnf MedicalTranscriptionJobSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf contentIdentificationType
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf transcriptionJobStatus
      `Prelude.seq` Prelude.rnf medicalTranscriptionJobName
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf outputLocationType
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf specialty
