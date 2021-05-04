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
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.OutputLocationType
import Network.AWS.Transcribe.Types.Specialty
import Network.AWS.Transcribe.Types.TranscriptionJobStatus
import Network.AWS.Transcribe.Types.Type

-- | Provides summary information about a transcription job.
--
-- /See:/ 'newMedicalTranscriptionJobSummary' smart constructor.
data MedicalTranscriptionJobSummary = MedicalTranscriptionJobSummary'
  { -- | The language of the transcript in the source audio file.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | A timestamp that shows when the medical transcription job was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Prelude.Maybe Prelude.POSIX,
    -- | A timestamp that shows when the job began processing.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The status of the medical transcription job.
    transcriptionJobStatus :: Prelude.Maybe TranscriptionJobStatus,
    -- | Indicates the location of the transcription job\'s output.
    --
    -- The @CUSTOMER_BUCKET@ is the S3 location provided in the
    -- @OutputBucketName@ field when the
    outputLocationType :: Prelude.Maybe OutputLocationType,
    -- | The medical specialty of the transcription job. @Primary care@ is the
    -- only valid value.
    specialty :: Prelude.Maybe Specialty,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
    -- error.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The speech of the clinician in the input audio.
    type' :: Prelude.Maybe Type,
    -- | The name of a medical transcription job.
    medicalTranscriptionJobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MedicalTranscriptionJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'medicalTranscriptionJobSummary_languageCode' - The language of the transcript in the source audio file.
--
-- 'creationTime', 'medicalTranscriptionJobSummary_creationTime' - A timestamp that shows when the medical transcription job was created.
--
-- 'completionTime', 'medicalTranscriptionJobSummary_completionTime' - A timestamp that shows when the job was completed.
--
-- 'startTime', 'medicalTranscriptionJobSummary_startTime' - A timestamp that shows when the job began processing.
--
-- 'transcriptionJobStatus', 'medicalTranscriptionJobSummary_transcriptionJobStatus' - The status of the medical transcription job.
--
-- 'outputLocationType', 'medicalTranscriptionJobSummary_outputLocationType' - Indicates the location of the transcription job\'s output.
--
-- The @CUSTOMER_BUCKET@ is the S3 location provided in the
-- @OutputBucketName@ field when the
--
-- 'specialty', 'medicalTranscriptionJobSummary_specialty' - The medical specialty of the transcription job. @Primary care@ is the
-- only valid value.
--
-- 'failureReason', 'medicalTranscriptionJobSummary_failureReason' - If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
-- error.
--
-- 'type'', 'medicalTranscriptionJobSummary_type' - The speech of the clinician in the input audio.
--
-- 'medicalTranscriptionJobName', 'medicalTranscriptionJobSummary_medicalTranscriptionJobName' - The name of a medical transcription job.
newMedicalTranscriptionJobSummary ::
  MedicalTranscriptionJobSummary
newMedicalTranscriptionJobSummary =
  MedicalTranscriptionJobSummary'
    { languageCode =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      startTime = Prelude.Nothing,
      transcriptionJobStatus = Prelude.Nothing,
      outputLocationType = Prelude.Nothing,
      specialty = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      type' = Prelude.Nothing,
      medicalTranscriptionJobName =
        Prelude.Nothing
    }

-- | The language of the transcript in the source audio file.
medicalTranscriptionJobSummary_languageCode :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe LanguageCode)
medicalTranscriptionJobSummary_languageCode = Lens.lens (\MedicalTranscriptionJobSummary' {languageCode} -> languageCode) (\s@MedicalTranscriptionJobSummary' {} a -> s {languageCode = a} :: MedicalTranscriptionJobSummary)

-- | A timestamp that shows when the medical transcription job was created.
medicalTranscriptionJobSummary_creationTime :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJobSummary_creationTime = Lens.lens (\MedicalTranscriptionJobSummary' {creationTime} -> creationTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {creationTime = a} :: MedicalTranscriptionJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | A timestamp that shows when the job was completed.
medicalTranscriptionJobSummary_completionTime :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJobSummary_completionTime = Lens.lens (\MedicalTranscriptionJobSummary' {completionTime} -> completionTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {completionTime = a} :: MedicalTranscriptionJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | A timestamp that shows when the job began processing.
medicalTranscriptionJobSummary_startTime :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJobSummary_startTime = Lens.lens (\MedicalTranscriptionJobSummary' {startTime} -> startTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {startTime = a} :: MedicalTranscriptionJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | The status of the medical transcription job.
medicalTranscriptionJobSummary_transcriptionJobStatus :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe TranscriptionJobStatus)
medicalTranscriptionJobSummary_transcriptionJobStatus = Lens.lens (\MedicalTranscriptionJobSummary' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@MedicalTranscriptionJobSummary' {} a -> s {transcriptionJobStatus = a} :: MedicalTranscriptionJobSummary)

-- | Indicates the location of the transcription job\'s output.
--
-- The @CUSTOMER_BUCKET@ is the S3 location provided in the
-- @OutputBucketName@ field when the
medicalTranscriptionJobSummary_outputLocationType :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe OutputLocationType)
medicalTranscriptionJobSummary_outputLocationType = Lens.lens (\MedicalTranscriptionJobSummary' {outputLocationType} -> outputLocationType) (\s@MedicalTranscriptionJobSummary' {} a -> s {outputLocationType = a} :: MedicalTranscriptionJobSummary)

-- | The medical specialty of the transcription job. @Primary care@ is the
-- only valid value.
medicalTranscriptionJobSummary_specialty :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Specialty)
medicalTranscriptionJobSummary_specialty = Lens.lens (\MedicalTranscriptionJobSummary' {specialty} -> specialty) (\s@MedicalTranscriptionJobSummary' {} a -> s {specialty = a} :: MedicalTranscriptionJobSummary)

-- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
-- error.
medicalTranscriptionJobSummary_failureReason :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.Text)
medicalTranscriptionJobSummary_failureReason = Lens.lens (\MedicalTranscriptionJobSummary' {failureReason} -> failureReason) (\s@MedicalTranscriptionJobSummary' {} a -> s {failureReason = a} :: MedicalTranscriptionJobSummary)

-- | The speech of the clinician in the input audio.
medicalTranscriptionJobSummary_type :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Type)
medicalTranscriptionJobSummary_type = Lens.lens (\MedicalTranscriptionJobSummary' {type'} -> type') (\s@MedicalTranscriptionJobSummary' {} a -> s {type' = a} :: MedicalTranscriptionJobSummary)

-- | The name of a medical transcription job.
medicalTranscriptionJobSummary_medicalTranscriptionJobName :: Lens.Lens' MedicalTranscriptionJobSummary (Prelude.Maybe Prelude.Text)
medicalTranscriptionJobSummary_medicalTranscriptionJobName = Lens.lens (\MedicalTranscriptionJobSummary' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@MedicalTranscriptionJobSummary' {} a -> s {medicalTranscriptionJobName = a} :: MedicalTranscriptionJobSummary)

instance
  Prelude.FromJSON
    MedicalTranscriptionJobSummary
  where
  parseJSON =
    Prelude.withObject
      "MedicalTranscriptionJobSummary"
      ( \x ->
          MedicalTranscriptionJobSummary'
            Prelude.<$> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "CompletionTime")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "TranscriptionJobStatus")
            Prelude.<*> (x Prelude..:? "OutputLocationType")
            Prelude.<*> (x Prelude..:? "Specialty")
            Prelude.<*> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "MedicalTranscriptionJobName")
      )

instance
  Prelude.Hashable
    MedicalTranscriptionJobSummary

instance
  Prelude.NFData
    MedicalTranscriptionJobSummary
