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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
    languageCode :: Core.Maybe LanguageCode,
    -- | A timestamp that shows when the medical transcription job was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Core.Maybe Core.POSIX,
    -- | A timestamp that shows when the job began processing.
    startTime :: Core.Maybe Core.POSIX,
    -- | The status of the medical transcription job.
    transcriptionJobStatus :: Core.Maybe TranscriptionJobStatus,
    -- | Indicates the location of the transcription job\'s output.
    --
    -- The @CUSTOMER_BUCKET@ is the S3 location provided in the
    -- @OutputBucketName@ field when the
    outputLocationType :: Core.Maybe OutputLocationType,
    -- | The medical specialty of the transcription job. @Primary care@ is the
    -- only valid value.
    specialty :: Core.Maybe Specialty,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
    -- error.
    failureReason :: Core.Maybe Core.Text,
    -- | The speech of the clinician in the input audio.
    type' :: Core.Maybe Type,
    -- | The name of a medical transcription job.
    medicalTranscriptionJobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      creationTime = Core.Nothing,
      completionTime = Core.Nothing,
      startTime = Core.Nothing,
      transcriptionJobStatus = Core.Nothing,
      outputLocationType = Core.Nothing,
      specialty = Core.Nothing,
      failureReason = Core.Nothing,
      type' = Core.Nothing,
      medicalTranscriptionJobName = Core.Nothing
    }

-- | The language of the transcript in the source audio file.
medicalTranscriptionJobSummary_languageCode :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe LanguageCode)
medicalTranscriptionJobSummary_languageCode = Lens.lens (\MedicalTranscriptionJobSummary' {languageCode} -> languageCode) (\s@MedicalTranscriptionJobSummary' {} a -> s {languageCode = a} :: MedicalTranscriptionJobSummary)

-- | A timestamp that shows when the medical transcription job was created.
medicalTranscriptionJobSummary_creationTime :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Core.UTCTime)
medicalTranscriptionJobSummary_creationTime = Lens.lens (\MedicalTranscriptionJobSummary' {creationTime} -> creationTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {creationTime = a} :: MedicalTranscriptionJobSummary) Core.. Lens.mapping Core._Time

-- | A timestamp that shows when the job was completed.
medicalTranscriptionJobSummary_completionTime :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Core.UTCTime)
medicalTranscriptionJobSummary_completionTime = Lens.lens (\MedicalTranscriptionJobSummary' {completionTime} -> completionTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {completionTime = a} :: MedicalTranscriptionJobSummary) Core.. Lens.mapping Core._Time

-- | A timestamp that shows when the job began processing.
medicalTranscriptionJobSummary_startTime :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Core.UTCTime)
medicalTranscriptionJobSummary_startTime = Lens.lens (\MedicalTranscriptionJobSummary' {startTime} -> startTime) (\s@MedicalTranscriptionJobSummary' {} a -> s {startTime = a} :: MedicalTranscriptionJobSummary) Core.. Lens.mapping Core._Time

-- | The status of the medical transcription job.
medicalTranscriptionJobSummary_transcriptionJobStatus :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe TranscriptionJobStatus)
medicalTranscriptionJobSummary_transcriptionJobStatus = Lens.lens (\MedicalTranscriptionJobSummary' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@MedicalTranscriptionJobSummary' {} a -> s {transcriptionJobStatus = a} :: MedicalTranscriptionJobSummary)

-- | Indicates the location of the transcription job\'s output.
--
-- The @CUSTOMER_BUCKET@ is the S3 location provided in the
-- @OutputBucketName@ field when the
medicalTranscriptionJobSummary_outputLocationType :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe OutputLocationType)
medicalTranscriptionJobSummary_outputLocationType = Lens.lens (\MedicalTranscriptionJobSummary' {outputLocationType} -> outputLocationType) (\s@MedicalTranscriptionJobSummary' {} a -> s {outputLocationType = a} :: MedicalTranscriptionJobSummary)

-- | The medical specialty of the transcription job. @Primary care@ is the
-- only valid value.
medicalTranscriptionJobSummary_specialty :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Specialty)
medicalTranscriptionJobSummary_specialty = Lens.lens (\MedicalTranscriptionJobSummary' {specialty} -> specialty) (\s@MedicalTranscriptionJobSummary' {} a -> s {specialty = a} :: MedicalTranscriptionJobSummary)

-- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
-- error.
medicalTranscriptionJobSummary_failureReason :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Core.Text)
medicalTranscriptionJobSummary_failureReason = Lens.lens (\MedicalTranscriptionJobSummary' {failureReason} -> failureReason) (\s@MedicalTranscriptionJobSummary' {} a -> s {failureReason = a} :: MedicalTranscriptionJobSummary)

-- | The speech of the clinician in the input audio.
medicalTranscriptionJobSummary_type :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Type)
medicalTranscriptionJobSummary_type = Lens.lens (\MedicalTranscriptionJobSummary' {type'} -> type') (\s@MedicalTranscriptionJobSummary' {} a -> s {type' = a} :: MedicalTranscriptionJobSummary)

-- | The name of a medical transcription job.
medicalTranscriptionJobSummary_medicalTranscriptionJobName :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Core.Text)
medicalTranscriptionJobSummary_medicalTranscriptionJobName = Lens.lens (\MedicalTranscriptionJobSummary' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@MedicalTranscriptionJobSummary' {} a -> s {medicalTranscriptionJobName = a} :: MedicalTranscriptionJobSummary)

instance Core.FromJSON MedicalTranscriptionJobSummary where
  parseJSON =
    Core.withObject
      "MedicalTranscriptionJobSummary"
      ( \x ->
          MedicalTranscriptionJobSummary'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "CompletionTime")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "TranscriptionJobStatus")
            Core.<*> (x Core..:? "OutputLocationType")
            Core.<*> (x Core..:? "Specialty")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "MedicalTranscriptionJobName")
      )

instance Core.Hashable MedicalTranscriptionJobSummary

instance Core.NFData MedicalTranscriptionJobSummary
