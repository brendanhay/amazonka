{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
  ( MedicalTranscriptionJobSummary (..),

    -- * Smart constructor
    mkMedicalTranscriptionJobSummary,

    -- * Lenses
    mtjsCreationTime,
    mtjsSpecialty,
    mtjsFailureReason,
    mtjsLanguageCode,
    mtjsOutputLocationType,
    mtjsStartTime,
    mtjsCompletionTime,
    mtjsMedicalTranscriptionJobName,
    mtjsTranscriptionJobStatus,
    mtjsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.OutputLocationType
import Network.AWS.Transcribe.Types.Specialty
import Network.AWS.Transcribe.Types.TranscriptionJobStatus
import Network.AWS.Transcribe.Types.Type

-- | Provides summary information about a transcription job.
--
-- /See:/ 'mkMedicalTranscriptionJobSummary' smart constructor.
data MedicalTranscriptionJobSummary = MedicalTranscriptionJobSummary'
  { -- | A timestamp that shows when the medical transcription job was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The medical specialty of the transcription job. @Primary care@ is the only valid value.
    specialty :: Lude.Maybe Specialty,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The language of the transcript in the source audio file.
    languageCode :: Lude.Maybe LanguageCode,
    -- | Indicates the location of the transcription job's output.
    --
    -- The @CUSTOMER_BUCKET@ is the S3 location provided in the @OutputBucketName@ field when the
    outputLocationType :: Lude.Maybe OutputLocationType,
    -- | A timestamp that shows when the job began processing.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of a medical transcription job.
    medicalTranscriptionJobName :: Lude.Maybe Lude.Text,
    -- | The status of the medical transcription job.
    transcriptionJobStatus :: Lude.Maybe TranscriptionJobStatus,
    -- | The speech of the clinician in the input audio.
    type' :: Lude.Maybe Type
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MedicalTranscriptionJobSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the medical transcription job was created.
-- * 'specialty' - The medical specialty of the transcription job. @Primary care@ is the only valid value.
-- * 'failureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
-- * 'languageCode' - The language of the transcript in the source audio file.
-- * 'outputLocationType' - Indicates the location of the transcription job's output.
--
-- The @CUSTOMER_BUCKET@ is the S3 location provided in the @OutputBucketName@ field when the
-- * 'startTime' - A timestamp that shows when the job began processing.
-- * 'completionTime' - A timestamp that shows when the job was completed.
-- * 'medicalTranscriptionJobName' - The name of a medical transcription job.
-- * 'transcriptionJobStatus' - The status of the medical transcription job.
-- * 'type'' - The speech of the clinician in the input audio.
mkMedicalTranscriptionJobSummary ::
  MedicalTranscriptionJobSummary
mkMedicalTranscriptionJobSummary =
  MedicalTranscriptionJobSummary'
    { creationTime = Lude.Nothing,
      specialty = Lude.Nothing,
      failureReason = Lude.Nothing,
      languageCode = Lude.Nothing,
      outputLocationType = Lude.Nothing,
      startTime = Lude.Nothing,
      completionTime = Lude.Nothing,
      medicalTranscriptionJobName = Lude.Nothing,
      transcriptionJobStatus = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | A timestamp that shows when the medical transcription job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsCreationTime :: Lens.Lens' MedicalTranscriptionJobSummary (Lude.Maybe Lude.Timestamp)
mtjsCreationTime = Lens.lens (creationTime :: MedicalTranscriptionJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: MedicalTranscriptionJobSummary)
{-# DEPRECATED mtjsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The medical specialty of the transcription job. @Primary care@ is the only valid value.
--
-- /Note:/ Consider using 'specialty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsSpecialty :: Lens.Lens' MedicalTranscriptionJobSummary (Lude.Maybe Specialty)
mtjsSpecialty = Lens.lens (specialty :: MedicalTranscriptionJobSummary -> Lude.Maybe Specialty) (\s a -> s {specialty = a} :: MedicalTranscriptionJobSummary)
{-# DEPRECATED mtjsSpecialty "Use generic-lens or generic-optics with 'specialty' instead." #-}

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsFailureReason :: Lens.Lens' MedicalTranscriptionJobSummary (Lude.Maybe Lude.Text)
mtjsFailureReason = Lens.lens (failureReason :: MedicalTranscriptionJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: MedicalTranscriptionJobSummary)
{-# DEPRECATED mtjsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The language of the transcript in the source audio file.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsLanguageCode :: Lens.Lens' MedicalTranscriptionJobSummary (Lude.Maybe LanguageCode)
mtjsLanguageCode = Lens.lens (languageCode :: MedicalTranscriptionJobSummary -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: MedicalTranscriptionJobSummary)
{-# DEPRECATED mtjsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Indicates the location of the transcription job's output.
--
-- The @CUSTOMER_BUCKET@ is the S3 location provided in the @OutputBucketName@ field when the
--
-- /Note:/ Consider using 'outputLocationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsOutputLocationType :: Lens.Lens' MedicalTranscriptionJobSummary (Lude.Maybe OutputLocationType)
mtjsOutputLocationType = Lens.lens (outputLocationType :: MedicalTranscriptionJobSummary -> Lude.Maybe OutputLocationType) (\s a -> s {outputLocationType = a} :: MedicalTranscriptionJobSummary)
{-# DEPRECATED mtjsOutputLocationType "Use generic-lens or generic-optics with 'outputLocationType' instead." #-}

-- | A timestamp that shows when the job began processing.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsStartTime :: Lens.Lens' MedicalTranscriptionJobSummary (Lude.Maybe Lude.Timestamp)
mtjsStartTime = Lens.lens (startTime :: MedicalTranscriptionJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: MedicalTranscriptionJobSummary)
{-# DEPRECATED mtjsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A timestamp that shows when the job was completed.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsCompletionTime :: Lens.Lens' MedicalTranscriptionJobSummary (Lude.Maybe Lude.Timestamp)
mtjsCompletionTime = Lens.lens (completionTime :: MedicalTranscriptionJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {completionTime = a} :: MedicalTranscriptionJobSummary)
{-# DEPRECATED mtjsCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

-- | The name of a medical transcription job.
--
-- /Note:/ Consider using 'medicalTranscriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsMedicalTranscriptionJobName :: Lens.Lens' MedicalTranscriptionJobSummary (Lude.Maybe Lude.Text)
mtjsMedicalTranscriptionJobName = Lens.lens (medicalTranscriptionJobName :: MedicalTranscriptionJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {medicalTranscriptionJobName = a} :: MedicalTranscriptionJobSummary)
{-# DEPRECATED mtjsMedicalTranscriptionJobName "Use generic-lens or generic-optics with 'medicalTranscriptionJobName' instead." #-}

-- | The status of the medical transcription job.
--
-- /Note:/ Consider using 'transcriptionJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsTranscriptionJobStatus :: Lens.Lens' MedicalTranscriptionJobSummary (Lude.Maybe TranscriptionJobStatus)
mtjsTranscriptionJobStatus = Lens.lens (transcriptionJobStatus :: MedicalTranscriptionJobSummary -> Lude.Maybe TranscriptionJobStatus) (\s a -> s {transcriptionJobStatus = a} :: MedicalTranscriptionJobSummary)
{-# DEPRECATED mtjsTranscriptionJobStatus "Use generic-lens or generic-optics with 'transcriptionJobStatus' instead." #-}

-- | The speech of the clinician in the input audio.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsType :: Lens.Lens' MedicalTranscriptionJobSummary (Lude.Maybe Type)
mtjsType = Lens.lens (type' :: MedicalTranscriptionJobSummary -> Lude.Maybe Type) (\s a -> s {type' = a} :: MedicalTranscriptionJobSummary)
{-# DEPRECATED mtjsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON MedicalTranscriptionJobSummary where
  parseJSON =
    Lude.withObject
      "MedicalTranscriptionJobSummary"
      ( \x ->
          MedicalTranscriptionJobSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Specialty")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "OutputLocationType")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "CompletionTime")
            Lude.<*> (x Lude..:? "MedicalTranscriptionJobName")
            Lude.<*> (x Lude..:? "TranscriptionJobStatus")
            Lude.<*> (x Lude..:? "Type")
      )
