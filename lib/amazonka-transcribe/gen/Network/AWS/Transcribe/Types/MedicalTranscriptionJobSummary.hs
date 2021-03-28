{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
  ( MedicalTranscriptionJobSummary (..)
  -- * Smart constructor
  , mkMedicalTranscriptionJobSummary
  -- * Lenses
  , mtjsCompletionTime
  , mtjsCreationTime
  , mtjsFailureReason
  , mtjsLanguageCode
  , mtjsMedicalTranscriptionJobName
  , mtjsOutputLocationType
  , mtjsSpecialty
  , mtjsStartTime
  , mtjsTranscriptionJobStatus
  , mtjsType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.FailureReason as Types
import qualified Network.AWS.Transcribe.Types.LanguageCode as Types
import qualified Network.AWS.Transcribe.Types.OutputLocationType as Types
import qualified Network.AWS.Transcribe.Types.Specialty as Types
import qualified Network.AWS.Transcribe.Types.TranscriptionJobName as Types
import qualified Network.AWS.Transcribe.Types.TranscriptionJobStatus as Types
import qualified Network.AWS.Transcribe.Types.Type as Types

-- | Provides summary information about a transcription job.
--
-- /See:/ 'mkMedicalTranscriptionJobSummary' smart constructor.
data MedicalTranscriptionJobSummary = MedicalTranscriptionJobSummary'
  { completionTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that shows when the job was completed.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that shows when the medical transcription job was created.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
  , languageCode :: Core.Maybe Types.LanguageCode
    -- ^ The language of the transcript in the source audio file.
  , medicalTranscriptionJobName :: Core.Maybe Types.TranscriptionJobName
    -- ^ The name of a medical transcription job.
  , outputLocationType :: Core.Maybe Types.OutputLocationType
    -- ^ Indicates the location of the transcription job's output.
--
-- The @CUSTOMER_BUCKET@ is the S3 location provided in the @OutputBucketName@ field when the 
  , specialty :: Core.Maybe Types.Specialty
    -- ^ The medical specialty of the transcription job. @Primary care@ is the only valid value.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that shows when the job began processing.
  , transcriptionJobStatus :: Core.Maybe Types.TranscriptionJobStatus
    -- ^ The status of the medical transcription job.
  , type' :: Core.Maybe Types.Type
    -- ^ The speech of the clinician in the input audio.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MedicalTranscriptionJobSummary' value with any optional fields omitted.
mkMedicalTranscriptionJobSummary
    :: MedicalTranscriptionJobSummary
mkMedicalTranscriptionJobSummary
  = MedicalTranscriptionJobSummary'{completionTime = Core.Nothing,
                                    creationTime = Core.Nothing, failureReason = Core.Nothing,
                                    languageCode = Core.Nothing,
                                    medicalTranscriptionJobName = Core.Nothing,
                                    outputLocationType = Core.Nothing, specialty = Core.Nothing,
                                    startTime = Core.Nothing, transcriptionJobStatus = Core.Nothing,
                                    type' = Core.Nothing}

-- | A timestamp that shows when the job was completed.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsCompletionTime :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Core.NominalDiffTime)
mtjsCompletionTime = Lens.field @"completionTime"
{-# INLINEABLE mtjsCompletionTime #-}
{-# DEPRECATED completionTime "Use generic-lens or generic-optics with 'completionTime' instead"  #-}

-- | A timestamp that shows when the medical transcription job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsCreationTime :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Core.NominalDiffTime)
mtjsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE mtjsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsFailureReason :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Types.FailureReason)
mtjsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE mtjsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The language of the transcript in the source audio file.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsLanguageCode :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Types.LanguageCode)
mtjsLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE mtjsLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | The name of a medical transcription job.
--
-- /Note:/ Consider using 'medicalTranscriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsMedicalTranscriptionJobName :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Types.TranscriptionJobName)
mtjsMedicalTranscriptionJobName = Lens.field @"medicalTranscriptionJobName"
{-# INLINEABLE mtjsMedicalTranscriptionJobName #-}
{-# DEPRECATED medicalTranscriptionJobName "Use generic-lens or generic-optics with 'medicalTranscriptionJobName' instead"  #-}

-- | Indicates the location of the transcription job's output.
--
-- The @CUSTOMER_BUCKET@ is the S3 location provided in the @OutputBucketName@ field when the 
--
-- /Note:/ Consider using 'outputLocationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsOutputLocationType :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Types.OutputLocationType)
mtjsOutputLocationType = Lens.field @"outputLocationType"
{-# INLINEABLE mtjsOutputLocationType #-}
{-# DEPRECATED outputLocationType "Use generic-lens or generic-optics with 'outputLocationType' instead"  #-}

-- | The medical specialty of the transcription job. @Primary care@ is the only valid value.
--
-- /Note:/ Consider using 'specialty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsSpecialty :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Types.Specialty)
mtjsSpecialty = Lens.field @"specialty"
{-# INLINEABLE mtjsSpecialty #-}
{-# DEPRECATED specialty "Use generic-lens or generic-optics with 'specialty' instead"  #-}

-- | A timestamp that shows when the job began processing.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsStartTime :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Core.NominalDiffTime)
mtjsStartTime = Lens.field @"startTime"
{-# INLINEABLE mtjsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The status of the medical transcription job.
--
-- /Note:/ Consider using 'transcriptionJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsTranscriptionJobStatus :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Types.TranscriptionJobStatus)
mtjsTranscriptionJobStatus = Lens.field @"transcriptionJobStatus"
{-# INLINEABLE mtjsTranscriptionJobStatus #-}
{-# DEPRECATED transcriptionJobStatus "Use generic-lens or generic-optics with 'transcriptionJobStatus' instead"  #-}

-- | The speech of the clinician in the input audio.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjsType :: Lens.Lens' MedicalTranscriptionJobSummary (Core.Maybe Types.Type)
mtjsType = Lens.field @"type'"
{-# INLINEABLE mtjsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON MedicalTranscriptionJobSummary where
        parseJSON
          = Core.withObject "MedicalTranscriptionJobSummary" Core.$
              \ x ->
                MedicalTranscriptionJobSummary' Core.<$>
                  (x Core..:? "CompletionTime") Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "FailureReason"
                    Core.<*> x Core..:? "LanguageCode"
                    Core.<*> x Core..:? "MedicalTranscriptionJobName"
                    Core.<*> x Core..:? "OutputLocationType"
                    Core.<*> x Core..:? "Specialty"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "TranscriptionJobStatus"
                    Core.<*> x Core..:? "Type"
