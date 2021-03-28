{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.TranscriptionJobSummary
  ( TranscriptionJobSummary (..)
  -- * Smart constructor
  , mkTranscriptionJobSummary
  -- * Lenses
  , tjsCompletionTime
  , tjsContentRedaction
  , tjsCreationTime
  , tjsFailureReason
  , tjsIdentifiedLanguageScore
  , tjsIdentifyLanguage
  , tjsLanguageCode
  , tjsModelSettings
  , tjsOutputLocationType
  , tjsStartTime
  , tjsTranscriptionJobName
  , tjsTranscriptionJobStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.ContentRedaction as Types
import qualified Network.AWS.Transcribe.Types.FailureReason as Types
import qualified Network.AWS.Transcribe.Types.LanguageCode as Types
import qualified Network.AWS.Transcribe.Types.ModelSettings as Types
import qualified Network.AWS.Transcribe.Types.OutputLocationType as Types
import qualified Network.AWS.Transcribe.Types.TranscriptionJobName as Types
import qualified Network.AWS.Transcribe.Types.TranscriptionJobStatus as Types

-- | Provides a summary of information about a transcription job.
--
-- /See:/ 'mkTranscriptionJobSummary' smart constructor.
data TranscriptionJobSummary = TranscriptionJobSummary'
  { completionTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that shows when the job was completed.
  , contentRedaction :: Core.Maybe Types.ContentRedaction
    -- ^ The content redaction settings of the transcription job.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that shows when the job was created.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
  , identifiedLanguageScore :: Core.Maybe Core.Double
    -- ^ A value between zero and one that Amazon Transcribe assigned to the language it identified in the source audio. A higher score indicates that Amazon Transcribe is more confident in the language it identified.
  , identifyLanguage :: Core.Maybe Core.Bool
    -- ^ Whether automatic language identification was enabled for a transcription job.
  , languageCode :: Core.Maybe Types.LanguageCode
    -- ^ The language code for the input speech.
  , modelSettings :: Core.Maybe Types.ModelSettings
  , outputLocationType :: Core.Maybe Types.OutputLocationType
    -- ^ Indicates the location of the output of the transcription job.
--
-- If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket specified in the @outputBucketName@ field when the transcription job was started with the @StartTranscriptionJob@ operation.
-- If the value is @SERVICE_BUCKET@ then the output is stored by Amazon Transcribe and can be retrieved using the URI in the @GetTranscriptionJob@ response's @TranscriptFileUri@ field.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that shows when the job started processing.
  , transcriptionJobName :: Core.Maybe Types.TranscriptionJobName
    -- ^ The name of the transcription job.
  , transcriptionJobStatus :: Core.Maybe Types.TranscriptionJobStatus
    -- ^ The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TranscriptionJobSummary' value with any optional fields omitted.
mkTranscriptionJobSummary
    :: TranscriptionJobSummary
mkTranscriptionJobSummary
  = TranscriptionJobSummary'{completionTime = Core.Nothing,
                             contentRedaction = Core.Nothing, creationTime = Core.Nothing,
                             failureReason = Core.Nothing,
                             identifiedLanguageScore = Core.Nothing,
                             identifyLanguage = Core.Nothing, languageCode = Core.Nothing,
                             modelSettings = Core.Nothing, outputLocationType = Core.Nothing,
                             startTime = Core.Nothing, transcriptionJobName = Core.Nothing,
                             transcriptionJobStatus = Core.Nothing}

-- | A timestamp that shows when the job was completed.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsCompletionTime :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.NominalDiffTime)
tjsCompletionTime = Lens.field @"completionTime"
{-# INLINEABLE tjsCompletionTime #-}
{-# DEPRECATED completionTime "Use generic-lens or generic-optics with 'completionTime' instead"  #-}

-- | The content redaction settings of the transcription job.
--
-- /Note:/ Consider using 'contentRedaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsContentRedaction :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Types.ContentRedaction)
tjsContentRedaction = Lens.field @"contentRedaction"
{-# INLINEABLE tjsContentRedaction #-}
{-# DEPRECATED contentRedaction "Use generic-lens or generic-optics with 'contentRedaction' instead"  #-}

-- | A timestamp that shows when the job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsCreationTime :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.NominalDiffTime)
tjsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE tjsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsFailureReason :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Types.FailureReason)
tjsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE tjsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | A value between zero and one that Amazon Transcribe assigned to the language it identified in the source audio. A higher score indicates that Amazon Transcribe is more confident in the language it identified.
--
-- /Note:/ Consider using 'identifiedLanguageScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsIdentifiedLanguageScore :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.Double)
tjsIdentifiedLanguageScore = Lens.field @"identifiedLanguageScore"
{-# INLINEABLE tjsIdentifiedLanguageScore #-}
{-# DEPRECATED identifiedLanguageScore "Use generic-lens or generic-optics with 'identifiedLanguageScore' instead"  #-}

-- | Whether automatic language identification was enabled for a transcription job.
--
-- /Note:/ Consider using 'identifyLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsIdentifyLanguage :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.Bool)
tjsIdentifyLanguage = Lens.field @"identifyLanguage"
{-# INLINEABLE tjsIdentifyLanguage #-}
{-# DEPRECATED identifyLanguage "Use generic-lens or generic-optics with 'identifyLanguage' instead"  #-}

-- | The language code for the input speech.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsLanguageCode :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Types.LanguageCode)
tjsLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE tjsLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'modelSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsModelSettings :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Types.ModelSettings)
tjsModelSettings = Lens.field @"modelSettings"
{-# INLINEABLE tjsModelSettings #-}
{-# DEPRECATED modelSettings "Use generic-lens or generic-optics with 'modelSettings' instead"  #-}

-- | Indicates the location of the output of the transcription job.
--
-- If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket specified in the @outputBucketName@ field when the transcription job was started with the @StartTranscriptionJob@ operation.
-- If the value is @SERVICE_BUCKET@ then the output is stored by Amazon Transcribe and can be retrieved using the URI in the @GetTranscriptionJob@ response's @TranscriptFileUri@ field.
--
-- /Note:/ Consider using 'outputLocationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsOutputLocationType :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Types.OutputLocationType)
tjsOutputLocationType = Lens.field @"outputLocationType"
{-# INLINEABLE tjsOutputLocationType #-}
{-# DEPRECATED outputLocationType "Use generic-lens or generic-optics with 'outputLocationType' instead"  #-}

-- | A timestamp that shows when the job started processing.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsStartTime :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.NominalDiffTime)
tjsStartTime = Lens.field @"startTime"
{-# INLINEABLE tjsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The name of the transcription job.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTranscriptionJobName :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Types.TranscriptionJobName)
tjsTranscriptionJobName = Lens.field @"transcriptionJobName"
{-# INLINEABLE tjsTranscriptionJobName #-}
{-# DEPRECATED transcriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead"  #-}

-- | The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
--
-- /Note:/ Consider using 'transcriptionJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTranscriptionJobStatus :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Types.TranscriptionJobStatus)
tjsTranscriptionJobStatus = Lens.field @"transcriptionJobStatus"
{-# INLINEABLE tjsTranscriptionJobStatus #-}
{-# DEPRECATED transcriptionJobStatus "Use generic-lens or generic-optics with 'transcriptionJobStatus' instead"  #-}

instance Core.FromJSON TranscriptionJobSummary where
        parseJSON
          = Core.withObject "TranscriptionJobSummary" Core.$
              \ x ->
                TranscriptionJobSummary' Core.<$>
                  (x Core..:? "CompletionTime") Core.<*>
                    x Core..:? "ContentRedaction"
                    Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "FailureReason"
                    Core.<*> x Core..:? "IdentifiedLanguageScore"
                    Core.<*> x Core..:? "IdentifyLanguage"
                    Core.<*> x Core..:? "LanguageCode"
                    Core.<*> x Core..:? "ModelSettings"
                    Core.<*> x Core..:? "OutputLocationType"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "TranscriptionJobName"
                    Core.<*> x Core..:? "TranscriptionJobStatus"
