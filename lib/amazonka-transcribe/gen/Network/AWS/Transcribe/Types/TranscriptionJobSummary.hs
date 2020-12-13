{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.TranscriptionJobSummary
  ( TranscriptionJobSummary (..),

    -- * Smart constructor
    mkTranscriptionJobSummary,

    -- * Lenses
    tjsCreationTime,
    tjsFailureReason,
    tjsContentRedaction,
    tjsIdentifiedLanguageScore,
    tjsLanguageCode,
    tjsOutputLocationType,
    tjsStartTime,
    tjsCompletionTime,
    tjsModelSettings,
    tjsTranscriptionJobStatus,
    tjsTranscriptionJobName,
    tjsIdentifyLanguage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.ModelSettings
import Network.AWS.Transcribe.Types.OutputLocationType
import Network.AWS.Transcribe.Types.TranscriptionJobStatus

-- | Provides a summary of information about a transcription job.
--
-- /See:/ 'mkTranscriptionJobSummary' smart constructor.
data TranscriptionJobSummary = TranscriptionJobSummary'
  { -- | A timestamp that shows when the job was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The content redaction settings of the transcription job.
    contentRedaction :: Lude.Maybe ContentRedaction,
    -- | A value between zero and one that Amazon Transcribe assigned to the language it identified in the source audio. A higher score indicates that Amazon Transcribe is more confident in the language it identified.
    identifiedLanguageScore :: Lude.Maybe Lude.Double,
    -- | The language code for the input speech.
    languageCode :: Lude.Maybe LanguageCode,
    -- | Indicates the location of the output of the transcription job.
    --
    -- If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket specified in the @outputBucketName@ field when the transcription job was started with the @StartTranscriptionJob@ operation.
    -- If the value is @SERVICE_BUCKET@ then the output is stored by Amazon Transcribe and can be retrieved using the URI in the @GetTranscriptionJob@ response's @TranscriptFileUri@ field.
    outputLocationType :: Lude.Maybe OutputLocationType,
    -- | A timestamp that shows when the job started processing.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Lude.Maybe Lude.Timestamp,
    modelSettings :: Lude.Maybe ModelSettings,
    -- | The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
    transcriptionJobStatus :: Lude.Maybe TranscriptionJobStatus,
    -- | The name of the transcription job.
    transcriptionJobName :: Lude.Maybe Lude.Text,
    -- | Whether automatic language identification was enabled for a transcription job.
    identifyLanguage :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TranscriptionJobSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the job was created.
-- * 'failureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
-- * 'contentRedaction' - The content redaction settings of the transcription job.
-- * 'identifiedLanguageScore' - A value between zero and one that Amazon Transcribe assigned to the language it identified in the source audio. A higher score indicates that Amazon Transcribe is more confident in the language it identified.
-- * 'languageCode' - The language code for the input speech.
-- * 'outputLocationType' - Indicates the location of the output of the transcription job.
--
-- If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket specified in the @outputBucketName@ field when the transcription job was started with the @StartTranscriptionJob@ operation.
-- If the value is @SERVICE_BUCKET@ then the output is stored by Amazon Transcribe and can be retrieved using the URI in the @GetTranscriptionJob@ response's @TranscriptFileUri@ field.
-- * 'startTime' - A timestamp that shows when the job started processing.
-- * 'completionTime' - A timestamp that shows when the job was completed.
-- * 'modelSettings' -
-- * 'transcriptionJobStatus' - The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
-- * 'transcriptionJobName' - The name of the transcription job.
-- * 'identifyLanguage' - Whether automatic language identification was enabled for a transcription job.
mkTranscriptionJobSummary ::
  TranscriptionJobSummary
mkTranscriptionJobSummary =
  TranscriptionJobSummary'
    { creationTime = Lude.Nothing,
      failureReason = Lude.Nothing,
      contentRedaction = Lude.Nothing,
      identifiedLanguageScore = Lude.Nothing,
      languageCode = Lude.Nothing,
      outputLocationType = Lude.Nothing,
      startTime = Lude.Nothing,
      completionTime = Lude.Nothing,
      modelSettings = Lude.Nothing,
      transcriptionJobStatus = Lude.Nothing,
      transcriptionJobName = Lude.Nothing,
      identifyLanguage = Lude.Nothing
    }

-- | A timestamp that shows when the job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsCreationTime :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe Lude.Timestamp)
tjsCreationTime = Lens.lens (creationTime :: TranscriptionJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsFailureReason :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe Lude.Text)
tjsFailureReason = Lens.lens (failureReason :: TranscriptionJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The content redaction settings of the transcription job.
--
-- /Note:/ Consider using 'contentRedaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsContentRedaction :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe ContentRedaction)
tjsContentRedaction = Lens.lens (contentRedaction :: TranscriptionJobSummary -> Lude.Maybe ContentRedaction) (\s a -> s {contentRedaction = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsContentRedaction "Use generic-lens or generic-optics with 'contentRedaction' instead." #-}

-- | A value between zero and one that Amazon Transcribe assigned to the language it identified in the source audio. A higher score indicates that Amazon Transcribe is more confident in the language it identified.
--
-- /Note:/ Consider using 'identifiedLanguageScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsIdentifiedLanguageScore :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe Lude.Double)
tjsIdentifiedLanguageScore = Lens.lens (identifiedLanguageScore :: TranscriptionJobSummary -> Lude.Maybe Lude.Double) (\s a -> s {identifiedLanguageScore = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsIdentifiedLanguageScore "Use generic-lens or generic-optics with 'identifiedLanguageScore' instead." #-}

-- | The language code for the input speech.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsLanguageCode :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe LanguageCode)
tjsLanguageCode = Lens.lens (languageCode :: TranscriptionJobSummary -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Indicates the location of the output of the transcription job.
--
-- If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket specified in the @outputBucketName@ field when the transcription job was started with the @StartTranscriptionJob@ operation.
-- If the value is @SERVICE_BUCKET@ then the output is stored by Amazon Transcribe and can be retrieved using the URI in the @GetTranscriptionJob@ response's @TranscriptFileUri@ field.
--
-- /Note:/ Consider using 'outputLocationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsOutputLocationType :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe OutputLocationType)
tjsOutputLocationType = Lens.lens (outputLocationType :: TranscriptionJobSummary -> Lude.Maybe OutputLocationType) (\s a -> s {outputLocationType = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsOutputLocationType "Use generic-lens or generic-optics with 'outputLocationType' instead." #-}

-- | A timestamp that shows when the job started processing.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsStartTime :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe Lude.Timestamp)
tjsStartTime = Lens.lens (startTime :: TranscriptionJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A timestamp that shows when the job was completed.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsCompletionTime :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe Lude.Timestamp)
tjsCompletionTime = Lens.lens (completionTime :: TranscriptionJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {completionTime = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'modelSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsModelSettings :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe ModelSettings)
tjsModelSettings = Lens.lens (modelSettings :: TranscriptionJobSummary -> Lude.Maybe ModelSettings) (\s a -> s {modelSettings = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsModelSettings "Use generic-lens or generic-optics with 'modelSettings' instead." #-}

-- | The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
--
-- /Note:/ Consider using 'transcriptionJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTranscriptionJobStatus :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe TranscriptionJobStatus)
tjsTranscriptionJobStatus = Lens.lens (transcriptionJobStatus :: TranscriptionJobSummary -> Lude.Maybe TranscriptionJobStatus) (\s a -> s {transcriptionJobStatus = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsTranscriptionJobStatus "Use generic-lens or generic-optics with 'transcriptionJobStatus' instead." #-}

-- | The name of the transcription job.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTranscriptionJobName :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe Lude.Text)
tjsTranscriptionJobName = Lens.lens (transcriptionJobName :: TranscriptionJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {transcriptionJobName = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsTranscriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead." #-}

-- | Whether automatic language identification was enabled for a transcription job.
--
-- /Note:/ Consider using 'identifyLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsIdentifyLanguage :: Lens.Lens' TranscriptionJobSummary (Lude.Maybe Lude.Bool)
tjsIdentifyLanguage = Lens.lens (identifyLanguage :: TranscriptionJobSummary -> Lude.Maybe Lude.Bool) (\s a -> s {identifyLanguage = a} :: TranscriptionJobSummary)
{-# DEPRECATED tjsIdentifyLanguage "Use generic-lens or generic-optics with 'identifyLanguage' instead." #-}

instance Lude.FromJSON TranscriptionJobSummary where
  parseJSON =
    Lude.withObject
      "TranscriptionJobSummary"
      ( \x ->
          TranscriptionJobSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "ContentRedaction")
            Lude.<*> (x Lude..:? "IdentifiedLanguageScore")
            Lude.<*> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "OutputLocationType")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "CompletionTime")
            Lude.<*> (x Lude..:? "ModelSettings")
            Lude.<*> (x Lude..:? "TranscriptionJobStatus")
            Lude.<*> (x Lude..:? "TranscriptionJobName")
            Lude.<*> (x Lude..:? "IdentifyLanguage")
      )
