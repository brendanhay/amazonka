{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.TranscriptionJob
  ( TranscriptionJob (..),

    -- * Smart constructor
    mkTranscriptionJob,

    -- * Lenses
    tjCompletionTime,
    tjContentRedaction,
    tjCreationTime,
    tjFailureReason,
    tjIdentifiedLanguageScore,
    tjIdentifyLanguage,
    tjJobExecutionSettings,
    tjLanguageCode,
    tjLanguageOptions,
    tjMedia,
    tjMediaFormat,
    tjMediaSampleRateHertz,
    tjModelSettings,
    tjSettings,
    tjStartTime,
    tjTranscript,
    tjTranscriptionJobName,
    tjTranscriptionJobStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.ContentRedaction as Types
import qualified Network.AWS.Transcribe.Types.FailureReason as Types
import qualified Network.AWS.Transcribe.Types.JobExecutionSettings as Types
import qualified Network.AWS.Transcribe.Types.LanguageCode as Types
import qualified Network.AWS.Transcribe.Types.Media as Types
import qualified Network.AWS.Transcribe.Types.MediaFormat as Types
import qualified Network.AWS.Transcribe.Types.ModelSettings as Types
import qualified Network.AWS.Transcribe.Types.Settings as Types
import qualified Network.AWS.Transcribe.Types.Transcript as Types
import qualified Network.AWS.Transcribe.Types.TranscriptionJobName as Types
import qualified Network.AWS.Transcribe.Types.TranscriptionJobStatus as Types

-- | Describes an asynchronous transcription job that was created with the @StartTranscriptionJob@ operation.
--
-- /See:/ 'mkTranscriptionJob' smart constructor.
data TranscriptionJob = TranscriptionJob'
  { -- | A timestamp that shows when the job was completed.
    completionTime :: Core.Maybe Core.NominalDiffTime,
    -- | An object that describes content redaction settings for the transcription job.
    contentRedaction :: Core.Maybe Types.ContentRedaction,
    -- | A timestamp that shows when the job was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed.
    --
    -- The @FailureReason@ field can contain one of the following values:
    --
    --     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.
    --
    --
    --     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure that the two values match.
    --
    --
    --     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.
    --
    --
    --     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.
    --
    --
    --     * @Invalid file size: file size too large@ - The size of your audio file is larger than Amazon Transcribe can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Limits> in the /Amazon Transcribe Developer Guide/ .
    --
    --
    --     * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits-amazon-transcribe Amazon Transcribe Limits> in the /Amazon Web Services General Reference/ .
    failureReason :: Core.Maybe Types.FailureReason,
    -- | A value between zero and one that Amazon Transcribe assigned to the language that it identified in the source audio. Larger values indicate that Amazon Transcribe has higher confidence in the language it identified.
    identifiedLanguageScore :: Core.Maybe Core.Double,
    -- | A value that shows if automatic language identification was enabled for a transcription job.
    identifyLanguage :: Core.Maybe Core.Bool,
    -- | Provides information about how a transcription job is executed.
    jobExecutionSettings :: Core.Maybe Types.JobExecutionSettings,
    -- | The language code for the input speech.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | An object that shows the optional array of languages inputted for transcription jobs with automatic language identification enabled.
    languageOptions :: Core.Maybe (Core.NonEmpty Types.LanguageCode),
    -- | An object that describes the input media for the transcription job.
    media :: Core.Maybe Types.Media,
    -- | The format of the input media file.
    mediaFormat :: Core.Maybe Types.MediaFormat,
    -- | The sample rate, in Hertz, of the audio track in the input media file.
    mediaSampleRateHertz :: Core.Maybe Core.Natural,
    -- | An object containing the details of your custom language model.
    modelSettings :: Core.Maybe Types.ModelSettings,
    -- | Optional settings for the transcription job. Use these settings to turn on speaker recognition, to set the maximum number of speakers that should be identified and to specify a custom vocabulary to use when processing the transcription job.
    settings :: Core.Maybe Types.Settings,
    -- | A timestamp that shows with the job was started processing.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | An object that describes the output of the transcription job.
    transcript :: Core.Maybe Types.Transcript,
    -- | The name of the transcription job.
    transcriptionJobName :: Core.Maybe Types.TranscriptionJobName,
    -- | The status of the transcription job.
    transcriptionJobStatus :: Core.Maybe Types.TranscriptionJobStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TranscriptionJob' value with any optional fields omitted.
mkTranscriptionJob ::
  TranscriptionJob
mkTranscriptionJob =
  TranscriptionJob'
    { completionTime = Core.Nothing,
      contentRedaction = Core.Nothing,
      creationTime = Core.Nothing,
      failureReason = Core.Nothing,
      identifiedLanguageScore = Core.Nothing,
      identifyLanguage = Core.Nothing,
      jobExecutionSettings = Core.Nothing,
      languageCode = Core.Nothing,
      languageOptions = Core.Nothing,
      media = Core.Nothing,
      mediaFormat = Core.Nothing,
      mediaSampleRateHertz = Core.Nothing,
      modelSettings = Core.Nothing,
      settings = Core.Nothing,
      startTime = Core.Nothing,
      transcript = Core.Nothing,
      transcriptionJobName = Core.Nothing,
      transcriptionJobStatus = Core.Nothing
    }

-- | A timestamp that shows when the job was completed.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjCompletionTime :: Lens.Lens' TranscriptionJob (Core.Maybe Core.NominalDiffTime)
tjCompletionTime = Lens.field @"completionTime"
{-# DEPRECATED tjCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

-- | An object that describes content redaction settings for the transcription job.
--
-- /Note:/ Consider using 'contentRedaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjContentRedaction :: Lens.Lens' TranscriptionJob (Core.Maybe Types.ContentRedaction)
tjContentRedaction = Lens.field @"contentRedaction"
{-# DEPRECATED tjContentRedaction "Use generic-lens or generic-optics with 'contentRedaction' instead." #-}

-- | A timestamp that shows when the job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjCreationTime :: Lens.Lens' TranscriptionJob (Core.Maybe Core.NominalDiffTime)
tjCreationTime = Lens.field @"creationTime"
{-# DEPRECATED tjCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed.
--
-- The @FailureReason@ field can contain one of the following values:
--
--     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.
--
--
--     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure that the two values match.
--
--
--     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.
--
--
--     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.
--
--
--     * @Invalid file size: file size too large@ - The size of your audio file is larger than Amazon Transcribe can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Limits> in the /Amazon Transcribe Developer Guide/ .
--
--
--     * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits-amazon-transcribe Amazon Transcribe Limits> in the /Amazon Web Services General Reference/ .
--
--
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjFailureReason :: Lens.Lens' TranscriptionJob (Core.Maybe Types.FailureReason)
tjFailureReason = Lens.field @"failureReason"
{-# DEPRECATED tjFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | A value between zero and one that Amazon Transcribe assigned to the language that it identified in the source audio. Larger values indicate that Amazon Transcribe has higher confidence in the language it identified.
--
-- /Note:/ Consider using 'identifiedLanguageScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjIdentifiedLanguageScore :: Lens.Lens' TranscriptionJob (Core.Maybe Core.Double)
tjIdentifiedLanguageScore = Lens.field @"identifiedLanguageScore"
{-# DEPRECATED tjIdentifiedLanguageScore "Use generic-lens or generic-optics with 'identifiedLanguageScore' instead." #-}

-- | A value that shows if automatic language identification was enabled for a transcription job.
--
-- /Note:/ Consider using 'identifyLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjIdentifyLanguage :: Lens.Lens' TranscriptionJob (Core.Maybe Core.Bool)
tjIdentifyLanguage = Lens.field @"identifyLanguage"
{-# DEPRECATED tjIdentifyLanguage "Use generic-lens or generic-optics with 'identifyLanguage' instead." #-}

-- | Provides information about how a transcription job is executed.
--
-- /Note:/ Consider using 'jobExecutionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjJobExecutionSettings :: Lens.Lens' TranscriptionJob (Core.Maybe Types.JobExecutionSettings)
tjJobExecutionSettings = Lens.field @"jobExecutionSettings"
{-# DEPRECATED tjJobExecutionSettings "Use generic-lens or generic-optics with 'jobExecutionSettings' instead." #-}

-- | The language code for the input speech.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjLanguageCode :: Lens.Lens' TranscriptionJob (Core.Maybe Types.LanguageCode)
tjLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED tjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | An object that shows the optional array of languages inputted for transcription jobs with automatic language identification enabled.
--
-- /Note:/ Consider using 'languageOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjLanguageOptions :: Lens.Lens' TranscriptionJob (Core.Maybe (Core.NonEmpty Types.LanguageCode))
tjLanguageOptions = Lens.field @"languageOptions"
{-# DEPRECATED tjLanguageOptions "Use generic-lens or generic-optics with 'languageOptions' instead." #-}

-- | An object that describes the input media for the transcription job.
--
-- /Note:/ Consider using 'media' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjMedia :: Lens.Lens' TranscriptionJob (Core.Maybe Types.Media)
tjMedia = Lens.field @"media"
{-# DEPRECATED tjMedia "Use generic-lens or generic-optics with 'media' instead." #-}

-- | The format of the input media file.
--
-- /Note:/ Consider using 'mediaFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjMediaFormat :: Lens.Lens' TranscriptionJob (Core.Maybe Types.MediaFormat)
tjMediaFormat = Lens.field @"mediaFormat"
{-# DEPRECATED tjMediaFormat "Use generic-lens or generic-optics with 'mediaFormat' instead." #-}

-- | The sample rate, in Hertz, of the audio track in the input media file.
--
-- /Note:/ Consider using 'mediaSampleRateHertz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjMediaSampleRateHertz :: Lens.Lens' TranscriptionJob (Core.Maybe Core.Natural)
tjMediaSampleRateHertz = Lens.field @"mediaSampleRateHertz"
{-# DEPRECATED tjMediaSampleRateHertz "Use generic-lens or generic-optics with 'mediaSampleRateHertz' instead." #-}

-- | An object containing the details of your custom language model.
--
-- /Note:/ Consider using 'modelSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjModelSettings :: Lens.Lens' TranscriptionJob (Core.Maybe Types.ModelSettings)
tjModelSettings = Lens.field @"modelSettings"
{-# DEPRECATED tjModelSettings "Use generic-lens or generic-optics with 'modelSettings' instead." #-}

-- | Optional settings for the transcription job. Use these settings to turn on speaker recognition, to set the maximum number of speakers that should be identified and to specify a custom vocabulary to use when processing the transcription job.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjSettings :: Lens.Lens' TranscriptionJob (Core.Maybe Types.Settings)
tjSettings = Lens.field @"settings"
{-# DEPRECATED tjSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | A timestamp that shows with the job was started processing.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjStartTime :: Lens.Lens' TranscriptionJob (Core.Maybe Core.NominalDiffTime)
tjStartTime = Lens.field @"startTime"
{-# DEPRECATED tjStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | An object that describes the output of the transcription job.
--
-- /Note:/ Consider using 'transcript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTranscript :: Lens.Lens' TranscriptionJob (Core.Maybe Types.Transcript)
tjTranscript = Lens.field @"transcript"
{-# DEPRECATED tjTranscript "Use generic-lens or generic-optics with 'transcript' instead." #-}

-- | The name of the transcription job.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTranscriptionJobName :: Lens.Lens' TranscriptionJob (Core.Maybe Types.TranscriptionJobName)
tjTranscriptionJobName = Lens.field @"transcriptionJobName"
{-# DEPRECATED tjTranscriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead." #-}

-- | The status of the transcription job.
--
-- /Note:/ Consider using 'transcriptionJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTranscriptionJobStatus :: Lens.Lens' TranscriptionJob (Core.Maybe Types.TranscriptionJobStatus)
tjTranscriptionJobStatus = Lens.field @"transcriptionJobStatus"
{-# DEPRECATED tjTranscriptionJobStatus "Use generic-lens or generic-optics with 'transcriptionJobStatus' instead." #-}

instance Core.FromJSON TranscriptionJob where
  parseJSON =
    Core.withObject "TranscriptionJob" Core.$
      \x ->
        TranscriptionJob'
          Core.<$> (x Core..:? "CompletionTime")
          Core.<*> (x Core..:? "ContentRedaction")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "FailureReason")
          Core.<*> (x Core..:? "IdentifiedLanguageScore")
          Core.<*> (x Core..:? "IdentifyLanguage")
          Core.<*> (x Core..:? "JobExecutionSettings")
          Core.<*> (x Core..:? "LanguageCode")
          Core.<*> (x Core..:? "LanguageOptions")
          Core.<*> (x Core..:? "Media")
          Core.<*> (x Core..:? "MediaFormat")
          Core.<*> (x Core..:? "MediaSampleRateHertz")
          Core.<*> (x Core..:? "ModelSettings")
          Core.<*> (x Core..:? "Settings")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "Transcript")
          Core.<*> (x Core..:? "TranscriptionJobName")
          Core.<*> (x Core..:? "TranscriptionJobStatus")
