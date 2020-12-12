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
    tjCreationTime,
    tjFailureReason,
    tjContentRedaction,
    tjIdentifiedLanguageScore,
    tjLanguageCode,
    tjLanguageOptions,
    tjSettings,
    tjStartTime,
    tjCompletionTime,
    tjMedia,
    tjMediaFormat,
    tjModelSettings,
    tjTranscriptionJobStatus,
    tjJobExecutionSettings,
    tjTranscriptionJobName,
    tjIdentifyLanguage,
    tjTranscript,
    tjMediaSampleRateHertz,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.JobExecutionSettings
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.Media
import Network.AWS.Transcribe.Types.MediaFormat
import Network.AWS.Transcribe.Types.ModelSettings
import Network.AWS.Transcribe.Types.Settings
import Network.AWS.Transcribe.Types.Transcript
import Network.AWS.Transcribe.Types.TranscriptionJobStatus

-- | Describes an asynchronous transcription job that was created with the @StartTranscriptionJob@ operation.
--
-- /See:/ 'mkTranscriptionJob' smart constructor.
data TranscriptionJob = TranscriptionJob'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    failureReason :: Lude.Maybe Lude.Text,
    contentRedaction :: Lude.Maybe ContentRedaction,
    identifiedLanguageScore :: Lude.Maybe Lude.Double,
    languageCode :: Lude.Maybe LanguageCode,
    languageOptions ::
      Lude.Maybe (Lude.NonEmpty LanguageCode),
    settings :: Lude.Maybe Settings,
    startTime :: Lude.Maybe Lude.Timestamp,
    completionTime :: Lude.Maybe Lude.Timestamp,
    media :: Lude.Maybe Media,
    mediaFormat :: Lude.Maybe MediaFormat,
    modelSettings :: Lude.Maybe ModelSettings,
    transcriptionJobStatus ::
      Lude.Maybe TranscriptionJobStatus,
    jobExecutionSettings :: Lude.Maybe JobExecutionSettings,
    transcriptionJobName :: Lude.Maybe Lude.Text,
    identifyLanguage :: Lude.Maybe Lude.Bool,
    transcript :: Lude.Maybe Transcript,
    mediaSampleRateHertz :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TranscriptionJob' with the minimum fields required to make a request.
--
-- * 'completionTime' - A timestamp that shows when the job was completed.
-- * 'contentRedaction' - An object that describes content redaction settings for the transcription job.
-- * 'creationTime' - A timestamp that shows when the job was created.
-- * 'failureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed.
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
-- * 'identifiedLanguageScore' - A value between zero and one that Amazon Transcribe assigned to the language that it identified in the source audio. Larger values indicate that Amazon Transcribe has higher confidence in the language it identified.
-- * 'identifyLanguage' - A value that shows if automatic language identification was enabled for a transcription job.
-- * 'jobExecutionSettings' - Provides information about how a transcription job is executed.
-- * 'languageCode' - The language code for the input speech.
-- * 'languageOptions' - An object that shows the optional array of languages inputted for transcription jobs with automatic language identification enabled.
-- * 'media' - An object that describes the input media for the transcription job.
-- * 'mediaFormat' - The format of the input media file.
-- * 'mediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.
-- * 'modelSettings' - An object containing the details of your custom language model.
-- * 'settings' - Optional settings for the transcription job. Use these settings to turn on speaker recognition, to set the maximum number of speakers that should be identified and to specify a custom vocabulary to use when processing the transcription job.
-- * 'startTime' - A timestamp that shows with the job was started processing.
-- * 'transcript' - An object that describes the output of the transcription job.
-- * 'transcriptionJobName' - The name of the transcription job.
-- * 'transcriptionJobStatus' - The status of the transcription job.
mkTranscriptionJob ::
  TranscriptionJob
mkTranscriptionJob =
  TranscriptionJob'
    { creationTime = Lude.Nothing,
      failureReason = Lude.Nothing,
      contentRedaction = Lude.Nothing,
      identifiedLanguageScore = Lude.Nothing,
      languageCode = Lude.Nothing,
      languageOptions = Lude.Nothing,
      settings = Lude.Nothing,
      startTime = Lude.Nothing,
      completionTime = Lude.Nothing,
      media = Lude.Nothing,
      mediaFormat = Lude.Nothing,
      modelSettings = Lude.Nothing,
      transcriptionJobStatus = Lude.Nothing,
      jobExecutionSettings = Lude.Nothing,
      transcriptionJobName = Lude.Nothing,
      identifyLanguage = Lude.Nothing,
      transcript = Lude.Nothing,
      mediaSampleRateHertz = Lude.Nothing
    }

-- | A timestamp that shows when the job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjCreationTime :: Lens.Lens' TranscriptionJob (Lude.Maybe Lude.Timestamp)
tjCreationTime = Lens.lens (creationTime :: TranscriptionJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: TranscriptionJob)
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
tjFailureReason :: Lens.Lens' TranscriptionJob (Lude.Maybe Lude.Text)
tjFailureReason = Lens.lens (failureReason :: TranscriptionJob -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: TranscriptionJob)
{-# DEPRECATED tjFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | An object that describes content redaction settings for the transcription job.
--
-- /Note:/ Consider using 'contentRedaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjContentRedaction :: Lens.Lens' TranscriptionJob (Lude.Maybe ContentRedaction)
tjContentRedaction = Lens.lens (contentRedaction :: TranscriptionJob -> Lude.Maybe ContentRedaction) (\s a -> s {contentRedaction = a} :: TranscriptionJob)
{-# DEPRECATED tjContentRedaction "Use generic-lens or generic-optics with 'contentRedaction' instead." #-}

-- | A value between zero and one that Amazon Transcribe assigned to the language that it identified in the source audio. Larger values indicate that Amazon Transcribe has higher confidence in the language it identified.
--
-- /Note:/ Consider using 'identifiedLanguageScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjIdentifiedLanguageScore :: Lens.Lens' TranscriptionJob (Lude.Maybe Lude.Double)
tjIdentifiedLanguageScore = Lens.lens (identifiedLanguageScore :: TranscriptionJob -> Lude.Maybe Lude.Double) (\s a -> s {identifiedLanguageScore = a} :: TranscriptionJob)
{-# DEPRECATED tjIdentifiedLanguageScore "Use generic-lens or generic-optics with 'identifiedLanguageScore' instead." #-}

-- | The language code for the input speech.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjLanguageCode :: Lens.Lens' TranscriptionJob (Lude.Maybe LanguageCode)
tjLanguageCode = Lens.lens (languageCode :: TranscriptionJob -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: TranscriptionJob)
{-# DEPRECATED tjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | An object that shows the optional array of languages inputted for transcription jobs with automatic language identification enabled.
--
-- /Note:/ Consider using 'languageOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjLanguageOptions :: Lens.Lens' TranscriptionJob (Lude.Maybe (Lude.NonEmpty LanguageCode))
tjLanguageOptions = Lens.lens (languageOptions :: TranscriptionJob -> Lude.Maybe (Lude.NonEmpty LanguageCode)) (\s a -> s {languageOptions = a} :: TranscriptionJob)
{-# DEPRECATED tjLanguageOptions "Use generic-lens or generic-optics with 'languageOptions' instead." #-}

-- | Optional settings for the transcription job. Use these settings to turn on speaker recognition, to set the maximum number of speakers that should be identified and to specify a custom vocabulary to use when processing the transcription job.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjSettings :: Lens.Lens' TranscriptionJob (Lude.Maybe Settings)
tjSettings = Lens.lens (settings :: TranscriptionJob -> Lude.Maybe Settings) (\s a -> s {settings = a} :: TranscriptionJob)
{-# DEPRECATED tjSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | A timestamp that shows with the job was started processing.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjStartTime :: Lens.Lens' TranscriptionJob (Lude.Maybe Lude.Timestamp)
tjStartTime = Lens.lens (startTime :: TranscriptionJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: TranscriptionJob)
{-# DEPRECATED tjStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A timestamp that shows when the job was completed.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjCompletionTime :: Lens.Lens' TranscriptionJob (Lude.Maybe Lude.Timestamp)
tjCompletionTime = Lens.lens (completionTime :: TranscriptionJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {completionTime = a} :: TranscriptionJob)
{-# DEPRECATED tjCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

-- | An object that describes the input media for the transcription job.
--
-- /Note:/ Consider using 'media' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjMedia :: Lens.Lens' TranscriptionJob (Lude.Maybe Media)
tjMedia = Lens.lens (media :: TranscriptionJob -> Lude.Maybe Media) (\s a -> s {media = a} :: TranscriptionJob)
{-# DEPRECATED tjMedia "Use generic-lens or generic-optics with 'media' instead." #-}

-- | The format of the input media file.
--
-- /Note:/ Consider using 'mediaFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjMediaFormat :: Lens.Lens' TranscriptionJob (Lude.Maybe MediaFormat)
tjMediaFormat = Lens.lens (mediaFormat :: TranscriptionJob -> Lude.Maybe MediaFormat) (\s a -> s {mediaFormat = a} :: TranscriptionJob)
{-# DEPRECATED tjMediaFormat "Use generic-lens or generic-optics with 'mediaFormat' instead." #-}

-- | An object containing the details of your custom language model.
--
-- /Note:/ Consider using 'modelSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjModelSettings :: Lens.Lens' TranscriptionJob (Lude.Maybe ModelSettings)
tjModelSettings = Lens.lens (modelSettings :: TranscriptionJob -> Lude.Maybe ModelSettings) (\s a -> s {modelSettings = a} :: TranscriptionJob)
{-# DEPRECATED tjModelSettings "Use generic-lens or generic-optics with 'modelSettings' instead." #-}

-- | The status of the transcription job.
--
-- /Note:/ Consider using 'transcriptionJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTranscriptionJobStatus :: Lens.Lens' TranscriptionJob (Lude.Maybe TranscriptionJobStatus)
tjTranscriptionJobStatus = Lens.lens (transcriptionJobStatus :: TranscriptionJob -> Lude.Maybe TranscriptionJobStatus) (\s a -> s {transcriptionJobStatus = a} :: TranscriptionJob)
{-# DEPRECATED tjTranscriptionJobStatus "Use generic-lens or generic-optics with 'transcriptionJobStatus' instead." #-}

-- | Provides information about how a transcription job is executed.
--
-- /Note:/ Consider using 'jobExecutionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjJobExecutionSettings :: Lens.Lens' TranscriptionJob (Lude.Maybe JobExecutionSettings)
tjJobExecutionSettings = Lens.lens (jobExecutionSettings :: TranscriptionJob -> Lude.Maybe JobExecutionSettings) (\s a -> s {jobExecutionSettings = a} :: TranscriptionJob)
{-# DEPRECATED tjJobExecutionSettings "Use generic-lens or generic-optics with 'jobExecutionSettings' instead." #-}

-- | The name of the transcription job.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTranscriptionJobName :: Lens.Lens' TranscriptionJob (Lude.Maybe Lude.Text)
tjTranscriptionJobName = Lens.lens (transcriptionJobName :: TranscriptionJob -> Lude.Maybe Lude.Text) (\s a -> s {transcriptionJobName = a} :: TranscriptionJob)
{-# DEPRECATED tjTranscriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead." #-}

-- | A value that shows if automatic language identification was enabled for a transcription job.
--
-- /Note:/ Consider using 'identifyLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjIdentifyLanguage :: Lens.Lens' TranscriptionJob (Lude.Maybe Lude.Bool)
tjIdentifyLanguage = Lens.lens (identifyLanguage :: TranscriptionJob -> Lude.Maybe Lude.Bool) (\s a -> s {identifyLanguage = a} :: TranscriptionJob)
{-# DEPRECATED tjIdentifyLanguage "Use generic-lens or generic-optics with 'identifyLanguage' instead." #-}

-- | An object that describes the output of the transcription job.
--
-- /Note:/ Consider using 'transcript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTranscript :: Lens.Lens' TranscriptionJob (Lude.Maybe Transcript)
tjTranscript = Lens.lens (transcript :: TranscriptionJob -> Lude.Maybe Transcript) (\s a -> s {transcript = a} :: TranscriptionJob)
{-# DEPRECATED tjTranscript "Use generic-lens or generic-optics with 'transcript' instead." #-}

-- | The sample rate, in Hertz, of the audio track in the input media file.
--
-- /Note:/ Consider using 'mediaSampleRateHertz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjMediaSampleRateHertz :: Lens.Lens' TranscriptionJob (Lude.Maybe Lude.Natural)
tjMediaSampleRateHertz = Lens.lens (mediaSampleRateHertz :: TranscriptionJob -> Lude.Maybe Lude.Natural) (\s a -> s {mediaSampleRateHertz = a} :: TranscriptionJob)
{-# DEPRECATED tjMediaSampleRateHertz "Use generic-lens or generic-optics with 'mediaSampleRateHertz' instead." #-}

instance Lude.FromJSON TranscriptionJob where
  parseJSON =
    Lude.withObject
      "TranscriptionJob"
      ( \x ->
          TranscriptionJob'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "ContentRedaction")
            Lude.<*> (x Lude..:? "IdentifiedLanguageScore")
            Lude.<*> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "LanguageOptions")
            Lude.<*> (x Lude..:? "Settings")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "CompletionTime")
            Lude.<*> (x Lude..:? "Media")
            Lude.<*> (x Lude..:? "MediaFormat")
            Lude.<*> (x Lude..:? "ModelSettings")
            Lude.<*> (x Lude..:? "TranscriptionJobStatus")
            Lude.<*> (x Lude..:? "JobExecutionSettings")
            Lude.<*> (x Lude..:? "TranscriptionJobName")
            Lude.<*> (x Lude..:? "IdentifyLanguage")
            Lude.<*> (x Lude..:? "Transcript")
            Lude.<*> (x Lude..:? "MediaSampleRateHertz")
      )
