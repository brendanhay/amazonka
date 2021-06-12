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
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.TranscriptionJob where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.JobExecutionSettings
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.Media
import Network.AWS.Transcribe.Types.MediaFormat
import Network.AWS.Transcribe.Types.ModelSettings
import Network.AWS.Transcribe.Types.Settings
import Network.AWS.Transcribe.Types.Transcript
import Network.AWS.Transcribe.Types.TranscriptionJobStatus

-- | Describes an asynchronous transcription job that was created with the
-- @StartTranscriptionJob@ operation.
--
-- /See:/ 'newTranscriptionJob' smart constructor.
data TranscriptionJob = TranscriptionJob'
  { -- | The language code for the input speech.
    languageCode :: Core.Maybe LanguageCode,
    -- | The format of the input media file.
    mediaFormat :: Core.Maybe MediaFormat,
    -- | An object that describes content redaction settings for the
    -- transcription job.
    contentRedaction :: Core.Maybe ContentRedaction,
    -- | An object that describes the input media for the transcription job.
    media :: Core.Maybe Media,
    -- | A timestamp that shows when the job was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Core.Maybe Core.POSIX,
    -- | The name of the transcription job.
    transcriptionJobName :: Core.Maybe Core.Text,
    -- | An object that describes the output of the transcription job.
    transcript :: Core.Maybe Transcript,
    -- | A value that shows if automatic language identification was enabled for
    -- a transcription job.
    identifyLanguage :: Core.Maybe Core.Bool,
    -- | A timestamp that shows with the job was started processing.
    startTime :: Core.Maybe Core.POSIX,
    -- | The status of the transcription job.
    transcriptionJobStatus :: Core.Maybe TranscriptionJobStatus,
    -- | An object containing the details of your custom language model.
    modelSettings :: Core.Maybe ModelSettings,
    -- | A value between zero and one that Amazon Transcribe assigned to the
    -- language that it identified in the source audio. Larger values indicate
    -- that Amazon Transcribe has higher confidence in the language it
    -- identified.
    identifiedLanguageScore :: Core.Maybe Core.Double,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@, this field contains
    -- information about why the job failed.
    --
    -- The @FailureReason@ field can contain one of the following values:
    --
    -- -   @Unsupported media format@ - The media format specified in the
    --     @MediaFormat@ field of the request isn\'t valid. See the description
    --     of the @MediaFormat@ field for a list of valid values.
    --
    -- -   @The media format provided does not match the detected media format@
    --     - The media format of the audio file doesn\'t match the format
    --     specified in the @MediaFormat@ field in the request. Check the media
    --     format of your media file and make sure that the two values match.
    --
    -- -   @Invalid sample rate for audio file@ - The sample rate specified in
    --     the @MediaSampleRateHertz@ of the request isn\'t valid. The sample
    --     rate must be between 8000 and 48000 Hertz.
    --
    -- -   @The sample rate provided does not match the detected sample rate@ -
    --     The sample rate in the audio file doesn\'t match the sample rate
    --     specified in the @MediaSampleRateHertz@ field in the request. Check
    --     the sample rate of your media file and make sure that the two values
    --     match.
    --
    -- -   @Invalid file size: file size too large@ - The size of your audio
    --     file is larger than Amazon Transcribe can process. For more
    --     information, see
    --     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Limits>
    --     in the /Amazon Transcribe Developer Guide/.
    --
    -- -   @Invalid number of channels: number of channels too large@ - Your
    --     audio contains more channels than Amazon Transcribe is configured to
    --     process. To request additional channels, see
    --     <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits-amazon-transcribe Amazon Transcribe Limits>
    --     in the /Amazon Web Services General Reference/.
    failureReason :: Core.Maybe Core.Text,
    -- | The sample rate, in Hertz, of the audio track in the input media file.
    mediaSampleRateHertz :: Core.Maybe Core.Natural,
    -- | Provides information about how a transcription job is executed.
    jobExecutionSettings :: Core.Maybe JobExecutionSettings,
    -- | Optional settings for the transcription job. Use these settings to turn
    -- on speaker recognition, to set the maximum number of speakers that
    -- should be identified and to specify a custom vocabulary to use when
    -- processing the transcription job.
    settings :: Core.Maybe Settings,
    -- | An object that shows the optional array of languages inputted for
    -- transcription jobs with automatic language identification enabled.
    languageOptions :: Core.Maybe (Core.NonEmpty LanguageCode)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'transcriptionJob_languageCode' - The language code for the input speech.
--
-- 'mediaFormat', 'transcriptionJob_mediaFormat' - The format of the input media file.
--
-- 'contentRedaction', 'transcriptionJob_contentRedaction' - An object that describes content redaction settings for the
-- transcription job.
--
-- 'media', 'transcriptionJob_media' - An object that describes the input media for the transcription job.
--
-- 'creationTime', 'transcriptionJob_creationTime' - A timestamp that shows when the job was created.
--
-- 'completionTime', 'transcriptionJob_completionTime' - A timestamp that shows when the job was completed.
--
-- 'transcriptionJobName', 'transcriptionJob_transcriptionJobName' - The name of the transcription job.
--
-- 'transcript', 'transcriptionJob_transcript' - An object that describes the output of the transcription job.
--
-- 'identifyLanguage', 'transcriptionJob_identifyLanguage' - A value that shows if automatic language identification was enabled for
-- a transcription job.
--
-- 'startTime', 'transcriptionJob_startTime' - A timestamp that shows with the job was started processing.
--
-- 'transcriptionJobStatus', 'transcriptionJob_transcriptionJobStatus' - The status of the transcription job.
--
-- 'modelSettings', 'transcriptionJob_modelSettings' - An object containing the details of your custom language model.
--
-- 'identifiedLanguageScore', 'transcriptionJob_identifiedLanguageScore' - A value between zero and one that Amazon Transcribe assigned to the
-- language that it identified in the source audio. Larger values indicate
-- that Amazon Transcribe has higher confidence in the language it
-- identified.
--
-- 'failureReason', 'transcriptionJob_failureReason' - If the @TranscriptionJobStatus@ field is @FAILED@, this field contains
-- information about why the job failed.
--
-- The @FailureReason@ field can contain one of the following values:
--
-- -   @Unsupported media format@ - The media format specified in the
--     @MediaFormat@ field of the request isn\'t valid. See the description
--     of the @MediaFormat@ field for a list of valid values.
--
-- -   @The media format provided does not match the detected media format@
--     - The media format of the audio file doesn\'t match the format
--     specified in the @MediaFormat@ field in the request. Check the media
--     format of your media file and make sure that the two values match.
--
-- -   @Invalid sample rate for audio file@ - The sample rate specified in
--     the @MediaSampleRateHertz@ of the request isn\'t valid. The sample
--     rate must be between 8000 and 48000 Hertz.
--
-- -   @The sample rate provided does not match the detected sample rate@ -
--     The sample rate in the audio file doesn\'t match the sample rate
--     specified in the @MediaSampleRateHertz@ field in the request. Check
--     the sample rate of your media file and make sure that the two values
--     match.
--
-- -   @Invalid file size: file size too large@ - The size of your audio
--     file is larger than Amazon Transcribe can process. For more
--     information, see
--     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Limits>
--     in the /Amazon Transcribe Developer Guide/.
--
-- -   @Invalid number of channels: number of channels too large@ - Your
--     audio contains more channels than Amazon Transcribe is configured to
--     process. To request additional channels, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits-amazon-transcribe Amazon Transcribe Limits>
--     in the /Amazon Web Services General Reference/.
--
-- 'mediaSampleRateHertz', 'transcriptionJob_mediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.
--
-- 'jobExecutionSettings', 'transcriptionJob_jobExecutionSettings' - Provides information about how a transcription job is executed.
--
-- 'settings', 'transcriptionJob_settings' - Optional settings for the transcription job. Use these settings to turn
-- on speaker recognition, to set the maximum number of speakers that
-- should be identified and to specify a custom vocabulary to use when
-- processing the transcription job.
--
-- 'languageOptions', 'transcriptionJob_languageOptions' - An object that shows the optional array of languages inputted for
-- transcription jobs with automatic language identification enabled.
newTranscriptionJob ::
  TranscriptionJob
newTranscriptionJob =
  TranscriptionJob'
    { languageCode = Core.Nothing,
      mediaFormat = Core.Nothing,
      contentRedaction = Core.Nothing,
      media = Core.Nothing,
      creationTime = Core.Nothing,
      completionTime = Core.Nothing,
      transcriptionJobName = Core.Nothing,
      transcript = Core.Nothing,
      identifyLanguage = Core.Nothing,
      startTime = Core.Nothing,
      transcriptionJobStatus = Core.Nothing,
      modelSettings = Core.Nothing,
      identifiedLanguageScore = Core.Nothing,
      failureReason = Core.Nothing,
      mediaSampleRateHertz = Core.Nothing,
      jobExecutionSettings = Core.Nothing,
      settings = Core.Nothing,
      languageOptions = Core.Nothing
    }

-- | The language code for the input speech.
transcriptionJob_languageCode :: Lens.Lens' TranscriptionJob (Core.Maybe LanguageCode)
transcriptionJob_languageCode = Lens.lens (\TranscriptionJob' {languageCode} -> languageCode) (\s@TranscriptionJob' {} a -> s {languageCode = a} :: TranscriptionJob)

-- | The format of the input media file.
transcriptionJob_mediaFormat :: Lens.Lens' TranscriptionJob (Core.Maybe MediaFormat)
transcriptionJob_mediaFormat = Lens.lens (\TranscriptionJob' {mediaFormat} -> mediaFormat) (\s@TranscriptionJob' {} a -> s {mediaFormat = a} :: TranscriptionJob)

-- | An object that describes content redaction settings for the
-- transcription job.
transcriptionJob_contentRedaction :: Lens.Lens' TranscriptionJob (Core.Maybe ContentRedaction)
transcriptionJob_contentRedaction = Lens.lens (\TranscriptionJob' {contentRedaction} -> contentRedaction) (\s@TranscriptionJob' {} a -> s {contentRedaction = a} :: TranscriptionJob)

-- | An object that describes the input media for the transcription job.
transcriptionJob_media :: Lens.Lens' TranscriptionJob (Core.Maybe Media)
transcriptionJob_media = Lens.lens (\TranscriptionJob' {media} -> media) (\s@TranscriptionJob' {} a -> s {media = a} :: TranscriptionJob)

-- | A timestamp that shows when the job was created.
transcriptionJob_creationTime :: Lens.Lens' TranscriptionJob (Core.Maybe Core.UTCTime)
transcriptionJob_creationTime = Lens.lens (\TranscriptionJob' {creationTime} -> creationTime) (\s@TranscriptionJob' {} a -> s {creationTime = a} :: TranscriptionJob) Core.. Lens.mapping Core._Time

-- | A timestamp that shows when the job was completed.
transcriptionJob_completionTime :: Lens.Lens' TranscriptionJob (Core.Maybe Core.UTCTime)
transcriptionJob_completionTime = Lens.lens (\TranscriptionJob' {completionTime} -> completionTime) (\s@TranscriptionJob' {} a -> s {completionTime = a} :: TranscriptionJob) Core.. Lens.mapping Core._Time

-- | The name of the transcription job.
transcriptionJob_transcriptionJobName :: Lens.Lens' TranscriptionJob (Core.Maybe Core.Text)
transcriptionJob_transcriptionJobName = Lens.lens (\TranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@TranscriptionJob' {} a -> s {transcriptionJobName = a} :: TranscriptionJob)

-- | An object that describes the output of the transcription job.
transcriptionJob_transcript :: Lens.Lens' TranscriptionJob (Core.Maybe Transcript)
transcriptionJob_transcript = Lens.lens (\TranscriptionJob' {transcript} -> transcript) (\s@TranscriptionJob' {} a -> s {transcript = a} :: TranscriptionJob)

-- | A value that shows if automatic language identification was enabled for
-- a transcription job.
transcriptionJob_identifyLanguage :: Lens.Lens' TranscriptionJob (Core.Maybe Core.Bool)
transcriptionJob_identifyLanguage = Lens.lens (\TranscriptionJob' {identifyLanguage} -> identifyLanguage) (\s@TranscriptionJob' {} a -> s {identifyLanguage = a} :: TranscriptionJob)

-- | A timestamp that shows with the job was started processing.
transcriptionJob_startTime :: Lens.Lens' TranscriptionJob (Core.Maybe Core.UTCTime)
transcriptionJob_startTime = Lens.lens (\TranscriptionJob' {startTime} -> startTime) (\s@TranscriptionJob' {} a -> s {startTime = a} :: TranscriptionJob) Core.. Lens.mapping Core._Time

-- | The status of the transcription job.
transcriptionJob_transcriptionJobStatus :: Lens.Lens' TranscriptionJob (Core.Maybe TranscriptionJobStatus)
transcriptionJob_transcriptionJobStatus = Lens.lens (\TranscriptionJob' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@TranscriptionJob' {} a -> s {transcriptionJobStatus = a} :: TranscriptionJob)

-- | An object containing the details of your custom language model.
transcriptionJob_modelSettings :: Lens.Lens' TranscriptionJob (Core.Maybe ModelSettings)
transcriptionJob_modelSettings = Lens.lens (\TranscriptionJob' {modelSettings} -> modelSettings) (\s@TranscriptionJob' {} a -> s {modelSettings = a} :: TranscriptionJob)

-- | A value between zero and one that Amazon Transcribe assigned to the
-- language that it identified in the source audio. Larger values indicate
-- that Amazon Transcribe has higher confidence in the language it
-- identified.
transcriptionJob_identifiedLanguageScore :: Lens.Lens' TranscriptionJob (Core.Maybe Core.Double)
transcriptionJob_identifiedLanguageScore = Lens.lens (\TranscriptionJob' {identifiedLanguageScore} -> identifiedLanguageScore) (\s@TranscriptionJob' {} a -> s {identifiedLanguageScore = a} :: TranscriptionJob)

-- | If the @TranscriptionJobStatus@ field is @FAILED@, this field contains
-- information about why the job failed.
--
-- The @FailureReason@ field can contain one of the following values:
--
-- -   @Unsupported media format@ - The media format specified in the
--     @MediaFormat@ field of the request isn\'t valid. See the description
--     of the @MediaFormat@ field for a list of valid values.
--
-- -   @The media format provided does not match the detected media format@
--     - The media format of the audio file doesn\'t match the format
--     specified in the @MediaFormat@ field in the request. Check the media
--     format of your media file and make sure that the two values match.
--
-- -   @Invalid sample rate for audio file@ - The sample rate specified in
--     the @MediaSampleRateHertz@ of the request isn\'t valid. The sample
--     rate must be between 8000 and 48000 Hertz.
--
-- -   @The sample rate provided does not match the detected sample rate@ -
--     The sample rate in the audio file doesn\'t match the sample rate
--     specified in the @MediaSampleRateHertz@ field in the request. Check
--     the sample rate of your media file and make sure that the two values
--     match.
--
-- -   @Invalid file size: file size too large@ - The size of your audio
--     file is larger than Amazon Transcribe can process. For more
--     information, see
--     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Limits>
--     in the /Amazon Transcribe Developer Guide/.
--
-- -   @Invalid number of channels: number of channels too large@ - Your
--     audio contains more channels than Amazon Transcribe is configured to
--     process. To request additional channels, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits-amazon-transcribe Amazon Transcribe Limits>
--     in the /Amazon Web Services General Reference/.
transcriptionJob_failureReason :: Lens.Lens' TranscriptionJob (Core.Maybe Core.Text)
transcriptionJob_failureReason = Lens.lens (\TranscriptionJob' {failureReason} -> failureReason) (\s@TranscriptionJob' {} a -> s {failureReason = a} :: TranscriptionJob)

-- | The sample rate, in Hertz, of the audio track in the input media file.
transcriptionJob_mediaSampleRateHertz :: Lens.Lens' TranscriptionJob (Core.Maybe Core.Natural)
transcriptionJob_mediaSampleRateHertz = Lens.lens (\TranscriptionJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@TranscriptionJob' {} a -> s {mediaSampleRateHertz = a} :: TranscriptionJob)

-- | Provides information about how a transcription job is executed.
transcriptionJob_jobExecutionSettings :: Lens.Lens' TranscriptionJob (Core.Maybe JobExecutionSettings)
transcriptionJob_jobExecutionSettings = Lens.lens (\TranscriptionJob' {jobExecutionSettings} -> jobExecutionSettings) (\s@TranscriptionJob' {} a -> s {jobExecutionSettings = a} :: TranscriptionJob)

-- | Optional settings for the transcription job. Use these settings to turn
-- on speaker recognition, to set the maximum number of speakers that
-- should be identified and to specify a custom vocabulary to use when
-- processing the transcription job.
transcriptionJob_settings :: Lens.Lens' TranscriptionJob (Core.Maybe Settings)
transcriptionJob_settings = Lens.lens (\TranscriptionJob' {settings} -> settings) (\s@TranscriptionJob' {} a -> s {settings = a} :: TranscriptionJob)

-- | An object that shows the optional array of languages inputted for
-- transcription jobs with automatic language identification enabled.
transcriptionJob_languageOptions :: Lens.Lens' TranscriptionJob (Core.Maybe (Core.NonEmpty LanguageCode))
transcriptionJob_languageOptions = Lens.lens (\TranscriptionJob' {languageOptions} -> languageOptions) (\s@TranscriptionJob' {} a -> s {languageOptions = a} :: TranscriptionJob) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON TranscriptionJob where
  parseJSON =
    Core.withObject
      "TranscriptionJob"
      ( \x ->
          TranscriptionJob'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "MediaFormat")
            Core.<*> (x Core..:? "ContentRedaction")
            Core.<*> (x Core..:? "Media")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "CompletionTime")
            Core.<*> (x Core..:? "TranscriptionJobName")
            Core.<*> (x Core..:? "Transcript")
            Core.<*> (x Core..:? "IdentifyLanguage")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "TranscriptionJobStatus")
            Core.<*> (x Core..:? "ModelSettings")
            Core.<*> (x Core..:? "IdentifiedLanguageScore")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "MediaSampleRateHertz")
            Core.<*> (x Core..:? "JobExecutionSettings")
            Core.<*> (x Core..:? "Settings")
            Core.<*> (x Core..:? "LanguageOptions")
      )

instance Core.Hashable TranscriptionJob

instance Core.NFData TranscriptionJob
