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
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.TranscriptionJob where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The format of the input media file.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | An object that describes content redaction settings for the
    -- transcription job.
    contentRedaction :: Prelude.Maybe ContentRedaction,
    -- | An object that describes the input media for the transcription job.
    media :: Prelude.Maybe Media,
    -- | A timestamp that shows when the job was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the transcription job.
    transcriptionJobName :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the output of the transcription job.
    transcript :: Prelude.Maybe Transcript,
    -- | A value that shows if automatic language identification was enabled for
    -- a transcription job.
    identifyLanguage :: Prelude.Maybe Prelude.Bool,
    -- | A timestamp that shows with the job was started processing.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The status of the transcription job.
    transcriptionJobStatus :: Prelude.Maybe TranscriptionJobStatus,
    -- | An object containing the details of your custom language model.
    modelSettings :: Prelude.Maybe ModelSettings,
    -- | A value between zero and one that Amazon Transcribe assigned to the
    -- language that it identified in the source audio. Larger values indicate
    -- that Amazon Transcribe has higher confidence in the language it
    -- identified.
    identifiedLanguageScore :: Prelude.Maybe Prelude.Double,
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
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The sample rate, in Hertz, of the audio track in the input media file.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural,
    -- | Provides information about how a transcription job is executed.
    jobExecutionSettings :: Prelude.Maybe JobExecutionSettings,
    -- | Optional settings for the transcription job. Use these settings to turn
    -- on speaker recognition, to set the maximum number of speakers that
    -- should be identified and to specify a custom vocabulary to use when
    -- processing the transcription job.
    settings :: Prelude.Maybe Settings,
    -- | An object that shows the optional array of languages inputted for
    -- transcription jobs with automatic language identification enabled.
    languageOptions :: Prelude.Maybe (Prelude.NonEmpty LanguageCode)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { languageCode = Prelude.Nothing,
      mediaFormat = Prelude.Nothing,
      contentRedaction = Prelude.Nothing,
      media = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      transcriptionJobName = Prelude.Nothing,
      transcript = Prelude.Nothing,
      identifyLanguage = Prelude.Nothing,
      startTime = Prelude.Nothing,
      transcriptionJobStatus = Prelude.Nothing,
      modelSettings = Prelude.Nothing,
      identifiedLanguageScore = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      mediaSampleRateHertz = Prelude.Nothing,
      jobExecutionSettings = Prelude.Nothing,
      settings = Prelude.Nothing,
      languageOptions = Prelude.Nothing
    }

-- | The language code for the input speech.
transcriptionJob_languageCode :: Lens.Lens' TranscriptionJob (Prelude.Maybe LanguageCode)
transcriptionJob_languageCode = Lens.lens (\TranscriptionJob' {languageCode} -> languageCode) (\s@TranscriptionJob' {} a -> s {languageCode = a} :: TranscriptionJob)

-- | The format of the input media file.
transcriptionJob_mediaFormat :: Lens.Lens' TranscriptionJob (Prelude.Maybe MediaFormat)
transcriptionJob_mediaFormat = Lens.lens (\TranscriptionJob' {mediaFormat} -> mediaFormat) (\s@TranscriptionJob' {} a -> s {mediaFormat = a} :: TranscriptionJob)

-- | An object that describes content redaction settings for the
-- transcription job.
transcriptionJob_contentRedaction :: Lens.Lens' TranscriptionJob (Prelude.Maybe ContentRedaction)
transcriptionJob_contentRedaction = Lens.lens (\TranscriptionJob' {contentRedaction} -> contentRedaction) (\s@TranscriptionJob' {} a -> s {contentRedaction = a} :: TranscriptionJob)

-- | An object that describes the input media for the transcription job.
transcriptionJob_media :: Lens.Lens' TranscriptionJob (Prelude.Maybe Media)
transcriptionJob_media = Lens.lens (\TranscriptionJob' {media} -> media) (\s@TranscriptionJob' {} a -> s {media = a} :: TranscriptionJob)

-- | A timestamp that shows when the job was created.
transcriptionJob_creationTime :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.UTCTime)
transcriptionJob_creationTime = Lens.lens (\TranscriptionJob' {creationTime} -> creationTime) (\s@TranscriptionJob' {} a -> s {creationTime = a} :: TranscriptionJob) Prelude.. Lens.mapping Prelude._Time

-- | A timestamp that shows when the job was completed.
transcriptionJob_completionTime :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.UTCTime)
transcriptionJob_completionTime = Lens.lens (\TranscriptionJob' {completionTime} -> completionTime) (\s@TranscriptionJob' {} a -> s {completionTime = a} :: TranscriptionJob) Prelude.. Lens.mapping Prelude._Time

-- | The name of the transcription job.
transcriptionJob_transcriptionJobName :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Text)
transcriptionJob_transcriptionJobName = Lens.lens (\TranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@TranscriptionJob' {} a -> s {transcriptionJobName = a} :: TranscriptionJob)

-- | An object that describes the output of the transcription job.
transcriptionJob_transcript :: Lens.Lens' TranscriptionJob (Prelude.Maybe Transcript)
transcriptionJob_transcript = Lens.lens (\TranscriptionJob' {transcript} -> transcript) (\s@TranscriptionJob' {} a -> s {transcript = a} :: TranscriptionJob)

-- | A value that shows if automatic language identification was enabled for
-- a transcription job.
transcriptionJob_identifyLanguage :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Bool)
transcriptionJob_identifyLanguage = Lens.lens (\TranscriptionJob' {identifyLanguage} -> identifyLanguage) (\s@TranscriptionJob' {} a -> s {identifyLanguage = a} :: TranscriptionJob)

-- | A timestamp that shows with the job was started processing.
transcriptionJob_startTime :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.UTCTime)
transcriptionJob_startTime = Lens.lens (\TranscriptionJob' {startTime} -> startTime) (\s@TranscriptionJob' {} a -> s {startTime = a} :: TranscriptionJob) Prelude.. Lens.mapping Prelude._Time

-- | The status of the transcription job.
transcriptionJob_transcriptionJobStatus :: Lens.Lens' TranscriptionJob (Prelude.Maybe TranscriptionJobStatus)
transcriptionJob_transcriptionJobStatus = Lens.lens (\TranscriptionJob' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@TranscriptionJob' {} a -> s {transcriptionJobStatus = a} :: TranscriptionJob)

-- | An object containing the details of your custom language model.
transcriptionJob_modelSettings :: Lens.Lens' TranscriptionJob (Prelude.Maybe ModelSettings)
transcriptionJob_modelSettings = Lens.lens (\TranscriptionJob' {modelSettings} -> modelSettings) (\s@TranscriptionJob' {} a -> s {modelSettings = a} :: TranscriptionJob)

-- | A value between zero and one that Amazon Transcribe assigned to the
-- language that it identified in the source audio. Larger values indicate
-- that Amazon Transcribe has higher confidence in the language it
-- identified.
transcriptionJob_identifiedLanguageScore :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Double)
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
transcriptionJob_failureReason :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Text)
transcriptionJob_failureReason = Lens.lens (\TranscriptionJob' {failureReason} -> failureReason) (\s@TranscriptionJob' {} a -> s {failureReason = a} :: TranscriptionJob)

-- | The sample rate, in Hertz, of the audio track in the input media file.
transcriptionJob_mediaSampleRateHertz :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Natural)
transcriptionJob_mediaSampleRateHertz = Lens.lens (\TranscriptionJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@TranscriptionJob' {} a -> s {mediaSampleRateHertz = a} :: TranscriptionJob)

-- | Provides information about how a transcription job is executed.
transcriptionJob_jobExecutionSettings :: Lens.Lens' TranscriptionJob (Prelude.Maybe JobExecutionSettings)
transcriptionJob_jobExecutionSettings = Lens.lens (\TranscriptionJob' {jobExecutionSettings} -> jobExecutionSettings) (\s@TranscriptionJob' {} a -> s {jobExecutionSettings = a} :: TranscriptionJob)

-- | Optional settings for the transcription job. Use these settings to turn
-- on speaker recognition, to set the maximum number of speakers that
-- should be identified and to specify a custom vocabulary to use when
-- processing the transcription job.
transcriptionJob_settings :: Lens.Lens' TranscriptionJob (Prelude.Maybe Settings)
transcriptionJob_settings = Lens.lens (\TranscriptionJob' {settings} -> settings) (\s@TranscriptionJob' {} a -> s {settings = a} :: TranscriptionJob)

-- | An object that shows the optional array of languages inputted for
-- transcription jobs with automatic language identification enabled.
transcriptionJob_languageOptions :: Lens.Lens' TranscriptionJob (Prelude.Maybe (Prelude.NonEmpty LanguageCode))
transcriptionJob_languageOptions = Lens.lens (\TranscriptionJob' {languageOptions} -> languageOptions) (\s@TranscriptionJob' {} a -> s {languageOptions = a} :: TranscriptionJob) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON TranscriptionJob where
  parseJSON =
    Prelude.withObject
      "TranscriptionJob"
      ( \x ->
          TranscriptionJob'
            Prelude.<$> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "MediaFormat")
            Prelude.<*> (x Prelude..:? "ContentRedaction")
            Prelude.<*> (x Prelude..:? "Media")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "CompletionTime")
            Prelude.<*> (x Prelude..:? "TranscriptionJobName")
            Prelude.<*> (x Prelude..:? "Transcript")
            Prelude.<*> (x Prelude..:? "IdentifyLanguage")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "TranscriptionJobStatus")
            Prelude.<*> (x Prelude..:? "ModelSettings")
            Prelude.<*> (x Prelude..:? "IdentifiedLanguageScore")
            Prelude.<*> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..:? "MediaSampleRateHertz")
            Prelude.<*> (x Prelude..:? "JobExecutionSettings")
            Prelude.<*> (x Prelude..:? "Settings")
            Prelude.<*> (x Prelude..:? "LanguageOptions")
      )

instance Prelude.Hashable TranscriptionJob

instance Prelude.NFData TranscriptionJob
