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
-- Module      : Amazonka.Transcribe.Types.CallAnalyticsJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.CallAnalyticsJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.CallAnalyticsJobSettings
import Amazonka.Transcribe.Types.CallAnalyticsJobStatus
import Amazonka.Transcribe.Types.ChannelDefinition
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.Media
import Amazonka.Transcribe.Types.MediaFormat
import Amazonka.Transcribe.Types.Transcript

-- | Describes an asynchronous analytics job that was created with the
-- @StartAnalyticsJob@ operation.
--
-- /See:/ 'newCallAnalyticsJob' smart constructor.
data CallAnalyticsJob = CallAnalyticsJob'
  { -- | A timestamp that shows when the analytics job was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | If the @AnalyticsJobStatus@ is @FAILED@, this field contains information
    -- about why the job failed.
    --
    -- The @FailureReason@ field can contain one of the following values:
    --
    -- -   @Unsupported media format@: The media format specified in the
    --     @MediaFormat@ field of the request isn\'t valid. See the description
    --     of the @MediaFormat@ field for a list of valid values.
    --
    -- -   @The media format provided does not match the detected media format@:
    --     The media format of the audio file doesn\'t match the format
    --     specified in the @MediaFormat@ field in the request. Check the media
    --     format of your media file and make sure the two values match.
    --
    -- -   @Invalid sample rate for audio file@: The sample rate specified in
    --     the @MediaSampleRateHertz@ of the request isn\'t valid. The sample
    --     rate must be between 8,000 and 48,000 Hertz.
    --
    -- -   @The sample rate provided does not match the detected sample rate@:
    --     The sample rate in the audio file doesn\'t match the sample rate
    --     specified in the @MediaSampleRateHertz@ field in the request. Check
    --     the sample rate of your media file and make sure that the two values
    --     match.
    --
    -- -   @Invalid file size: file size too large@: The size of your audio
    --     file is larger than what Amazon Transcribe Medical can process. For
    --     more information, see /Guidelines and Quotas/ in the Amazon
    --     Transcribe Medical Guide.
    --
    -- -   @Invalid number of channels: number of channels too large@: Your
    --     audio contains more channels than Amazon Transcribe Medical is
    --     configured to process. To request additional channels, see Amazon
    --     Transcribe Medical Endpoints and Quotas in the
    --     <https://docs.aws.amazon.com/general/latest/gr/Welcome.html Amazon Web Services General Reference>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the analytics job.
    callAnalyticsJobStatus :: Prelude.Maybe CallAnalyticsJobStatus,
    -- | A value between zero and one that Amazon Transcribe assigned to the
    -- language that it identified in the source audio. This value appears only
    -- when you don\'t provide a single language code. Larger values indicate
    -- that Amazon Transcribe has higher confidence in the language that it
    -- identified
    identifiedLanguageScore :: Prelude.Maybe Prelude.Double,
    -- | If you know the language spoken between the customer and the agent,
    -- specify a language code for this field.
    --
    -- If you don\'t know the language, you can leave this field blank, and
    -- Amazon Transcribe will use machine learning to automatically identify
    -- the language. To improve the accuracy of language identification, you
    -- can provide an array containing the possible language codes for the
    -- language spoken in your audio. Refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html Supported languages and language-specific features>
    -- for additional information.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Provides information about the settings used to run a transcription job.
    settings :: Prelude.Maybe CallAnalyticsJobSettings,
    -- | A timestamp that shows when the analytics job started processing.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | A timestamp that shows when the analytics job was completed.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the call analytics job.
    callAnalyticsJobName :: Prelude.Maybe Prelude.Text,
    media :: Prelude.Maybe Media,
    -- | The format of the input audio file. Note: for call analytics jobs, only
    -- the following media formats are supported: MP3, MP4, WAV, FLAC, OGG, and
    -- WebM.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | Shows numeric values to indicate the channel assigned to the agent\'s
    -- audio and the channel assigned to the customer\'s audio.
    channelDefinitions :: Prelude.Maybe (Prelude.NonEmpty ChannelDefinition),
    -- | The Amazon Resource Number (ARN) that you use to get access to the
    -- analytics job.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    transcript :: Prelude.Maybe Transcript,
    -- | The sample rate, in Hertz, of the audio.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CallAnalyticsJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'callAnalyticsJob_creationTime' - A timestamp that shows when the analytics job was created.
--
-- 'failureReason', 'callAnalyticsJob_failureReason' - If the @AnalyticsJobStatus@ is @FAILED@, this field contains information
-- about why the job failed.
--
-- The @FailureReason@ field can contain one of the following values:
--
-- -   @Unsupported media format@: The media format specified in the
--     @MediaFormat@ field of the request isn\'t valid. See the description
--     of the @MediaFormat@ field for a list of valid values.
--
-- -   @The media format provided does not match the detected media format@:
--     The media format of the audio file doesn\'t match the format
--     specified in the @MediaFormat@ field in the request. Check the media
--     format of your media file and make sure the two values match.
--
-- -   @Invalid sample rate for audio file@: The sample rate specified in
--     the @MediaSampleRateHertz@ of the request isn\'t valid. The sample
--     rate must be between 8,000 and 48,000 Hertz.
--
-- -   @The sample rate provided does not match the detected sample rate@:
--     The sample rate in the audio file doesn\'t match the sample rate
--     specified in the @MediaSampleRateHertz@ field in the request. Check
--     the sample rate of your media file and make sure that the two values
--     match.
--
-- -   @Invalid file size: file size too large@: The size of your audio
--     file is larger than what Amazon Transcribe Medical can process. For
--     more information, see /Guidelines and Quotas/ in the Amazon
--     Transcribe Medical Guide.
--
-- -   @Invalid number of channels: number of channels too large@: Your
--     audio contains more channels than Amazon Transcribe Medical is
--     configured to process. To request additional channels, see Amazon
--     Transcribe Medical Endpoints and Quotas in the
--     <https://docs.aws.amazon.com/general/latest/gr/Welcome.html Amazon Web Services General Reference>.
--
-- 'callAnalyticsJobStatus', 'callAnalyticsJob_callAnalyticsJobStatus' - The status of the analytics job.
--
-- 'identifiedLanguageScore', 'callAnalyticsJob_identifiedLanguageScore' - A value between zero and one that Amazon Transcribe assigned to the
-- language that it identified in the source audio. This value appears only
-- when you don\'t provide a single language code. Larger values indicate
-- that Amazon Transcribe has higher confidence in the language that it
-- identified
--
-- 'languageCode', 'callAnalyticsJob_languageCode' - If you know the language spoken between the customer and the agent,
-- specify a language code for this field.
--
-- If you don\'t know the language, you can leave this field blank, and
-- Amazon Transcribe will use machine learning to automatically identify
-- the language. To improve the accuracy of language identification, you
-- can provide an array containing the possible language codes for the
-- language spoken in your audio. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html Supported languages and language-specific features>
-- for additional information.
--
-- 'settings', 'callAnalyticsJob_settings' - Provides information about the settings used to run a transcription job.
--
-- 'startTime', 'callAnalyticsJob_startTime' - A timestamp that shows when the analytics job started processing.
--
-- 'completionTime', 'callAnalyticsJob_completionTime' - A timestamp that shows when the analytics job was completed.
--
-- 'callAnalyticsJobName', 'callAnalyticsJob_callAnalyticsJobName' - The name of the call analytics job.
--
-- 'media', 'callAnalyticsJob_media' - Undocumented member.
--
-- 'mediaFormat', 'callAnalyticsJob_mediaFormat' - The format of the input audio file. Note: for call analytics jobs, only
-- the following media formats are supported: MP3, MP4, WAV, FLAC, OGG, and
-- WebM.
--
-- 'channelDefinitions', 'callAnalyticsJob_channelDefinitions' - Shows numeric values to indicate the channel assigned to the agent\'s
-- audio and the channel assigned to the customer\'s audio.
--
-- 'dataAccessRoleArn', 'callAnalyticsJob_dataAccessRoleArn' - The Amazon Resource Number (ARN) that you use to get access to the
-- analytics job.
--
-- 'transcript', 'callAnalyticsJob_transcript' - Undocumented member.
--
-- 'mediaSampleRateHertz', 'callAnalyticsJob_mediaSampleRateHertz' - The sample rate, in Hertz, of the audio.
newCallAnalyticsJob ::
  CallAnalyticsJob
newCallAnalyticsJob =
  CallAnalyticsJob'
    { creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      callAnalyticsJobStatus = Prelude.Nothing,
      identifiedLanguageScore = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      settings = Prelude.Nothing,
      startTime = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      callAnalyticsJobName = Prelude.Nothing,
      media = Prelude.Nothing,
      mediaFormat = Prelude.Nothing,
      channelDefinitions = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      transcript = Prelude.Nothing,
      mediaSampleRateHertz = Prelude.Nothing
    }

-- | A timestamp that shows when the analytics job was created.
callAnalyticsJob_creationTime :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJob_creationTime = Lens.lens (\CallAnalyticsJob' {creationTime} -> creationTime) (\s@CallAnalyticsJob' {} a -> s {creationTime = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Core._Time

-- | If the @AnalyticsJobStatus@ is @FAILED@, this field contains information
-- about why the job failed.
--
-- The @FailureReason@ field can contain one of the following values:
--
-- -   @Unsupported media format@: The media format specified in the
--     @MediaFormat@ field of the request isn\'t valid. See the description
--     of the @MediaFormat@ field for a list of valid values.
--
-- -   @The media format provided does not match the detected media format@:
--     The media format of the audio file doesn\'t match the format
--     specified in the @MediaFormat@ field in the request. Check the media
--     format of your media file and make sure the two values match.
--
-- -   @Invalid sample rate for audio file@: The sample rate specified in
--     the @MediaSampleRateHertz@ of the request isn\'t valid. The sample
--     rate must be between 8,000 and 48,000 Hertz.
--
-- -   @The sample rate provided does not match the detected sample rate@:
--     The sample rate in the audio file doesn\'t match the sample rate
--     specified in the @MediaSampleRateHertz@ field in the request. Check
--     the sample rate of your media file and make sure that the two values
--     match.
--
-- -   @Invalid file size: file size too large@: The size of your audio
--     file is larger than what Amazon Transcribe Medical can process. For
--     more information, see /Guidelines and Quotas/ in the Amazon
--     Transcribe Medical Guide.
--
-- -   @Invalid number of channels: number of channels too large@: Your
--     audio contains more channels than Amazon Transcribe Medical is
--     configured to process. To request additional channels, see Amazon
--     Transcribe Medical Endpoints and Quotas in the
--     <https://docs.aws.amazon.com/general/latest/gr/Welcome.html Amazon Web Services General Reference>.
callAnalyticsJob_failureReason :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Text)
callAnalyticsJob_failureReason = Lens.lens (\CallAnalyticsJob' {failureReason} -> failureReason) (\s@CallAnalyticsJob' {} a -> s {failureReason = a} :: CallAnalyticsJob)

-- | The status of the analytics job.
callAnalyticsJob_callAnalyticsJobStatus :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe CallAnalyticsJobStatus)
callAnalyticsJob_callAnalyticsJobStatus = Lens.lens (\CallAnalyticsJob' {callAnalyticsJobStatus} -> callAnalyticsJobStatus) (\s@CallAnalyticsJob' {} a -> s {callAnalyticsJobStatus = a} :: CallAnalyticsJob)

-- | A value between zero and one that Amazon Transcribe assigned to the
-- language that it identified in the source audio. This value appears only
-- when you don\'t provide a single language code. Larger values indicate
-- that Amazon Transcribe has higher confidence in the language that it
-- identified
callAnalyticsJob_identifiedLanguageScore :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Double)
callAnalyticsJob_identifiedLanguageScore = Lens.lens (\CallAnalyticsJob' {identifiedLanguageScore} -> identifiedLanguageScore) (\s@CallAnalyticsJob' {} a -> s {identifiedLanguageScore = a} :: CallAnalyticsJob)

-- | If you know the language spoken between the customer and the agent,
-- specify a language code for this field.
--
-- If you don\'t know the language, you can leave this field blank, and
-- Amazon Transcribe will use machine learning to automatically identify
-- the language. To improve the accuracy of language identification, you
-- can provide an array containing the possible language codes for the
-- language spoken in your audio. Refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html Supported languages and language-specific features>
-- for additional information.
callAnalyticsJob_languageCode :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe LanguageCode)
callAnalyticsJob_languageCode = Lens.lens (\CallAnalyticsJob' {languageCode} -> languageCode) (\s@CallAnalyticsJob' {} a -> s {languageCode = a} :: CallAnalyticsJob)

-- | Provides information about the settings used to run a transcription job.
callAnalyticsJob_settings :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe CallAnalyticsJobSettings)
callAnalyticsJob_settings = Lens.lens (\CallAnalyticsJob' {settings} -> settings) (\s@CallAnalyticsJob' {} a -> s {settings = a} :: CallAnalyticsJob)

-- | A timestamp that shows when the analytics job started processing.
callAnalyticsJob_startTime :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJob_startTime = Lens.lens (\CallAnalyticsJob' {startTime} -> startTime) (\s@CallAnalyticsJob' {} a -> s {startTime = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Core._Time

-- | A timestamp that shows when the analytics job was completed.
callAnalyticsJob_completionTime :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJob_completionTime = Lens.lens (\CallAnalyticsJob' {completionTime} -> completionTime) (\s@CallAnalyticsJob' {} a -> s {completionTime = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Core._Time

-- | The name of the call analytics job.
callAnalyticsJob_callAnalyticsJobName :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Text)
callAnalyticsJob_callAnalyticsJobName = Lens.lens (\CallAnalyticsJob' {callAnalyticsJobName} -> callAnalyticsJobName) (\s@CallAnalyticsJob' {} a -> s {callAnalyticsJobName = a} :: CallAnalyticsJob)

-- | Undocumented member.
callAnalyticsJob_media :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Media)
callAnalyticsJob_media = Lens.lens (\CallAnalyticsJob' {media} -> media) (\s@CallAnalyticsJob' {} a -> s {media = a} :: CallAnalyticsJob)

-- | The format of the input audio file. Note: for call analytics jobs, only
-- the following media formats are supported: MP3, MP4, WAV, FLAC, OGG, and
-- WebM.
callAnalyticsJob_mediaFormat :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe MediaFormat)
callAnalyticsJob_mediaFormat = Lens.lens (\CallAnalyticsJob' {mediaFormat} -> mediaFormat) (\s@CallAnalyticsJob' {} a -> s {mediaFormat = a} :: CallAnalyticsJob)

-- | Shows numeric values to indicate the channel assigned to the agent\'s
-- audio and the channel assigned to the customer\'s audio.
callAnalyticsJob_channelDefinitions :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe (Prelude.NonEmpty ChannelDefinition))
callAnalyticsJob_channelDefinitions = Lens.lens (\CallAnalyticsJob' {channelDefinitions} -> channelDefinitions) (\s@CallAnalyticsJob' {} a -> s {channelDefinitions = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Number (ARN) that you use to get access to the
-- analytics job.
callAnalyticsJob_dataAccessRoleArn :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Text)
callAnalyticsJob_dataAccessRoleArn = Lens.lens (\CallAnalyticsJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@CallAnalyticsJob' {} a -> s {dataAccessRoleArn = a} :: CallAnalyticsJob)

-- | Undocumented member.
callAnalyticsJob_transcript :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Transcript)
callAnalyticsJob_transcript = Lens.lens (\CallAnalyticsJob' {transcript} -> transcript) (\s@CallAnalyticsJob' {} a -> s {transcript = a} :: CallAnalyticsJob)

-- | The sample rate, in Hertz, of the audio.
callAnalyticsJob_mediaSampleRateHertz :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Natural)
callAnalyticsJob_mediaSampleRateHertz = Lens.lens (\CallAnalyticsJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@CallAnalyticsJob' {} a -> s {mediaSampleRateHertz = a} :: CallAnalyticsJob)

instance Core.FromJSON CallAnalyticsJob where
  parseJSON =
    Core.withObject
      "CallAnalyticsJob"
      ( \x ->
          CallAnalyticsJob'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "CallAnalyticsJobStatus")
            Prelude.<*> (x Core..:? "IdentifiedLanguageScore")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "Settings")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "CompletionTime")
            Prelude.<*> (x Core..:? "CallAnalyticsJobName")
            Prelude.<*> (x Core..:? "Media")
            Prelude.<*> (x Core..:? "MediaFormat")
            Prelude.<*> (x Core..:? "ChannelDefinitions")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "Transcript")
            Prelude.<*> (x Core..:? "MediaSampleRateHertz")
      )

instance Prelude.Hashable CallAnalyticsJob where
  hashWithSalt _salt CallAnalyticsJob' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` callAnalyticsJobStatus
      `Prelude.hashWithSalt` identifiedLanguageScore
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` callAnalyticsJobName
      `Prelude.hashWithSalt` media
      `Prelude.hashWithSalt` mediaFormat
      `Prelude.hashWithSalt` channelDefinitions
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` transcript
      `Prelude.hashWithSalt` mediaSampleRateHertz

instance Prelude.NFData CallAnalyticsJob where
  rnf CallAnalyticsJob' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf callAnalyticsJobStatus
      `Prelude.seq` Prelude.rnf identifiedLanguageScore
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf callAnalyticsJobName
      `Prelude.seq` Prelude.rnf media
      `Prelude.seq` Prelude.rnf mediaFormat
      `Prelude.seq` Prelude.rnf channelDefinitions
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf transcript
      `Prelude.seq` Prelude.rnf mediaSampleRateHertz
