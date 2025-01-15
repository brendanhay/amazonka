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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.CallAnalyticsJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.CallAnalyticsJobSettings
import Amazonka.Transcribe.Types.CallAnalyticsJobStatus
import Amazonka.Transcribe.Types.ChannelDefinition
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.Media
import Amazonka.Transcribe.Types.MediaFormat
import Amazonka.Transcribe.Types.Transcript

-- | Provides detailed information about a Call Analytics job.
--
-- To view the job\'s status, refer to @CallAnalyticsJobStatus@. If the
-- status is @COMPLETED@, the job is finished. You can find your completed
-- transcript at the URI specified in @TranscriptFileUri@. If the status is
-- @FAILED@, @FailureReason@ provides details on why your transcription job
-- failed.
--
-- If you enabled personally identifiable information (PII) redaction, the
-- redacted transcript appears at the location specified in
-- @RedactedTranscriptFileUri@.
--
-- If you chose to redact the audio in your media file, you can find your
-- redacted media file at the location specified in the
-- @RedactedMediaFileUri@ field of your response.
--
-- /See:/ 'newCallAnalyticsJob' smart constructor.
data CallAnalyticsJob = CallAnalyticsJob'
  { -- | The name of the Call Analytics job. Job names are case sensitive and
    -- must be unique within an Amazon Web Services account.
    callAnalyticsJobName :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of the specified Call Analytics job.
    --
    -- If the status is @COMPLETED@, the job is finished and you can find the
    -- results at the location specified in @TranscriptFileUri@ (or
    -- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
    -- the status is @FAILED@, @FailureReason@ provides details on why your
    -- transcription job failed.
    callAnalyticsJobStatus :: Prelude.Maybe CallAnalyticsJobStatus,
    -- | Indicates which speaker is on which channel.
    channelDefinitions :: Prelude.Maybe (Prelude.NonEmpty ChannelDefinition),
    -- | The date and time the specified Call Analytics job finished processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
    -- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time the specified Call Analytics job request was made.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) you included in your request.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | If @CallAnalyticsJobStatus@ is @FAILED@, @FailureReason@ contains
    -- information about why the Call Analytics job request failed.
    --
    -- The @FailureReason@ field contains one of the following values:
    --
    -- -   @Unsupported media format@.
    --
    --     The media format specified in @MediaFormat@ isn\'t valid. Refer to
    --     __MediaFormat__ for a list of supported formats.
    --
    -- -   @The media format provided does not match the detected media format@.
    --
    --     The media format specified in @MediaFormat@ doesn\'t match the
    --     format of the input file. Check the media format of your media file
    --     and correct the specified value.
    --
    -- -   @Invalid sample rate for audio file@.
    --
    --     The sample rate specified in @MediaSampleRateHertz@ isn\'t valid.
    --     The sample rate must be between 8,000 and 48,000 hertz.
    --
    -- -   @The sample rate provided does not match the detected sample rate@.
    --
    --     The sample rate specified in @MediaSampleRateHertz@ doesn\'t match
    --     the sample rate detected in your input media file. Check the sample
    --     rate of your media file and correct the specified value.
    --
    -- -   @Invalid file size: file size too large@.
    --
    --     The size of your media file is larger than what Amazon Transcribe
    --     can process. For more information, refer to
    --     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and quotas>.
    --
    -- -   @Invalid number of channels: number of channels too large@.
    --
    --     Your audio contains more channels than Amazon Transcribe is able to
    --     process. For more information, refer to
    --     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and quotas>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The confidence score associated with the language identified in your
    -- media file.
    --
    -- Confidence scores are values between 0 and 1; a larger value indicates a
    -- higher probability that the identified language correctly matches the
    -- language spoken in your media.
    identifiedLanguageScore :: Prelude.Maybe Prelude.Double,
    -- | The language code used to create your Call Analytics job. For a list of
    -- supported languages and their associated language codes, refer to the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
    -- table.
    --
    -- If you don\'t know the language spoken in your media file, you can omit
    -- this field and let Amazon Transcribe automatically identify the language
    -- of your media. To improve the accuracy of language identification, you
    -- can include several language codes and Amazon Transcribe chooses the
    -- closest match for your transcription.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Provides the Amazon S3 location of the media file you used in your Call
    -- Analytics request.
    media :: Prelude.Maybe Media,
    -- | The format of the input media file.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | The sample rate, in hertz, of the audio track in your input media file.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural,
    -- | Provides information on any additional settings that were included in
    -- your request. Additional settings include content redaction and language
    -- identification settings.
    settings :: Prelude.Maybe CallAnalyticsJobSettings,
    -- | The date and time the specified Call Analytics job began processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    startTime :: Prelude.Maybe Data.POSIX,
    transcript :: Prelude.Maybe Transcript
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
-- 'callAnalyticsJobName', 'callAnalyticsJob_callAnalyticsJobName' - The name of the Call Analytics job. Job names are case sensitive and
-- must be unique within an Amazon Web Services account.
--
-- 'callAnalyticsJobStatus', 'callAnalyticsJob_callAnalyticsJobStatus' - Provides the status of the specified Call Analytics job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@ (or
-- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
-- the status is @FAILED@, @FailureReason@ provides details on why your
-- transcription job failed.
--
-- 'channelDefinitions', 'callAnalyticsJob_channelDefinitions' - Indicates which speaker is on which channel.
--
-- 'completionTime', 'callAnalyticsJob_completionTime' - The date and time the specified Call Analytics job finished processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
--
-- 'creationTime', 'callAnalyticsJob_creationTime' - The date and time the specified Call Analytics job request was made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'dataAccessRoleArn', 'callAnalyticsJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) you included in your request.
--
-- 'failureReason', 'callAnalyticsJob_failureReason' - If @CallAnalyticsJobStatus@ is @FAILED@, @FailureReason@ contains
-- information about why the Call Analytics job request failed.
--
-- The @FailureReason@ field contains one of the following values:
--
-- -   @Unsupported media format@.
--
--     The media format specified in @MediaFormat@ isn\'t valid. Refer to
--     __MediaFormat__ for a list of supported formats.
--
-- -   @The media format provided does not match the detected media format@.
--
--     The media format specified in @MediaFormat@ doesn\'t match the
--     format of the input file. Check the media format of your media file
--     and correct the specified value.
--
-- -   @Invalid sample rate for audio file@.
--
--     The sample rate specified in @MediaSampleRateHertz@ isn\'t valid.
--     The sample rate must be between 8,000 and 48,000 hertz.
--
-- -   @The sample rate provided does not match the detected sample rate@.
--
--     The sample rate specified in @MediaSampleRateHertz@ doesn\'t match
--     the sample rate detected in your input media file. Check the sample
--     rate of your media file and correct the specified value.
--
-- -   @Invalid file size: file size too large@.
--
--     The size of your media file is larger than what Amazon Transcribe
--     can process. For more information, refer to
--     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and quotas>.
--
-- -   @Invalid number of channels: number of channels too large@.
--
--     Your audio contains more channels than Amazon Transcribe is able to
--     process. For more information, refer to
--     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and quotas>.
--
-- 'identifiedLanguageScore', 'callAnalyticsJob_identifiedLanguageScore' - The confidence score associated with the language identified in your
-- media file.
--
-- Confidence scores are values between 0 and 1; a larger value indicates a
-- higher probability that the identified language correctly matches the
-- language spoken in your media.
--
-- 'languageCode', 'callAnalyticsJob_languageCode' - The language code used to create your Call Analytics job. For a list of
-- supported languages and their associated language codes, refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
--
-- If you don\'t know the language spoken in your media file, you can omit
-- this field and let Amazon Transcribe automatically identify the language
-- of your media. To improve the accuracy of language identification, you
-- can include several language codes and Amazon Transcribe chooses the
-- closest match for your transcription.
--
-- 'media', 'callAnalyticsJob_media' - Provides the Amazon S3 location of the media file you used in your Call
-- Analytics request.
--
-- 'mediaFormat', 'callAnalyticsJob_mediaFormat' - The format of the input media file.
--
-- 'mediaSampleRateHertz', 'callAnalyticsJob_mediaSampleRateHertz' - The sample rate, in hertz, of the audio track in your input media file.
--
-- 'settings', 'callAnalyticsJob_settings' - Provides information on any additional settings that were included in
-- your request. Additional settings include content redaction and language
-- identification settings.
--
-- 'startTime', 'callAnalyticsJob_startTime' - The date and time the specified Call Analytics job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'transcript', 'callAnalyticsJob_transcript' - Undocumented member.
newCallAnalyticsJob ::
  CallAnalyticsJob
newCallAnalyticsJob =
  CallAnalyticsJob'
    { callAnalyticsJobName =
        Prelude.Nothing,
      callAnalyticsJobStatus = Prelude.Nothing,
      channelDefinitions = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      identifiedLanguageScore = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      media = Prelude.Nothing,
      mediaFormat = Prelude.Nothing,
      mediaSampleRateHertz = Prelude.Nothing,
      settings = Prelude.Nothing,
      startTime = Prelude.Nothing,
      transcript = Prelude.Nothing
    }

-- | The name of the Call Analytics job. Job names are case sensitive and
-- must be unique within an Amazon Web Services account.
callAnalyticsJob_callAnalyticsJobName :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Text)
callAnalyticsJob_callAnalyticsJobName = Lens.lens (\CallAnalyticsJob' {callAnalyticsJobName} -> callAnalyticsJobName) (\s@CallAnalyticsJob' {} a -> s {callAnalyticsJobName = a} :: CallAnalyticsJob)

-- | Provides the status of the specified Call Analytics job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@ (or
-- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
-- the status is @FAILED@, @FailureReason@ provides details on why your
-- transcription job failed.
callAnalyticsJob_callAnalyticsJobStatus :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe CallAnalyticsJobStatus)
callAnalyticsJob_callAnalyticsJobStatus = Lens.lens (\CallAnalyticsJob' {callAnalyticsJobStatus} -> callAnalyticsJobStatus) (\s@CallAnalyticsJob' {} a -> s {callAnalyticsJobStatus = a} :: CallAnalyticsJob)

-- | Indicates which speaker is on which channel.
callAnalyticsJob_channelDefinitions :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe (Prelude.NonEmpty ChannelDefinition))
callAnalyticsJob_channelDefinitions = Lens.lens (\CallAnalyticsJob' {channelDefinitions} -> channelDefinitions) (\s@CallAnalyticsJob' {} a -> s {channelDefinitions = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the specified Call Analytics job finished processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
callAnalyticsJob_completionTime :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJob_completionTime = Lens.lens (\CallAnalyticsJob' {completionTime} -> completionTime) (\s@CallAnalyticsJob' {} a -> s {completionTime = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Data._Time

-- | The date and time the specified Call Analytics job request was made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
callAnalyticsJob_creationTime :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJob_creationTime = Lens.lens (\CallAnalyticsJob' {creationTime} -> creationTime) (\s@CallAnalyticsJob' {} a -> s {creationTime = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) you included in your request.
callAnalyticsJob_dataAccessRoleArn :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Text)
callAnalyticsJob_dataAccessRoleArn = Lens.lens (\CallAnalyticsJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@CallAnalyticsJob' {} a -> s {dataAccessRoleArn = a} :: CallAnalyticsJob)

-- | If @CallAnalyticsJobStatus@ is @FAILED@, @FailureReason@ contains
-- information about why the Call Analytics job request failed.
--
-- The @FailureReason@ field contains one of the following values:
--
-- -   @Unsupported media format@.
--
--     The media format specified in @MediaFormat@ isn\'t valid. Refer to
--     __MediaFormat__ for a list of supported formats.
--
-- -   @The media format provided does not match the detected media format@.
--
--     The media format specified in @MediaFormat@ doesn\'t match the
--     format of the input file. Check the media format of your media file
--     and correct the specified value.
--
-- -   @Invalid sample rate for audio file@.
--
--     The sample rate specified in @MediaSampleRateHertz@ isn\'t valid.
--     The sample rate must be between 8,000 and 48,000 hertz.
--
-- -   @The sample rate provided does not match the detected sample rate@.
--
--     The sample rate specified in @MediaSampleRateHertz@ doesn\'t match
--     the sample rate detected in your input media file. Check the sample
--     rate of your media file and correct the specified value.
--
-- -   @Invalid file size: file size too large@.
--
--     The size of your media file is larger than what Amazon Transcribe
--     can process. For more information, refer to
--     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and quotas>.
--
-- -   @Invalid number of channels: number of channels too large@.
--
--     Your audio contains more channels than Amazon Transcribe is able to
--     process. For more information, refer to
--     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and quotas>.
callAnalyticsJob_failureReason :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Text)
callAnalyticsJob_failureReason = Lens.lens (\CallAnalyticsJob' {failureReason} -> failureReason) (\s@CallAnalyticsJob' {} a -> s {failureReason = a} :: CallAnalyticsJob)

-- | The confidence score associated with the language identified in your
-- media file.
--
-- Confidence scores are values between 0 and 1; a larger value indicates a
-- higher probability that the identified language correctly matches the
-- language spoken in your media.
callAnalyticsJob_identifiedLanguageScore :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Double)
callAnalyticsJob_identifiedLanguageScore = Lens.lens (\CallAnalyticsJob' {identifiedLanguageScore} -> identifiedLanguageScore) (\s@CallAnalyticsJob' {} a -> s {identifiedLanguageScore = a} :: CallAnalyticsJob)

-- | The language code used to create your Call Analytics job. For a list of
-- supported languages and their associated language codes, refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
--
-- If you don\'t know the language spoken in your media file, you can omit
-- this field and let Amazon Transcribe automatically identify the language
-- of your media. To improve the accuracy of language identification, you
-- can include several language codes and Amazon Transcribe chooses the
-- closest match for your transcription.
callAnalyticsJob_languageCode :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe LanguageCode)
callAnalyticsJob_languageCode = Lens.lens (\CallAnalyticsJob' {languageCode} -> languageCode) (\s@CallAnalyticsJob' {} a -> s {languageCode = a} :: CallAnalyticsJob)

-- | Provides the Amazon S3 location of the media file you used in your Call
-- Analytics request.
callAnalyticsJob_media :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Media)
callAnalyticsJob_media = Lens.lens (\CallAnalyticsJob' {media} -> media) (\s@CallAnalyticsJob' {} a -> s {media = a} :: CallAnalyticsJob)

-- | The format of the input media file.
callAnalyticsJob_mediaFormat :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe MediaFormat)
callAnalyticsJob_mediaFormat = Lens.lens (\CallAnalyticsJob' {mediaFormat} -> mediaFormat) (\s@CallAnalyticsJob' {} a -> s {mediaFormat = a} :: CallAnalyticsJob)

-- | The sample rate, in hertz, of the audio track in your input media file.
callAnalyticsJob_mediaSampleRateHertz :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Natural)
callAnalyticsJob_mediaSampleRateHertz = Lens.lens (\CallAnalyticsJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@CallAnalyticsJob' {} a -> s {mediaSampleRateHertz = a} :: CallAnalyticsJob)

-- | Provides information on any additional settings that were included in
-- your request. Additional settings include content redaction and language
-- identification settings.
callAnalyticsJob_settings :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe CallAnalyticsJobSettings)
callAnalyticsJob_settings = Lens.lens (\CallAnalyticsJob' {settings} -> settings) (\s@CallAnalyticsJob' {} a -> s {settings = a} :: CallAnalyticsJob)

-- | The date and time the specified Call Analytics job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
callAnalyticsJob_startTime :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJob_startTime = Lens.lens (\CallAnalyticsJob' {startTime} -> startTime) (\s@CallAnalyticsJob' {} a -> s {startTime = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
callAnalyticsJob_transcript :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Transcript)
callAnalyticsJob_transcript = Lens.lens (\CallAnalyticsJob' {transcript} -> transcript) (\s@CallAnalyticsJob' {} a -> s {transcript = a} :: CallAnalyticsJob)

instance Data.FromJSON CallAnalyticsJob where
  parseJSON =
    Data.withObject
      "CallAnalyticsJob"
      ( \x ->
          CallAnalyticsJob'
            Prelude.<$> (x Data..:? "CallAnalyticsJobName")
            Prelude.<*> (x Data..:? "CallAnalyticsJobStatus")
            Prelude.<*> (x Data..:? "ChannelDefinitions")
            Prelude.<*> (x Data..:? "CompletionTime")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "IdentifiedLanguageScore")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "Media")
            Prelude.<*> (x Data..:? "MediaFormat")
            Prelude.<*> (x Data..:? "MediaSampleRateHertz")
            Prelude.<*> (x Data..:? "Settings")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Transcript")
      )

instance Prelude.Hashable CallAnalyticsJob where
  hashWithSalt _salt CallAnalyticsJob' {..} =
    _salt
      `Prelude.hashWithSalt` callAnalyticsJobName
      `Prelude.hashWithSalt` callAnalyticsJobStatus
      `Prelude.hashWithSalt` channelDefinitions
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` identifiedLanguageScore
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` media
      `Prelude.hashWithSalt` mediaFormat
      `Prelude.hashWithSalt` mediaSampleRateHertz
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` transcript

instance Prelude.NFData CallAnalyticsJob where
  rnf CallAnalyticsJob' {..} =
    Prelude.rnf callAnalyticsJobName `Prelude.seq`
      Prelude.rnf callAnalyticsJobStatus `Prelude.seq`
        Prelude.rnf channelDefinitions `Prelude.seq`
          Prelude.rnf completionTime `Prelude.seq`
            Prelude.rnf creationTime `Prelude.seq`
              Prelude.rnf dataAccessRoleArn `Prelude.seq`
                Prelude.rnf failureReason `Prelude.seq`
                  Prelude.rnf identifiedLanguageScore `Prelude.seq`
                    Prelude.rnf languageCode `Prelude.seq`
                      Prelude.rnf media `Prelude.seq`
                        Prelude.rnf mediaFormat `Prelude.seq`
                          Prelude.rnf mediaSampleRateHertz `Prelude.seq`
                            Prelude.rnf settings `Prelude.seq`
                              Prelude.rnf startTime `Prelude.seq`
                                Prelude.rnf transcript
