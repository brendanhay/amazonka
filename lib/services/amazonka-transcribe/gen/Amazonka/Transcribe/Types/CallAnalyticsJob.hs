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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.CallAnalyticsJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { transcript :: Prelude.Maybe Transcript,
    -- | The format of the input media file.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | Provides the status of the specified Call Analytics job.
    --
    -- If the status is @COMPLETED@, the job is finished and you can find the
    -- results at the location specified in @TranscriptFileUri@ (or
    -- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
    -- the status is @FAILED@, @FailureReason@ provides details on why your
    -- transcription job failed.
    callAnalyticsJobStatus :: Prelude.Maybe CallAnalyticsJobStatus,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permissions to
    -- access the Amazon S3 bucket that contains your input files. If the role
    -- you specify doesn’t have the appropriate permissions to access the
    -- specified Amazon S3 location, your request fails.
    --
    -- IAM role ARNs have the format
    -- @arn:partition:iam::account:role\/role-name-with-path@. For example:
    -- @arn:aws:iam::111122223333:role\/Admin@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time the specified Call Analytics job finished processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
    -- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | Allows additional optional settings in your request, including content
    -- redaction; allows you to apply custom language models, vocabulary
    -- filters, and custom vocabularies to your Call Analytics job.
    settings :: Prelude.Maybe CallAnalyticsJobSettings,
    -- | The sample rate, in Hertz, of the audio track in your input media file.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural,
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
    -- | The name of the Call Analytics job. Job names are case sensitive and
    -- must be unique within an Amazon Web Services account.
    callAnalyticsJobName :: Prelude.Maybe Prelude.Text,
    -- | The date and time the specified Call Analytics job request was made.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The confidence score associated with the language identified in your
    -- media file.
    --
    -- Confidence scores are values between 0 and 1; a larger value indicates a
    -- higher probability that the identified language correctly matches the
    -- language spoken in your media.
    identifiedLanguageScore :: Prelude.Maybe Prelude.Double,
    -- | The date and time the specified Call Analytics job began processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    startTime :: Prelude.Maybe Core.POSIX,
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
    --     The sample rate must be between 8,000 and 48,000 Hertz.
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
    media :: Prelude.Maybe Media,
    -- | Allows you to specify which speaker is on which channel in your Call
    -- Analytics job request. For example, if your agent is the first
    -- participant to speak, you would set @ChannelId@ to @0@ (to indicate the
    -- first channel) and @ParticipantRole@ to @AGENT@ (to indicate that it\'s
    -- the agent speaking).
    channelDefinitions :: Prelude.Maybe (Prelude.NonEmpty ChannelDefinition)
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
-- 'transcript', 'callAnalyticsJob_transcript' - Undocumented member.
--
-- 'mediaFormat', 'callAnalyticsJob_mediaFormat' - The format of the input media file.
--
-- 'callAnalyticsJobStatus', 'callAnalyticsJob_callAnalyticsJobStatus' - Provides the status of the specified Call Analytics job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@ (or
-- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
-- the status is @FAILED@, @FailureReason@ provides details on why your
-- transcription job failed.
--
-- 'dataAccessRoleArn', 'callAnalyticsJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of an IAM role that has permissions to
-- access the Amazon S3 bucket that contains your input files. If the role
-- you specify doesn’t have the appropriate permissions to access the
-- specified Amazon S3 location, your request fails.
--
-- IAM role ARNs have the format
-- @arn:partition:iam::account:role\/role-name-with-path@. For example:
-- @arn:aws:iam::111122223333:role\/Admin@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
--
-- 'completionTime', 'callAnalyticsJob_completionTime' - The date and time the specified Call Analytics job finished processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
--
-- 'settings', 'callAnalyticsJob_settings' - Allows additional optional settings in your request, including content
-- redaction; allows you to apply custom language models, vocabulary
-- filters, and custom vocabularies to your Call Analytics job.
--
-- 'mediaSampleRateHertz', 'callAnalyticsJob_mediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in your input media file.
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
-- 'callAnalyticsJobName', 'callAnalyticsJob_callAnalyticsJobName' - The name of the Call Analytics job. Job names are case sensitive and
-- must be unique within an Amazon Web Services account.
--
-- 'creationTime', 'callAnalyticsJob_creationTime' - The date and time the specified Call Analytics job request was made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'identifiedLanguageScore', 'callAnalyticsJob_identifiedLanguageScore' - The confidence score associated with the language identified in your
-- media file.
--
-- Confidence scores are values between 0 and 1; a larger value indicates a
-- higher probability that the identified language correctly matches the
-- language spoken in your media.
--
-- 'startTime', 'callAnalyticsJob_startTime' - The date and time the specified Call Analytics job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
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
--     The sample rate must be between 8,000 and 48,000 Hertz.
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
-- 'media', 'callAnalyticsJob_media' - Undocumented member.
--
-- 'channelDefinitions', 'callAnalyticsJob_channelDefinitions' - Allows you to specify which speaker is on which channel in your Call
-- Analytics job request. For example, if your agent is the first
-- participant to speak, you would set @ChannelId@ to @0@ (to indicate the
-- first channel) and @ParticipantRole@ to @AGENT@ (to indicate that it\'s
-- the agent speaking).
newCallAnalyticsJob ::
  CallAnalyticsJob
newCallAnalyticsJob =
  CallAnalyticsJob'
    { transcript = Prelude.Nothing,
      mediaFormat = Prelude.Nothing,
      callAnalyticsJobStatus = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      settings = Prelude.Nothing,
      mediaSampleRateHertz = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      callAnalyticsJobName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      identifiedLanguageScore = Prelude.Nothing,
      startTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      media = Prelude.Nothing,
      channelDefinitions = Prelude.Nothing
    }

-- | Undocumented member.
callAnalyticsJob_transcript :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Transcript)
callAnalyticsJob_transcript = Lens.lens (\CallAnalyticsJob' {transcript} -> transcript) (\s@CallAnalyticsJob' {} a -> s {transcript = a} :: CallAnalyticsJob)

-- | The format of the input media file.
callAnalyticsJob_mediaFormat :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe MediaFormat)
callAnalyticsJob_mediaFormat = Lens.lens (\CallAnalyticsJob' {mediaFormat} -> mediaFormat) (\s@CallAnalyticsJob' {} a -> s {mediaFormat = a} :: CallAnalyticsJob)

-- | Provides the status of the specified Call Analytics job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@ (or
-- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
-- the status is @FAILED@, @FailureReason@ provides details on why your
-- transcription job failed.
callAnalyticsJob_callAnalyticsJobStatus :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe CallAnalyticsJobStatus)
callAnalyticsJob_callAnalyticsJobStatus = Lens.lens (\CallAnalyticsJob' {callAnalyticsJobStatus} -> callAnalyticsJobStatus) (\s@CallAnalyticsJob' {} a -> s {callAnalyticsJobStatus = a} :: CallAnalyticsJob)

-- | The Amazon Resource Name (ARN) of an IAM role that has permissions to
-- access the Amazon S3 bucket that contains your input files. If the role
-- you specify doesn’t have the appropriate permissions to access the
-- specified Amazon S3 location, your request fails.
--
-- IAM role ARNs have the format
-- @arn:partition:iam::account:role\/role-name-with-path@. For example:
-- @arn:aws:iam::111122223333:role\/Admin@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
callAnalyticsJob_dataAccessRoleArn :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Text)
callAnalyticsJob_dataAccessRoleArn = Lens.lens (\CallAnalyticsJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@CallAnalyticsJob' {} a -> s {dataAccessRoleArn = a} :: CallAnalyticsJob)

-- | The date and time the specified Call Analytics job finished processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
callAnalyticsJob_completionTime :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJob_completionTime = Lens.lens (\CallAnalyticsJob' {completionTime} -> completionTime) (\s@CallAnalyticsJob' {} a -> s {completionTime = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Core._Time

-- | Allows additional optional settings in your request, including content
-- redaction; allows you to apply custom language models, vocabulary
-- filters, and custom vocabularies to your Call Analytics job.
callAnalyticsJob_settings :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe CallAnalyticsJobSettings)
callAnalyticsJob_settings = Lens.lens (\CallAnalyticsJob' {settings} -> settings) (\s@CallAnalyticsJob' {} a -> s {settings = a} :: CallAnalyticsJob)

-- | The sample rate, in Hertz, of the audio track in your input media file.
callAnalyticsJob_mediaSampleRateHertz :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Natural)
callAnalyticsJob_mediaSampleRateHertz = Lens.lens (\CallAnalyticsJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@CallAnalyticsJob' {} a -> s {mediaSampleRateHertz = a} :: CallAnalyticsJob)

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

-- | The name of the Call Analytics job. Job names are case sensitive and
-- must be unique within an Amazon Web Services account.
callAnalyticsJob_callAnalyticsJobName :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Text)
callAnalyticsJob_callAnalyticsJobName = Lens.lens (\CallAnalyticsJob' {callAnalyticsJobName} -> callAnalyticsJobName) (\s@CallAnalyticsJob' {} a -> s {callAnalyticsJobName = a} :: CallAnalyticsJob)

-- | The date and time the specified Call Analytics job request was made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
callAnalyticsJob_creationTime :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJob_creationTime = Lens.lens (\CallAnalyticsJob' {creationTime} -> creationTime) (\s@CallAnalyticsJob' {} a -> s {creationTime = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Core._Time

-- | The confidence score associated with the language identified in your
-- media file.
--
-- Confidence scores are values between 0 and 1; a larger value indicates a
-- higher probability that the identified language correctly matches the
-- language spoken in your media.
callAnalyticsJob_identifiedLanguageScore :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.Double)
callAnalyticsJob_identifiedLanguageScore = Lens.lens (\CallAnalyticsJob' {identifiedLanguageScore} -> identifiedLanguageScore) (\s@CallAnalyticsJob' {} a -> s {identifiedLanguageScore = a} :: CallAnalyticsJob)

-- | The date and time the specified Call Analytics job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
callAnalyticsJob_startTime :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJob_startTime = Lens.lens (\CallAnalyticsJob' {startTime} -> startTime) (\s@CallAnalyticsJob' {} a -> s {startTime = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Core._Time

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
--     The sample rate must be between 8,000 and 48,000 Hertz.
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

-- | Undocumented member.
callAnalyticsJob_media :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe Media)
callAnalyticsJob_media = Lens.lens (\CallAnalyticsJob' {media} -> media) (\s@CallAnalyticsJob' {} a -> s {media = a} :: CallAnalyticsJob)

-- | Allows you to specify which speaker is on which channel in your Call
-- Analytics job request. For example, if your agent is the first
-- participant to speak, you would set @ChannelId@ to @0@ (to indicate the
-- first channel) and @ParticipantRole@ to @AGENT@ (to indicate that it\'s
-- the agent speaking).
callAnalyticsJob_channelDefinitions :: Lens.Lens' CallAnalyticsJob (Prelude.Maybe (Prelude.NonEmpty ChannelDefinition))
callAnalyticsJob_channelDefinitions = Lens.lens (\CallAnalyticsJob' {channelDefinitions} -> channelDefinitions) (\s@CallAnalyticsJob' {} a -> s {channelDefinitions = a} :: CallAnalyticsJob) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CallAnalyticsJob where
  parseJSON =
    Core.withObject
      "CallAnalyticsJob"
      ( \x ->
          CallAnalyticsJob'
            Prelude.<$> (x Core..:? "Transcript")
            Prelude.<*> (x Core..:? "MediaFormat")
            Prelude.<*> (x Core..:? "CallAnalyticsJobStatus")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "CompletionTime")
            Prelude.<*> (x Core..:? "Settings")
            Prelude.<*> (x Core..:? "MediaSampleRateHertz")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "CallAnalyticsJobName")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "IdentifiedLanguageScore")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "Media")
            Prelude.<*> (x Core..:? "ChannelDefinitions")
      )

instance Prelude.Hashable CallAnalyticsJob where
  hashWithSalt _salt CallAnalyticsJob' {..} =
    _salt `Prelude.hashWithSalt` transcript
      `Prelude.hashWithSalt` mediaFormat
      `Prelude.hashWithSalt` callAnalyticsJobStatus
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` mediaSampleRateHertz
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` callAnalyticsJobName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` identifiedLanguageScore
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` media
      `Prelude.hashWithSalt` channelDefinitions

instance Prelude.NFData CallAnalyticsJob where
  rnf CallAnalyticsJob' {..} =
    Prelude.rnf transcript
      `Prelude.seq` Prelude.rnf mediaFormat
      `Prelude.seq` Prelude.rnf callAnalyticsJobStatus
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf mediaSampleRateHertz
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf callAnalyticsJobName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf identifiedLanguageScore
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf media
      `Prelude.seq` Prelude.rnf channelDefinitions
