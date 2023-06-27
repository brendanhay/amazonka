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
-- Module      : Amazonka.Transcribe.Types.TranscriptionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.TranscriptionJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.ContentRedaction
import Amazonka.Transcribe.Types.JobExecutionSettings
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.LanguageCodeItem
import Amazonka.Transcribe.Types.LanguageIdSettings
import Amazonka.Transcribe.Types.Media
import Amazonka.Transcribe.Types.MediaFormat
import Amazonka.Transcribe.Types.ModelSettings
import Amazonka.Transcribe.Types.Settings
import Amazonka.Transcribe.Types.SubtitlesOutput
import Amazonka.Transcribe.Types.Tag
import Amazonka.Transcribe.Types.Transcript
import Amazonka.Transcribe.Types.TranscriptionJobStatus

-- | Provides detailed information about a transcription job.
--
-- To view the status of the specified transcription job, check the
-- @TranscriptionJobStatus@ field. If the status is @COMPLETED@, the job is
-- finished and you can find the results at the location specified in
-- @TranscriptFileUri@. If the status is @FAILED@, @FailureReason@ provides
-- details on why your transcription job failed.
--
-- If you enabled content redaction, the redacted transcript can be found
-- at the location specified in @RedactedTranscriptFileUri@.
--
-- /See:/ 'newTranscriptionJob' smart constructor.
data TranscriptionJob = TranscriptionJob'
  { -- | The date and time the specified transcription job finished processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
    -- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether redaction was enabled in your transcript.
    contentRedaction :: Prelude.Maybe ContentRedaction,
    -- | The date and time the specified transcription job request was made.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | If @TranscriptionJobStatus@ is @FAILED@, @FailureReason@ contains
    -- information about why the transcription job request failed.
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
    -- | Indicates whether automatic language identification was enabled (@TRUE@)
    -- for the specified transcription job.
    identifyLanguage :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether automatic multi-language identification was enabled
    -- (@TRUE@) for the specified transcription job.
    identifyMultipleLanguages :: Prelude.Maybe Prelude.Bool,
    -- | Provides information about how your transcription job was processed.
    -- This parameter shows if your request was queued and what data access
    -- role was used.
    jobExecutionSettings :: Prelude.Maybe JobExecutionSettings,
    -- | The language code used to create your transcription job. This parameter
    -- is used with single-language identification. For multi-language
    -- identification requests, refer to the plural version of this parameter,
    -- @LanguageCodes@.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The language codes used to create your transcription job. This parameter
    -- is used with multi-language identification. For single-language
    -- identification requests, refer to the singular version of this
    -- parameter, @LanguageCode@.
    languageCodes :: Prelude.Maybe [LanguageCodeItem],
    -- | Provides the name and language of all custom language models, custom
    -- vocabularies, and custom vocabulary filters that you included in your
    -- request.
    languageIdSettings :: Prelude.Maybe (Prelude.HashMap LanguageCode LanguageIdSettings),
    -- | Provides the language codes you specified in your request.
    languageOptions :: Prelude.Maybe (Prelude.NonEmpty LanguageCode),
    -- | Provides the Amazon S3 location of the media file you used in your
    -- request.
    media :: Prelude.Maybe Media,
    -- | The format of the input media file.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | The sample rate, in hertz, of the audio track in your input media file.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural,
    -- | Provides information on the custom language model you included in your
    -- request.
    modelSettings :: Prelude.Maybe ModelSettings,
    -- | Provides information on any additional settings that were included in
    -- your request. Additional settings include channel identification,
    -- alternative transcriptions, speaker partitioning, custom vocabularies,
    -- and custom vocabulary filters.
    settings :: Prelude.Maybe Settings,
    -- | The date and time the specified transcription job began processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether subtitles were generated with your transcription.
    subtitles :: Prelude.Maybe SubtitlesOutput,
    -- | The tags, each in the form of a key:value pair, assigned to the
    -- specified transcription job.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Provides you with the Amazon S3 URI you can use to access your
    -- transcript.
    transcript :: Prelude.Maybe Transcript,
    -- | The name of the transcription job. Job names are case sensitive and must
    -- be unique within an Amazon Web Services account.
    transcriptionJobName :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of the specified transcription job.
    --
    -- If the status is @COMPLETED@, the job is finished and you can find the
    -- results at the location specified in @TranscriptFileUri@ (or
    -- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
    -- the status is @FAILED@, @FailureReason@ provides details on why your
    -- transcription job failed.
    transcriptionJobStatus :: Prelude.Maybe TranscriptionJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'transcriptionJob_completionTime' - The date and time the specified transcription job finished processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
--
-- 'contentRedaction', 'transcriptionJob_contentRedaction' - Indicates whether redaction was enabled in your transcript.
--
-- 'creationTime', 'transcriptionJob_creationTime' - The date and time the specified transcription job request was made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'failureReason', 'transcriptionJob_failureReason' - If @TranscriptionJobStatus@ is @FAILED@, @FailureReason@ contains
-- information about why the transcription job request failed.
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
-- 'identifiedLanguageScore', 'transcriptionJob_identifiedLanguageScore' - The confidence score associated with the language identified in your
-- media file.
--
-- Confidence scores are values between 0 and 1; a larger value indicates a
-- higher probability that the identified language correctly matches the
-- language spoken in your media.
--
-- 'identifyLanguage', 'transcriptionJob_identifyLanguage' - Indicates whether automatic language identification was enabled (@TRUE@)
-- for the specified transcription job.
--
-- 'identifyMultipleLanguages', 'transcriptionJob_identifyMultipleLanguages' - Indicates whether automatic multi-language identification was enabled
-- (@TRUE@) for the specified transcription job.
--
-- 'jobExecutionSettings', 'transcriptionJob_jobExecutionSettings' - Provides information about how your transcription job was processed.
-- This parameter shows if your request was queued and what data access
-- role was used.
--
-- 'languageCode', 'transcriptionJob_languageCode' - The language code used to create your transcription job. This parameter
-- is used with single-language identification. For multi-language
-- identification requests, refer to the plural version of this parameter,
-- @LanguageCodes@.
--
-- 'languageCodes', 'transcriptionJob_languageCodes' - The language codes used to create your transcription job. This parameter
-- is used with multi-language identification. For single-language
-- identification requests, refer to the singular version of this
-- parameter, @LanguageCode@.
--
-- 'languageIdSettings', 'transcriptionJob_languageIdSettings' - Provides the name and language of all custom language models, custom
-- vocabularies, and custom vocabulary filters that you included in your
-- request.
--
-- 'languageOptions', 'transcriptionJob_languageOptions' - Provides the language codes you specified in your request.
--
-- 'media', 'transcriptionJob_media' - Provides the Amazon S3 location of the media file you used in your
-- request.
--
-- 'mediaFormat', 'transcriptionJob_mediaFormat' - The format of the input media file.
--
-- 'mediaSampleRateHertz', 'transcriptionJob_mediaSampleRateHertz' - The sample rate, in hertz, of the audio track in your input media file.
--
-- 'modelSettings', 'transcriptionJob_modelSettings' - Provides information on the custom language model you included in your
-- request.
--
-- 'settings', 'transcriptionJob_settings' - Provides information on any additional settings that were included in
-- your request. Additional settings include channel identification,
-- alternative transcriptions, speaker partitioning, custom vocabularies,
-- and custom vocabulary filters.
--
-- 'startTime', 'transcriptionJob_startTime' - The date and time the specified transcription job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'subtitles', 'transcriptionJob_subtitles' - Indicates whether subtitles were generated with your transcription.
--
-- 'tags', 'transcriptionJob_tags' - The tags, each in the form of a key:value pair, assigned to the
-- specified transcription job.
--
-- 'transcript', 'transcriptionJob_transcript' - Provides you with the Amazon S3 URI you can use to access your
-- transcript.
--
-- 'transcriptionJobName', 'transcriptionJob_transcriptionJobName' - The name of the transcription job. Job names are case sensitive and must
-- be unique within an Amazon Web Services account.
--
-- 'transcriptionJobStatus', 'transcriptionJob_transcriptionJobStatus' - Provides the status of the specified transcription job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@ (or
-- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
-- the status is @FAILED@, @FailureReason@ provides details on why your
-- transcription job failed.
newTranscriptionJob ::
  TranscriptionJob
newTranscriptionJob =
  TranscriptionJob'
    { completionTime = Prelude.Nothing,
      contentRedaction = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      identifiedLanguageScore = Prelude.Nothing,
      identifyLanguage = Prelude.Nothing,
      identifyMultipleLanguages = Prelude.Nothing,
      jobExecutionSettings = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      languageCodes = Prelude.Nothing,
      languageIdSettings = Prelude.Nothing,
      languageOptions = Prelude.Nothing,
      media = Prelude.Nothing,
      mediaFormat = Prelude.Nothing,
      mediaSampleRateHertz = Prelude.Nothing,
      modelSettings = Prelude.Nothing,
      settings = Prelude.Nothing,
      startTime = Prelude.Nothing,
      subtitles = Prelude.Nothing,
      tags = Prelude.Nothing,
      transcript = Prelude.Nothing,
      transcriptionJobName = Prelude.Nothing,
      transcriptionJobStatus = Prelude.Nothing
    }

-- | The date and time the specified transcription job finished processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
transcriptionJob_completionTime :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.UTCTime)
transcriptionJob_completionTime = Lens.lens (\TranscriptionJob' {completionTime} -> completionTime) (\s@TranscriptionJob' {} a -> s {completionTime = a} :: TranscriptionJob) Prelude.. Lens.mapping Data._Time

-- | Indicates whether redaction was enabled in your transcript.
transcriptionJob_contentRedaction :: Lens.Lens' TranscriptionJob (Prelude.Maybe ContentRedaction)
transcriptionJob_contentRedaction = Lens.lens (\TranscriptionJob' {contentRedaction} -> contentRedaction) (\s@TranscriptionJob' {} a -> s {contentRedaction = a} :: TranscriptionJob)

-- | The date and time the specified transcription job request was made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
transcriptionJob_creationTime :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.UTCTime)
transcriptionJob_creationTime = Lens.lens (\TranscriptionJob' {creationTime} -> creationTime) (\s@TranscriptionJob' {} a -> s {creationTime = a} :: TranscriptionJob) Prelude.. Lens.mapping Data._Time

-- | If @TranscriptionJobStatus@ is @FAILED@, @FailureReason@ contains
-- information about why the transcription job request failed.
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
transcriptionJob_failureReason :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Text)
transcriptionJob_failureReason = Lens.lens (\TranscriptionJob' {failureReason} -> failureReason) (\s@TranscriptionJob' {} a -> s {failureReason = a} :: TranscriptionJob)

-- | The confidence score associated with the language identified in your
-- media file.
--
-- Confidence scores are values between 0 and 1; a larger value indicates a
-- higher probability that the identified language correctly matches the
-- language spoken in your media.
transcriptionJob_identifiedLanguageScore :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Double)
transcriptionJob_identifiedLanguageScore = Lens.lens (\TranscriptionJob' {identifiedLanguageScore} -> identifiedLanguageScore) (\s@TranscriptionJob' {} a -> s {identifiedLanguageScore = a} :: TranscriptionJob)

-- | Indicates whether automatic language identification was enabled (@TRUE@)
-- for the specified transcription job.
transcriptionJob_identifyLanguage :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Bool)
transcriptionJob_identifyLanguage = Lens.lens (\TranscriptionJob' {identifyLanguage} -> identifyLanguage) (\s@TranscriptionJob' {} a -> s {identifyLanguage = a} :: TranscriptionJob)

-- | Indicates whether automatic multi-language identification was enabled
-- (@TRUE@) for the specified transcription job.
transcriptionJob_identifyMultipleLanguages :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Bool)
transcriptionJob_identifyMultipleLanguages = Lens.lens (\TranscriptionJob' {identifyMultipleLanguages} -> identifyMultipleLanguages) (\s@TranscriptionJob' {} a -> s {identifyMultipleLanguages = a} :: TranscriptionJob)

-- | Provides information about how your transcription job was processed.
-- This parameter shows if your request was queued and what data access
-- role was used.
transcriptionJob_jobExecutionSettings :: Lens.Lens' TranscriptionJob (Prelude.Maybe JobExecutionSettings)
transcriptionJob_jobExecutionSettings = Lens.lens (\TranscriptionJob' {jobExecutionSettings} -> jobExecutionSettings) (\s@TranscriptionJob' {} a -> s {jobExecutionSettings = a} :: TranscriptionJob)

-- | The language code used to create your transcription job. This parameter
-- is used with single-language identification. For multi-language
-- identification requests, refer to the plural version of this parameter,
-- @LanguageCodes@.
transcriptionJob_languageCode :: Lens.Lens' TranscriptionJob (Prelude.Maybe LanguageCode)
transcriptionJob_languageCode = Lens.lens (\TranscriptionJob' {languageCode} -> languageCode) (\s@TranscriptionJob' {} a -> s {languageCode = a} :: TranscriptionJob)

-- | The language codes used to create your transcription job. This parameter
-- is used with multi-language identification. For single-language
-- identification requests, refer to the singular version of this
-- parameter, @LanguageCode@.
transcriptionJob_languageCodes :: Lens.Lens' TranscriptionJob (Prelude.Maybe [LanguageCodeItem])
transcriptionJob_languageCodes = Lens.lens (\TranscriptionJob' {languageCodes} -> languageCodes) (\s@TranscriptionJob' {} a -> s {languageCodes = a} :: TranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | Provides the name and language of all custom language models, custom
-- vocabularies, and custom vocabulary filters that you included in your
-- request.
transcriptionJob_languageIdSettings :: Lens.Lens' TranscriptionJob (Prelude.Maybe (Prelude.HashMap LanguageCode LanguageIdSettings))
transcriptionJob_languageIdSettings = Lens.lens (\TranscriptionJob' {languageIdSettings} -> languageIdSettings) (\s@TranscriptionJob' {} a -> s {languageIdSettings = a} :: TranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | Provides the language codes you specified in your request.
transcriptionJob_languageOptions :: Lens.Lens' TranscriptionJob (Prelude.Maybe (Prelude.NonEmpty LanguageCode))
transcriptionJob_languageOptions = Lens.lens (\TranscriptionJob' {languageOptions} -> languageOptions) (\s@TranscriptionJob' {} a -> s {languageOptions = a} :: TranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | Provides the Amazon S3 location of the media file you used in your
-- request.
transcriptionJob_media :: Lens.Lens' TranscriptionJob (Prelude.Maybe Media)
transcriptionJob_media = Lens.lens (\TranscriptionJob' {media} -> media) (\s@TranscriptionJob' {} a -> s {media = a} :: TranscriptionJob)

-- | The format of the input media file.
transcriptionJob_mediaFormat :: Lens.Lens' TranscriptionJob (Prelude.Maybe MediaFormat)
transcriptionJob_mediaFormat = Lens.lens (\TranscriptionJob' {mediaFormat} -> mediaFormat) (\s@TranscriptionJob' {} a -> s {mediaFormat = a} :: TranscriptionJob)

-- | The sample rate, in hertz, of the audio track in your input media file.
transcriptionJob_mediaSampleRateHertz :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Natural)
transcriptionJob_mediaSampleRateHertz = Lens.lens (\TranscriptionJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@TranscriptionJob' {} a -> s {mediaSampleRateHertz = a} :: TranscriptionJob)

-- | Provides information on the custom language model you included in your
-- request.
transcriptionJob_modelSettings :: Lens.Lens' TranscriptionJob (Prelude.Maybe ModelSettings)
transcriptionJob_modelSettings = Lens.lens (\TranscriptionJob' {modelSettings} -> modelSettings) (\s@TranscriptionJob' {} a -> s {modelSettings = a} :: TranscriptionJob)

-- | Provides information on any additional settings that were included in
-- your request. Additional settings include channel identification,
-- alternative transcriptions, speaker partitioning, custom vocabularies,
-- and custom vocabulary filters.
transcriptionJob_settings :: Lens.Lens' TranscriptionJob (Prelude.Maybe Settings)
transcriptionJob_settings = Lens.lens (\TranscriptionJob' {settings} -> settings) (\s@TranscriptionJob' {} a -> s {settings = a} :: TranscriptionJob)

-- | The date and time the specified transcription job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
transcriptionJob_startTime :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.UTCTime)
transcriptionJob_startTime = Lens.lens (\TranscriptionJob' {startTime} -> startTime) (\s@TranscriptionJob' {} a -> s {startTime = a} :: TranscriptionJob) Prelude.. Lens.mapping Data._Time

-- | Indicates whether subtitles were generated with your transcription.
transcriptionJob_subtitles :: Lens.Lens' TranscriptionJob (Prelude.Maybe SubtitlesOutput)
transcriptionJob_subtitles = Lens.lens (\TranscriptionJob' {subtitles} -> subtitles) (\s@TranscriptionJob' {} a -> s {subtitles = a} :: TranscriptionJob)

-- | The tags, each in the form of a key:value pair, assigned to the
-- specified transcription job.
transcriptionJob_tags :: Lens.Lens' TranscriptionJob (Prelude.Maybe (Prelude.NonEmpty Tag))
transcriptionJob_tags = Lens.lens (\TranscriptionJob' {tags} -> tags) (\s@TranscriptionJob' {} a -> s {tags = a} :: TranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | Provides you with the Amazon S3 URI you can use to access your
-- transcript.
transcriptionJob_transcript :: Lens.Lens' TranscriptionJob (Prelude.Maybe Transcript)
transcriptionJob_transcript = Lens.lens (\TranscriptionJob' {transcript} -> transcript) (\s@TranscriptionJob' {} a -> s {transcript = a} :: TranscriptionJob)

-- | The name of the transcription job. Job names are case sensitive and must
-- be unique within an Amazon Web Services account.
transcriptionJob_transcriptionJobName :: Lens.Lens' TranscriptionJob (Prelude.Maybe Prelude.Text)
transcriptionJob_transcriptionJobName = Lens.lens (\TranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@TranscriptionJob' {} a -> s {transcriptionJobName = a} :: TranscriptionJob)

-- | Provides the status of the specified transcription job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@ (or
-- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
-- the status is @FAILED@, @FailureReason@ provides details on why your
-- transcription job failed.
transcriptionJob_transcriptionJobStatus :: Lens.Lens' TranscriptionJob (Prelude.Maybe TranscriptionJobStatus)
transcriptionJob_transcriptionJobStatus = Lens.lens (\TranscriptionJob' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@TranscriptionJob' {} a -> s {transcriptionJobStatus = a} :: TranscriptionJob)

instance Data.FromJSON TranscriptionJob where
  parseJSON =
    Data.withObject
      "TranscriptionJob"
      ( \x ->
          TranscriptionJob'
            Prelude.<$> (x Data..:? "CompletionTime")
            Prelude.<*> (x Data..:? "ContentRedaction")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "IdentifiedLanguageScore")
            Prelude.<*> (x Data..:? "IdentifyLanguage")
            Prelude.<*> (x Data..:? "IdentifyMultipleLanguages")
            Prelude.<*> (x Data..:? "JobExecutionSettings")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "LanguageCodes" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "LanguageIdSettings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LanguageOptions")
            Prelude.<*> (x Data..:? "Media")
            Prelude.<*> (x Data..:? "MediaFormat")
            Prelude.<*> (x Data..:? "MediaSampleRateHertz")
            Prelude.<*> (x Data..:? "ModelSettings")
            Prelude.<*> (x Data..:? "Settings")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Subtitles")
            Prelude.<*> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "Transcript")
            Prelude.<*> (x Data..:? "TranscriptionJobName")
            Prelude.<*> (x Data..:? "TranscriptionJobStatus")
      )

instance Prelude.Hashable TranscriptionJob where
  hashWithSalt _salt TranscriptionJob' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` contentRedaction
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` identifiedLanguageScore
      `Prelude.hashWithSalt` identifyLanguage
      `Prelude.hashWithSalt` identifyMultipleLanguages
      `Prelude.hashWithSalt` jobExecutionSettings
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` languageCodes
      `Prelude.hashWithSalt` languageIdSettings
      `Prelude.hashWithSalt` languageOptions
      `Prelude.hashWithSalt` media
      `Prelude.hashWithSalt` mediaFormat
      `Prelude.hashWithSalt` mediaSampleRateHertz
      `Prelude.hashWithSalt` modelSettings
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` subtitles
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` transcript
      `Prelude.hashWithSalt` transcriptionJobName
      `Prelude.hashWithSalt` transcriptionJobStatus

instance Prelude.NFData TranscriptionJob where
  rnf TranscriptionJob' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf contentRedaction
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf identifiedLanguageScore
      `Prelude.seq` Prelude.rnf identifyLanguage
      `Prelude.seq` Prelude.rnf identifyMultipleLanguages
      `Prelude.seq` Prelude.rnf jobExecutionSettings
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf languageCodes
      `Prelude.seq` Prelude.rnf languageIdSettings
      `Prelude.seq` Prelude.rnf languageOptions
      `Prelude.seq` Prelude.rnf media
      `Prelude.seq` Prelude.rnf mediaFormat
      `Prelude.seq` Prelude.rnf mediaSampleRateHertz
      `Prelude.seq` Prelude.rnf modelSettings
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf subtitles
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf transcript
      `Prelude.seq` Prelude.rnf
        transcriptionJobName
      `Prelude.seq` Prelude.rnf
        transcriptionJobStatus
