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
-- Module      : Amazonka.Transcribe.Types.MedicalTranscriptionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.MedicalTranscriptionJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.Media
import Amazonka.Transcribe.Types.MediaFormat
import Amazonka.Transcribe.Types.MedicalContentIdentificationType
import Amazonka.Transcribe.Types.MedicalTranscript
import Amazonka.Transcribe.Types.MedicalTranscriptionSetting
import Amazonka.Transcribe.Types.Specialty
import Amazonka.Transcribe.Types.Tag
import Amazonka.Transcribe.Types.TranscriptionJobStatus
import Amazonka.Transcribe.Types.Type

-- | Provides detailed information about a medical transcription job.
--
-- To view the status of the specified medical transcription job, check the
-- @TranscriptionJobStatus@ field. If the status is @COMPLETED@, the job is
-- finished and you can find the results at the location specified in
-- @TranscriptFileUri@. If the status is @FAILED@, @FailureReason@ provides
-- details on why your transcription job failed.
--
-- /See:/ 'newMedicalTranscriptionJob' smart constructor.
data MedicalTranscriptionJob = MedicalTranscriptionJob'
  { -- | The tags, each in the form of a key:value pair, assigned to the
    -- specified medical transcription job.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Indicates whether the input media is a dictation or a conversation, as
    -- specified in the @StartMedicalTranscriptionJob@ request.
    type' :: Prelude.Maybe Type,
    -- | Labels all personal health information (PHI) identified in your
    -- transcript. For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/phi-id.html Identifying personal health information (PHI) in a transcription>.
    contentIdentificationType :: Prelude.Maybe MedicalContentIdentificationType,
    -- | Provides you with the Amazon S3 URI you can use to access your
    -- transcript.
    transcript :: Prelude.Maybe MedicalTranscript,
    -- | The format of the input media file.
    mediaFormat :: Prelude.Maybe MediaFormat,
    -- | The name of the medical transcription job. Job names are case sensitive
    -- and must be unique within an Amazon Web Services account.
    medicalTranscriptionJobName :: Prelude.Maybe Prelude.Text,
    -- | The date and time the specified medical transcription job finished
    -- processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
    -- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | Specify additional optional settings in your request, including channel
    -- identification, alternative transcriptions, and speaker labeling; allows
    -- you to apply custom vocabularies to your medical transcription job.
    settings :: Prelude.Maybe MedicalTranscriptionSetting,
    -- | The sample rate, in Hertz, of the audio track in your input media file.
    mediaSampleRateHertz :: Prelude.Maybe Prelude.Natural,
    -- | The language code used to create your medical transcription job. US
    -- English (@en-US@) is the only supported language for medical
    -- transcriptions.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Provides the status of the specified medical transcription job.
    --
    -- If the status is @COMPLETED@, the job is finished and you can find the
    -- results at the location specified in @TranscriptFileUri@. If the status
    -- is @FAILED@, @FailureReason@ provides details on why your transcription
    -- job failed.
    transcriptionJobStatus :: Prelude.Maybe TranscriptionJobStatus,
    -- | The date and time the specified medical transcription job request was
    -- made.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Describes the medical specialty represented in your media.
    specialty :: Prelude.Maybe Specialty,
    -- | The date and time the specified medical transcription job began
    -- processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    startTime :: Prelude.Maybe Data.POSIX,
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
    --     The sample rate must be between 16,000 and 48,000 Hertz.
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
    media :: Prelude.Maybe Media
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MedicalTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'medicalTranscriptionJob_tags' - The tags, each in the form of a key:value pair, assigned to the
-- specified medical transcription job.
--
-- 'type'', 'medicalTranscriptionJob_type' - Indicates whether the input media is a dictation or a conversation, as
-- specified in the @StartMedicalTranscriptionJob@ request.
--
-- 'contentIdentificationType', 'medicalTranscriptionJob_contentIdentificationType' - Labels all personal health information (PHI) identified in your
-- transcript. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/phi-id.html Identifying personal health information (PHI) in a transcription>.
--
-- 'transcript', 'medicalTranscriptionJob_transcript' - Provides you with the Amazon S3 URI you can use to access your
-- transcript.
--
-- 'mediaFormat', 'medicalTranscriptionJob_mediaFormat' - The format of the input media file.
--
-- 'medicalTranscriptionJobName', 'medicalTranscriptionJob_medicalTranscriptionJobName' - The name of the medical transcription job. Job names are case sensitive
-- and must be unique within an Amazon Web Services account.
--
-- 'completionTime', 'medicalTranscriptionJob_completionTime' - The date and time the specified medical transcription job finished
-- processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
--
-- 'settings', 'medicalTranscriptionJob_settings' - Specify additional optional settings in your request, including channel
-- identification, alternative transcriptions, and speaker labeling; allows
-- you to apply custom vocabularies to your medical transcription job.
--
-- 'mediaSampleRateHertz', 'medicalTranscriptionJob_mediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in your input media file.
--
-- 'languageCode', 'medicalTranscriptionJob_languageCode' - The language code used to create your medical transcription job. US
-- English (@en-US@) is the only supported language for medical
-- transcriptions.
--
-- 'transcriptionJobStatus', 'medicalTranscriptionJob_transcriptionJobStatus' - Provides the status of the specified medical transcription job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@. If the status
-- is @FAILED@, @FailureReason@ provides details on why your transcription
-- job failed.
--
-- 'creationTime', 'medicalTranscriptionJob_creationTime' - The date and time the specified medical transcription job request was
-- made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'specialty', 'medicalTranscriptionJob_specialty' - Describes the medical specialty represented in your media.
--
-- 'startTime', 'medicalTranscriptionJob_startTime' - The date and time the specified medical transcription job began
-- processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'failureReason', 'medicalTranscriptionJob_failureReason' - If @TranscriptionJobStatus@ is @FAILED@, @FailureReason@ contains
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
--     The sample rate must be between 16,000 and 48,000 Hertz.
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
-- 'media', 'medicalTranscriptionJob_media' - Undocumented member.
newMedicalTranscriptionJob ::
  MedicalTranscriptionJob
newMedicalTranscriptionJob =
  MedicalTranscriptionJob'
    { tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      contentIdentificationType = Prelude.Nothing,
      transcript = Prelude.Nothing,
      mediaFormat = Prelude.Nothing,
      medicalTranscriptionJobName = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      settings = Prelude.Nothing,
      mediaSampleRateHertz = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      transcriptionJobStatus = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      specialty = Prelude.Nothing,
      startTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      media = Prelude.Nothing
    }

-- | The tags, each in the form of a key:value pair, assigned to the
-- specified medical transcription job.
medicalTranscriptionJob_tags :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe (Prelude.NonEmpty Tag))
medicalTranscriptionJob_tags = Lens.lens (\MedicalTranscriptionJob' {tags} -> tags) (\s@MedicalTranscriptionJob' {} a -> s {tags = a} :: MedicalTranscriptionJob) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the input media is a dictation or a conversation, as
-- specified in the @StartMedicalTranscriptionJob@ request.
medicalTranscriptionJob_type :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe Type)
medicalTranscriptionJob_type = Lens.lens (\MedicalTranscriptionJob' {type'} -> type') (\s@MedicalTranscriptionJob' {} a -> s {type' = a} :: MedicalTranscriptionJob)

-- | Labels all personal health information (PHI) identified in your
-- transcript. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/phi-id.html Identifying personal health information (PHI) in a transcription>.
medicalTranscriptionJob_contentIdentificationType :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe MedicalContentIdentificationType)
medicalTranscriptionJob_contentIdentificationType = Lens.lens (\MedicalTranscriptionJob' {contentIdentificationType} -> contentIdentificationType) (\s@MedicalTranscriptionJob' {} a -> s {contentIdentificationType = a} :: MedicalTranscriptionJob)

-- | Provides you with the Amazon S3 URI you can use to access your
-- transcript.
medicalTranscriptionJob_transcript :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe MedicalTranscript)
medicalTranscriptionJob_transcript = Lens.lens (\MedicalTranscriptionJob' {transcript} -> transcript) (\s@MedicalTranscriptionJob' {} a -> s {transcript = a} :: MedicalTranscriptionJob)

-- | The format of the input media file.
medicalTranscriptionJob_mediaFormat :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe MediaFormat)
medicalTranscriptionJob_mediaFormat = Lens.lens (\MedicalTranscriptionJob' {mediaFormat} -> mediaFormat) (\s@MedicalTranscriptionJob' {} a -> s {mediaFormat = a} :: MedicalTranscriptionJob)

-- | The name of the medical transcription job. Job names are case sensitive
-- and must be unique within an Amazon Web Services account.
medicalTranscriptionJob_medicalTranscriptionJobName :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe Prelude.Text)
medicalTranscriptionJob_medicalTranscriptionJobName = Lens.lens (\MedicalTranscriptionJob' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@MedicalTranscriptionJob' {} a -> s {medicalTranscriptionJobName = a} :: MedicalTranscriptionJob)

-- | The date and time the specified medical transcription job finished
-- processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
medicalTranscriptionJob_completionTime :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJob_completionTime = Lens.lens (\MedicalTranscriptionJob' {completionTime} -> completionTime) (\s@MedicalTranscriptionJob' {} a -> s {completionTime = a} :: MedicalTranscriptionJob) Prelude.. Lens.mapping Data._Time

-- | Specify additional optional settings in your request, including channel
-- identification, alternative transcriptions, and speaker labeling; allows
-- you to apply custom vocabularies to your medical transcription job.
medicalTranscriptionJob_settings :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe MedicalTranscriptionSetting)
medicalTranscriptionJob_settings = Lens.lens (\MedicalTranscriptionJob' {settings} -> settings) (\s@MedicalTranscriptionJob' {} a -> s {settings = a} :: MedicalTranscriptionJob)

-- | The sample rate, in Hertz, of the audio track in your input media file.
medicalTranscriptionJob_mediaSampleRateHertz :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe Prelude.Natural)
medicalTranscriptionJob_mediaSampleRateHertz = Lens.lens (\MedicalTranscriptionJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@MedicalTranscriptionJob' {} a -> s {mediaSampleRateHertz = a} :: MedicalTranscriptionJob)

-- | The language code used to create your medical transcription job. US
-- English (@en-US@) is the only supported language for medical
-- transcriptions.
medicalTranscriptionJob_languageCode :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe LanguageCode)
medicalTranscriptionJob_languageCode = Lens.lens (\MedicalTranscriptionJob' {languageCode} -> languageCode) (\s@MedicalTranscriptionJob' {} a -> s {languageCode = a} :: MedicalTranscriptionJob)

-- | Provides the status of the specified medical transcription job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@. If the status
-- is @FAILED@, @FailureReason@ provides details on why your transcription
-- job failed.
medicalTranscriptionJob_transcriptionJobStatus :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe TranscriptionJobStatus)
medicalTranscriptionJob_transcriptionJobStatus = Lens.lens (\MedicalTranscriptionJob' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@MedicalTranscriptionJob' {} a -> s {transcriptionJobStatus = a} :: MedicalTranscriptionJob)

-- | The date and time the specified medical transcription job request was
-- made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
medicalTranscriptionJob_creationTime :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJob_creationTime = Lens.lens (\MedicalTranscriptionJob' {creationTime} -> creationTime) (\s@MedicalTranscriptionJob' {} a -> s {creationTime = a} :: MedicalTranscriptionJob) Prelude.. Lens.mapping Data._Time

-- | Describes the medical specialty represented in your media.
medicalTranscriptionJob_specialty :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe Specialty)
medicalTranscriptionJob_specialty = Lens.lens (\MedicalTranscriptionJob' {specialty} -> specialty) (\s@MedicalTranscriptionJob' {} a -> s {specialty = a} :: MedicalTranscriptionJob)

-- | The date and time the specified medical transcription job began
-- processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
medicalTranscriptionJob_startTime :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe Prelude.UTCTime)
medicalTranscriptionJob_startTime = Lens.lens (\MedicalTranscriptionJob' {startTime} -> startTime) (\s@MedicalTranscriptionJob' {} a -> s {startTime = a} :: MedicalTranscriptionJob) Prelude.. Lens.mapping Data._Time

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
--     The sample rate must be between 16,000 and 48,000 Hertz.
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
medicalTranscriptionJob_failureReason :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe Prelude.Text)
medicalTranscriptionJob_failureReason = Lens.lens (\MedicalTranscriptionJob' {failureReason} -> failureReason) (\s@MedicalTranscriptionJob' {} a -> s {failureReason = a} :: MedicalTranscriptionJob)

-- | Undocumented member.
medicalTranscriptionJob_media :: Lens.Lens' MedicalTranscriptionJob (Prelude.Maybe Media)
medicalTranscriptionJob_media = Lens.lens (\MedicalTranscriptionJob' {media} -> media) (\s@MedicalTranscriptionJob' {} a -> s {media = a} :: MedicalTranscriptionJob)

instance Data.FromJSON MedicalTranscriptionJob where
  parseJSON =
    Data.withObject
      "MedicalTranscriptionJob"
      ( \x ->
          MedicalTranscriptionJob'
            Prelude.<$> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "ContentIdentificationType")
            Prelude.<*> (x Data..:? "Transcript")
            Prelude.<*> (x Data..:? "MediaFormat")
            Prelude.<*> (x Data..:? "MedicalTranscriptionJobName")
            Prelude.<*> (x Data..:? "CompletionTime")
            Prelude.<*> (x Data..:? "Settings")
            Prelude.<*> (x Data..:? "MediaSampleRateHertz")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "TranscriptionJobStatus")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Specialty")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "Media")
      )

instance Prelude.Hashable MedicalTranscriptionJob where
  hashWithSalt _salt MedicalTranscriptionJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` contentIdentificationType
      `Prelude.hashWithSalt` transcript
      `Prelude.hashWithSalt` mediaFormat
      `Prelude.hashWithSalt` medicalTranscriptionJobName
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` mediaSampleRateHertz
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` transcriptionJobStatus
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` specialty
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` media

instance Prelude.NFData MedicalTranscriptionJob where
  rnf MedicalTranscriptionJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf contentIdentificationType
      `Prelude.seq` Prelude.rnf transcript
      `Prelude.seq` Prelude.rnf mediaFormat
      `Prelude.seq` Prelude.rnf medicalTranscriptionJobName
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf mediaSampleRateHertz
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf transcriptionJobStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf specialty
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf media
