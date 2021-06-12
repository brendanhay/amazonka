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
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscriptionJob where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.Media
import Network.AWS.Transcribe.Types.MediaFormat
import Network.AWS.Transcribe.Types.MedicalTranscript
import Network.AWS.Transcribe.Types.MedicalTranscriptionSetting
import Network.AWS.Transcribe.Types.Specialty
import Network.AWS.Transcribe.Types.TranscriptionJobStatus
import Network.AWS.Transcribe.Types.Type

-- | The data structure that contains the information for a medical
-- transcription job.
--
-- /See:/ 'newMedicalTranscriptionJob' smart constructor.
data MedicalTranscriptionJob = MedicalTranscriptionJob'
  { -- | The language code for the language spoken in the source audio file. US
    -- English (en-US) is the only supported language for medical
    -- transcriptions. Any other value you enter for language code results in a
    -- @BadRequestException@ error.
    languageCode :: Core.Maybe LanguageCode,
    -- | The format of the input media file.
    mediaFormat :: Core.Maybe MediaFormat,
    media :: Core.Maybe Media,
    -- | A timestamp that shows when the job was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Core.Maybe Core.POSIX,
    -- | An object that contains the @MedicalTranscript@. The @MedicalTranscript@
    -- contains the @TranscriptFileUri@.
    transcript :: Core.Maybe MedicalTranscript,
    -- | A timestamp that shows when the job started processing.
    startTime :: Core.Maybe Core.POSIX,
    -- | The completion status of a medical transcription job.
    transcriptionJobStatus :: Core.Maybe TranscriptionJobStatus,
    -- | The medical specialty of any clinicians providing a dictation or having
    -- a conversation. @PRIMARYCARE@ is the only available setting for this
    -- object. This specialty enables you to generate transcriptions for the
    -- following medical fields:
    --
    -- -   Family Medicine
    specialty :: Core.Maybe Specialty,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@, this field contains
    -- information about why the job failed.
    --
    -- The @FailureReason@ field contains one of the following values:
    --
    -- -   @Unsupported media format@- The media format specified in the
    --     @MediaFormat@ field of the request isn\'t valid. See the description
    --     of the @MediaFormat@ field for a list of valid values.
    --
    -- -   @The media format provided does not match the detected media format@-
    --     The media format of the audio file doesn\'t match the format
    --     specified in the @MediaFormat@ field in the request. Check the media
    --     format of your media file and make sure the two values match.
    --
    -- -   @Invalid sample rate for audio file@- The sample rate specified in
    --     the @MediaSampleRateHertz@ of the request isn\'t valid. The sample
    --     rate must be between 8000 and 48000 Hertz.
    --
    -- -   @The sample rate provided does not match the detected sample rate@-
    --     The sample rate in the audio file doesn\'t match the sample rate
    --     specified in the @MediaSampleRateHertz@ field in the request. Check
    --     the sample rate of your media file and make sure that the two values
    --     match.
    --
    -- -   @Invalid file size: file size too large@- The size of your audio
    --     file is larger than what Amazon Transcribe Medical can process. For
    --     more information, see
    --     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and Quotas>
    --     in the /Amazon Transcribe Medical Guide/
    --
    -- -   @Invalid number of channels: number of channels too large@- Your
    --     audio contains more channels than Amazon Transcribe Medical is
    --     configured to process. To request additional channels, see
    --     <https://docs.aws.amazon.com/general/latest/gr/transcribe-medical.html Amazon Transcribe Medical Endpoints and Quotas>
    --     in the /Amazon Web Services General Reference/
    failureReason :: Core.Maybe Core.Text,
    -- | The sample rate, in Hertz, of the source audio containing medical
    -- information.
    --
    -- If you don\'t specify the sample rate, Amazon Transcribe Medical
    -- determines it for you. If you choose to specify the sample rate, it must
    -- match the rate detected by Amazon Transcribe Medical. In most cases, you
    -- should leave the @MediaSampleHertz@ blank and let Amazon Transcribe
    -- Medical determine the sample rate.
    mediaSampleRateHertz :: Core.Maybe Core.Natural,
    -- | The type of speech in the transcription job. @CONVERSATION@ is generally
    -- used for patient-physician dialogues. @DICTATION@ is the setting for
    -- physicians speaking their notes after seeing a patient. For more
    -- information, see how-it-works-med
    type' :: Core.Maybe Type,
    -- | The name for a given medical transcription job.
    medicalTranscriptionJobName :: Core.Maybe Core.Text,
    -- | Object that contains object.
    settings :: Core.Maybe MedicalTranscriptionSetting
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MedicalTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'medicalTranscriptionJob_languageCode' - The language code for the language spoken in the source audio file. US
-- English (en-US) is the only supported language for medical
-- transcriptions. Any other value you enter for language code results in a
-- @BadRequestException@ error.
--
-- 'mediaFormat', 'medicalTranscriptionJob_mediaFormat' - The format of the input media file.
--
-- 'media', 'medicalTranscriptionJob_media' - Undocumented member.
--
-- 'creationTime', 'medicalTranscriptionJob_creationTime' - A timestamp that shows when the job was created.
--
-- 'completionTime', 'medicalTranscriptionJob_completionTime' - A timestamp that shows when the job was completed.
--
-- 'transcript', 'medicalTranscriptionJob_transcript' - An object that contains the @MedicalTranscript@. The @MedicalTranscript@
-- contains the @TranscriptFileUri@.
--
-- 'startTime', 'medicalTranscriptionJob_startTime' - A timestamp that shows when the job started processing.
--
-- 'transcriptionJobStatus', 'medicalTranscriptionJob_transcriptionJobStatus' - The completion status of a medical transcription job.
--
-- 'specialty', 'medicalTranscriptionJob_specialty' - The medical specialty of any clinicians providing a dictation or having
-- a conversation. @PRIMARYCARE@ is the only available setting for this
-- object. This specialty enables you to generate transcriptions for the
-- following medical fields:
--
-- -   Family Medicine
--
-- 'failureReason', 'medicalTranscriptionJob_failureReason' - If the @TranscriptionJobStatus@ field is @FAILED@, this field contains
-- information about why the job failed.
--
-- The @FailureReason@ field contains one of the following values:
--
-- -   @Unsupported media format@- The media format specified in the
--     @MediaFormat@ field of the request isn\'t valid. See the description
--     of the @MediaFormat@ field for a list of valid values.
--
-- -   @The media format provided does not match the detected media format@-
--     The media format of the audio file doesn\'t match the format
--     specified in the @MediaFormat@ field in the request. Check the media
--     format of your media file and make sure the two values match.
--
-- -   @Invalid sample rate for audio file@- The sample rate specified in
--     the @MediaSampleRateHertz@ of the request isn\'t valid. The sample
--     rate must be between 8000 and 48000 Hertz.
--
-- -   @The sample rate provided does not match the detected sample rate@-
--     The sample rate in the audio file doesn\'t match the sample rate
--     specified in the @MediaSampleRateHertz@ field in the request. Check
--     the sample rate of your media file and make sure that the two values
--     match.
--
-- -   @Invalid file size: file size too large@- The size of your audio
--     file is larger than what Amazon Transcribe Medical can process. For
--     more information, see
--     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and Quotas>
--     in the /Amazon Transcribe Medical Guide/
--
-- -   @Invalid number of channels: number of channels too large@- Your
--     audio contains more channels than Amazon Transcribe Medical is
--     configured to process. To request additional channels, see
--     <https://docs.aws.amazon.com/general/latest/gr/transcribe-medical.html Amazon Transcribe Medical Endpoints and Quotas>
--     in the /Amazon Web Services General Reference/
--
-- 'mediaSampleRateHertz', 'medicalTranscriptionJob_mediaSampleRateHertz' - The sample rate, in Hertz, of the source audio containing medical
-- information.
--
-- If you don\'t specify the sample rate, Amazon Transcribe Medical
-- determines it for you. If you choose to specify the sample rate, it must
-- match the rate detected by Amazon Transcribe Medical. In most cases, you
-- should leave the @MediaSampleHertz@ blank and let Amazon Transcribe
-- Medical determine the sample rate.
--
-- 'type'', 'medicalTranscriptionJob_type' - The type of speech in the transcription job. @CONVERSATION@ is generally
-- used for patient-physician dialogues. @DICTATION@ is the setting for
-- physicians speaking their notes after seeing a patient. For more
-- information, see how-it-works-med
--
-- 'medicalTranscriptionJobName', 'medicalTranscriptionJob_medicalTranscriptionJobName' - The name for a given medical transcription job.
--
-- 'settings', 'medicalTranscriptionJob_settings' - Object that contains object.
newMedicalTranscriptionJob ::
  MedicalTranscriptionJob
newMedicalTranscriptionJob =
  MedicalTranscriptionJob'
    { languageCode =
        Core.Nothing,
      mediaFormat = Core.Nothing,
      media = Core.Nothing,
      creationTime = Core.Nothing,
      completionTime = Core.Nothing,
      transcript = Core.Nothing,
      startTime = Core.Nothing,
      transcriptionJobStatus = Core.Nothing,
      specialty = Core.Nothing,
      failureReason = Core.Nothing,
      mediaSampleRateHertz = Core.Nothing,
      type' = Core.Nothing,
      medicalTranscriptionJobName = Core.Nothing,
      settings = Core.Nothing
    }

-- | The language code for the language spoken in the source audio file. US
-- English (en-US) is the only supported language for medical
-- transcriptions. Any other value you enter for language code results in a
-- @BadRequestException@ error.
medicalTranscriptionJob_languageCode :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe LanguageCode)
medicalTranscriptionJob_languageCode = Lens.lens (\MedicalTranscriptionJob' {languageCode} -> languageCode) (\s@MedicalTranscriptionJob' {} a -> s {languageCode = a} :: MedicalTranscriptionJob)

-- | The format of the input media file.
medicalTranscriptionJob_mediaFormat :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe MediaFormat)
medicalTranscriptionJob_mediaFormat = Lens.lens (\MedicalTranscriptionJob' {mediaFormat} -> mediaFormat) (\s@MedicalTranscriptionJob' {} a -> s {mediaFormat = a} :: MedicalTranscriptionJob)

-- | Undocumented member.
medicalTranscriptionJob_media :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Media)
medicalTranscriptionJob_media = Lens.lens (\MedicalTranscriptionJob' {media} -> media) (\s@MedicalTranscriptionJob' {} a -> s {media = a} :: MedicalTranscriptionJob)

-- | A timestamp that shows when the job was created.
medicalTranscriptionJob_creationTime :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Core.UTCTime)
medicalTranscriptionJob_creationTime = Lens.lens (\MedicalTranscriptionJob' {creationTime} -> creationTime) (\s@MedicalTranscriptionJob' {} a -> s {creationTime = a} :: MedicalTranscriptionJob) Core.. Lens.mapping Core._Time

-- | A timestamp that shows when the job was completed.
medicalTranscriptionJob_completionTime :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Core.UTCTime)
medicalTranscriptionJob_completionTime = Lens.lens (\MedicalTranscriptionJob' {completionTime} -> completionTime) (\s@MedicalTranscriptionJob' {} a -> s {completionTime = a} :: MedicalTranscriptionJob) Core.. Lens.mapping Core._Time

-- | An object that contains the @MedicalTranscript@. The @MedicalTranscript@
-- contains the @TranscriptFileUri@.
medicalTranscriptionJob_transcript :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe MedicalTranscript)
medicalTranscriptionJob_transcript = Lens.lens (\MedicalTranscriptionJob' {transcript} -> transcript) (\s@MedicalTranscriptionJob' {} a -> s {transcript = a} :: MedicalTranscriptionJob)

-- | A timestamp that shows when the job started processing.
medicalTranscriptionJob_startTime :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Core.UTCTime)
medicalTranscriptionJob_startTime = Lens.lens (\MedicalTranscriptionJob' {startTime} -> startTime) (\s@MedicalTranscriptionJob' {} a -> s {startTime = a} :: MedicalTranscriptionJob) Core.. Lens.mapping Core._Time

-- | The completion status of a medical transcription job.
medicalTranscriptionJob_transcriptionJobStatus :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe TranscriptionJobStatus)
medicalTranscriptionJob_transcriptionJobStatus = Lens.lens (\MedicalTranscriptionJob' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@MedicalTranscriptionJob' {} a -> s {transcriptionJobStatus = a} :: MedicalTranscriptionJob)

-- | The medical specialty of any clinicians providing a dictation or having
-- a conversation. @PRIMARYCARE@ is the only available setting for this
-- object. This specialty enables you to generate transcriptions for the
-- following medical fields:
--
-- -   Family Medicine
medicalTranscriptionJob_specialty :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Specialty)
medicalTranscriptionJob_specialty = Lens.lens (\MedicalTranscriptionJob' {specialty} -> specialty) (\s@MedicalTranscriptionJob' {} a -> s {specialty = a} :: MedicalTranscriptionJob)

-- | If the @TranscriptionJobStatus@ field is @FAILED@, this field contains
-- information about why the job failed.
--
-- The @FailureReason@ field contains one of the following values:
--
-- -   @Unsupported media format@- The media format specified in the
--     @MediaFormat@ field of the request isn\'t valid. See the description
--     of the @MediaFormat@ field for a list of valid values.
--
-- -   @The media format provided does not match the detected media format@-
--     The media format of the audio file doesn\'t match the format
--     specified in the @MediaFormat@ field in the request. Check the media
--     format of your media file and make sure the two values match.
--
-- -   @Invalid sample rate for audio file@- The sample rate specified in
--     the @MediaSampleRateHertz@ of the request isn\'t valid. The sample
--     rate must be between 8000 and 48000 Hertz.
--
-- -   @The sample rate provided does not match the detected sample rate@-
--     The sample rate in the audio file doesn\'t match the sample rate
--     specified in the @MediaSampleRateHertz@ field in the request. Check
--     the sample rate of your media file and make sure that the two values
--     match.
--
-- -   @Invalid file size: file size too large@- The size of your audio
--     file is larger than what Amazon Transcribe Medical can process. For
--     more information, see
--     <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and Quotas>
--     in the /Amazon Transcribe Medical Guide/
--
-- -   @Invalid number of channels: number of channels too large@- Your
--     audio contains more channels than Amazon Transcribe Medical is
--     configured to process. To request additional channels, see
--     <https://docs.aws.amazon.com/general/latest/gr/transcribe-medical.html Amazon Transcribe Medical Endpoints and Quotas>
--     in the /Amazon Web Services General Reference/
medicalTranscriptionJob_failureReason :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Core.Text)
medicalTranscriptionJob_failureReason = Lens.lens (\MedicalTranscriptionJob' {failureReason} -> failureReason) (\s@MedicalTranscriptionJob' {} a -> s {failureReason = a} :: MedicalTranscriptionJob)

-- | The sample rate, in Hertz, of the source audio containing medical
-- information.
--
-- If you don\'t specify the sample rate, Amazon Transcribe Medical
-- determines it for you. If you choose to specify the sample rate, it must
-- match the rate detected by Amazon Transcribe Medical. In most cases, you
-- should leave the @MediaSampleHertz@ blank and let Amazon Transcribe
-- Medical determine the sample rate.
medicalTranscriptionJob_mediaSampleRateHertz :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Core.Natural)
medicalTranscriptionJob_mediaSampleRateHertz = Lens.lens (\MedicalTranscriptionJob' {mediaSampleRateHertz} -> mediaSampleRateHertz) (\s@MedicalTranscriptionJob' {} a -> s {mediaSampleRateHertz = a} :: MedicalTranscriptionJob)

-- | The type of speech in the transcription job. @CONVERSATION@ is generally
-- used for patient-physician dialogues. @DICTATION@ is the setting for
-- physicians speaking their notes after seeing a patient. For more
-- information, see how-it-works-med
medicalTranscriptionJob_type :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Type)
medicalTranscriptionJob_type = Lens.lens (\MedicalTranscriptionJob' {type'} -> type') (\s@MedicalTranscriptionJob' {} a -> s {type' = a} :: MedicalTranscriptionJob)

-- | The name for a given medical transcription job.
medicalTranscriptionJob_medicalTranscriptionJobName :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Core.Text)
medicalTranscriptionJob_medicalTranscriptionJobName = Lens.lens (\MedicalTranscriptionJob' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@MedicalTranscriptionJob' {} a -> s {medicalTranscriptionJobName = a} :: MedicalTranscriptionJob)

-- | Object that contains object.
medicalTranscriptionJob_settings :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe MedicalTranscriptionSetting)
medicalTranscriptionJob_settings = Lens.lens (\MedicalTranscriptionJob' {settings} -> settings) (\s@MedicalTranscriptionJob' {} a -> s {settings = a} :: MedicalTranscriptionJob)

instance Core.FromJSON MedicalTranscriptionJob where
  parseJSON =
    Core.withObject
      "MedicalTranscriptionJob"
      ( \x ->
          MedicalTranscriptionJob'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "MediaFormat")
            Core.<*> (x Core..:? "Media")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "CompletionTime")
            Core.<*> (x Core..:? "Transcript")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "TranscriptionJobStatus")
            Core.<*> (x Core..:? "Specialty")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "MediaSampleRateHertz")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "MedicalTranscriptionJobName")
            Core.<*> (x Core..:? "Settings")
      )

instance Core.Hashable MedicalTranscriptionJob

instance Core.NFData MedicalTranscriptionJob
