{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscriptionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscriptionJob
  ( MedicalTranscriptionJob (..),

    -- * Smart constructor
    mkMedicalTranscriptionJob,

    -- * Lenses
    mtjCreationTime,
    mtjSpecialty,
    mtjFailureReason,
    mtjLanguageCode,
    mtjSettings,
    mtjStartTime,
    mtjCompletionTime,
    mtjMedia,
    mtjMediaFormat,
    mtjMedicalTranscriptionJobName,
    mtjTranscriptionJobStatus,
    mtjType,
    mtjTranscript,
    mtjMediaSampleRateHertz,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.Media
import Network.AWS.Transcribe.Types.MediaFormat
import Network.AWS.Transcribe.Types.MedicalTranscript
import Network.AWS.Transcribe.Types.MedicalTranscriptionSetting
import Network.AWS.Transcribe.Types.Specialty
import Network.AWS.Transcribe.Types.TranscriptionJobStatus
import Network.AWS.Transcribe.Types.Type

-- | The data structure that contains the information for a medical transcription job.
--
-- /See:/ 'mkMedicalTranscriptionJob' smart constructor.
data MedicalTranscriptionJob = MedicalTranscriptionJob'
  { -- | A timestamp that shows when the job was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The medical specialty of any clinicians providing a dictation or having a conversation. @PRIMARYCARE@ is the only available setting for this object. This specialty enables you to generate transcriptions for the following medical fields:
    --
    --
    --     * Family Medicine
    specialty :: Lude.Maybe Specialty,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed.
    --
    -- The @FailureReason@ field contains one of the following values:
    --
    --     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.
    --
    --
    --     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure the two values match.
    --
    --
    --     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.
    --
    --
    --     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.
    --
    --
    --     * @Invalid file size: file size too large@ - The size of your audio file is larger than what Amazon Transcribe Medical can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and Quotas> in the /Amazon Transcribe Medical Guide/
    --
    --
    --     * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe Medical is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/transcribe-medical.html Amazon Transcribe Medical Endpoints and Quotas> in the /Amazon Web Services General Reference/
    failureReason :: Lude.Maybe Lude.Text,
    -- | The language code for the language spoken in the source audio file. US English (en-US) is the only supported language for medical transcriptions. Any other value you enter for language code results in a @BadRequestException@ error.
    languageCode :: Lude.Maybe LanguageCode,
    -- | Object that contains object.
    settings :: Lude.Maybe MedicalTranscriptionSetting,
    -- | A timestamp that shows when the job started processing.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Lude.Maybe Lude.Timestamp,
    media :: Lude.Maybe Media,
    -- | The format of the input media file.
    mediaFormat :: Lude.Maybe MediaFormat,
    -- | The name for a given medical transcription job.
    medicalTranscriptionJobName :: Lude.Maybe Lude.Text,
    -- | The completion status of a medical transcription job.
    transcriptionJobStatus :: Lude.Maybe TranscriptionJobStatus,
    -- | The type of speech in the transcription job. @CONVERSATION@ is generally used for patient-physician dialogues. @DICTATION@ is the setting for physicians speaking their notes after seeing a patient. For more information, see 'how-it-works-med'
    type' :: Lude.Maybe Type,
    -- | An object that contains the @MedicalTranscript@ . The @MedicalTranscript@ contains the @TranscriptFileUri@ .
    transcript :: Lude.Maybe MedicalTranscript,
    -- | The sample rate, in Hertz, of the source audio containing medical information.
    --
    -- If you don't specify the sample rate, Amazon Transcribe Medical determines it for you. If you choose to specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleHertz@ blank and let Amazon Transcribe Medical determine the sample rate.
    mediaSampleRateHertz :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MedicalTranscriptionJob' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the job was created.
-- * 'specialty' - The medical specialty of any clinicians providing a dictation or having a conversation. @PRIMARYCARE@ is the only available setting for this object. This specialty enables you to generate transcriptions for the following medical fields:
--
--
--     * Family Medicine
--
--
-- * 'failureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed.
--
-- The @FailureReason@ field contains one of the following values:
--
--     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.
--
--
--     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure the two values match.
--
--
--     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.
--
--
--     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.
--
--
--     * @Invalid file size: file size too large@ - The size of your audio file is larger than what Amazon Transcribe Medical can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and Quotas> in the /Amazon Transcribe Medical Guide/
--
--
--     * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe Medical is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/transcribe-medical.html Amazon Transcribe Medical Endpoints and Quotas> in the /Amazon Web Services General Reference/
--
--
-- * 'languageCode' - The language code for the language spoken in the source audio file. US English (en-US) is the only supported language for medical transcriptions. Any other value you enter for language code results in a @BadRequestException@ error.
-- * 'settings' - Object that contains object.
-- * 'startTime' - A timestamp that shows when the job started processing.
-- * 'completionTime' - A timestamp that shows when the job was completed.
-- * 'media' -
-- * 'mediaFormat' - The format of the input media file.
-- * 'medicalTranscriptionJobName' - The name for a given medical transcription job.
-- * 'transcriptionJobStatus' - The completion status of a medical transcription job.
-- * 'type'' - The type of speech in the transcription job. @CONVERSATION@ is generally used for patient-physician dialogues. @DICTATION@ is the setting for physicians speaking their notes after seeing a patient. For more information, see 'how-it-works-med'
-- * 'transcript' - An object that contains the @MedicalTranscript@ . The @MedicalTranscript@ contains the @TranscriptFileUri@ .
-- * 'mediaSampleRateHertz' - The sample rate, in Hertz, of the source audio containing medical information.
--
-- If you don't specify the sample rate, Amazon Transcribe Medical determines it for you. If you choose to specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleHertz@ blank and let Amazon Transcribe Medical determine the sample rate.
mkMedicalTranscriptionJob ::
  MedicalTranscriptionJob
mkMedicalTranscriptionJob =
  MedicalTranscriptionJob'
    { creationTime = Lude.Nothing,
      specialty = Lude.Nothing,
      failureReason = Lude.Nothing,
      languageCode = Lude.Nothing,
      settings = Lude.Nothing,
      startTime = Lude.Nothing,
      completionTime = Lude.Nothing,
      media = Lude.Nothing,
      mediaFormat = Lude.Nothing,
      medicalTranscriptionJobName = Lude.Nothing,
      transcriptionJobStatus = Lude.Nothing,
      type' = Lude.Nothing,
      transcript = Lude.Nothing,
      mediaSampleRateHertz = Lude.Nothing
    }

-- | A timestamp that shows when the job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjCreationTime :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe Lude.Timestamp)
mtjCreationTime = Lens.lens (creationTime :: MedicalTranscriptionJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The medical specialty of any clinicians providing a dictation or having a conversation. @PRIMARYCARE@ is the only available setting for this object. This specialty enables you to generate transcriptions for the following medical fields:
--
--
--     * Family Medicine
--
--
--
-- /Note:/ Consider using 'specialty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjSpecialty :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe Specialty)
mtjSpecialty = Lens.lens (specialty :: MedicalTranscriptionJob -> Lude.Maybe Specialty) (\s a -> s {specialty = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjSpecialty "Use generic-lens or generic-optics with 'specialty' instead." #-}

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed.
--
-- The @FailureReason@ field contains one of the following values:
--
--     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.
--
--
--     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure the two values match.
--
--
--     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.
--
--
--     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.
--
--
--     * @Invalid file size: file size too large@ - The size of your audio file is larger than what Amazon Transcribe Medical can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and Quotas> in the /Amazon Transcribe Medical Guide/
--
--
--     * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe Medical is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/transcribe-medical.html Amazon Transcribe Medical Endpoints and Quotas> in the /Amazon Web Services General Reference/
--
--
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjFailureReason :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe Lude.Text)
mtjFailureReason = Lens.lens (failureReason :: MedicalTranscriptionJob -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The language code for the language spoken in the source audio file. US English (en-US) is the only supported language for medical transcriptions. Any other value you enter for language code results in a @BadRequestException@ error.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjLanguageCode :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe LanguageCode)
mtjLanguageCode = Lens.lens (languageCode :: MedicalTranscriptionJob -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Object that contains object.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjSettings :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe MedicalTranscriptionSetting)
mtjSettings = Lens.lens (settings :: MedicalTranscriptionJob -> Lude.Maybe MedicalTranscriptionSetting) (\s a -> s {settings = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | A timestamp that shows when the job started processing.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjStartTime :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe Lude.Timestamp)
mtjStartTime = Lens.lens (startTime :: MedicalTranscriptionJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A timestamp that shows when the job was completed.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjCompletionTime :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe Lude.Timestamp)
mtjCompletionTime = Lens.lens (completionTime :: MedicalTranscriptionJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {completionTime = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'media' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjMedia :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe Media)
mtjMedia = Lens.lens (media :: MedicalTranscriptionJob -> Lude.Maybe Media) (\s a -> s {media = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjMedia "Use generic-lens or generic-optics with 'media' instead." #-}

-- | The format of the input media file.
--
-- /Note:/ Consider using 'mediaFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjMediaFormat :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe MediaFormat)
mtjMediaFormat = Lens.lens (mediaFormat :: MedicalTranscriptionJob -> Lude.Maybe MediaFormat) (\s a -> s {mediaFormat = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjMediaFormat "Use generic-lens or generic-optics with 'mediaFormat' instead." #-}

-- | The name for a given medical transcription job.
--
-- /Note:/ Consider using 'medicalTranscriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjMedicalTranscriptionJobName :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe Lude.Text)
mtjMedicalTranscriptionJobName = Lens.lens (medicalTranscriptionJobName :: MedicalTranscriptionJob -> Lude.Maybe Lude.Text) (\s a -> s {medicalTranscriptionJobName = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjMedicalTranscriptionJobName "Use generic-lens or generic-optics with 'medicalTranscriptionJobName' instead." #-}

-- | The completion status of a medical transcription job.
--
-- /Note:/ Consider using 'transcriptionJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjTranscriptionJobStatus :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe TranscriptionJobStatus)
mtjTranscriptionJobStatus = Lens.lens (transcriptionJobStatus :: MedicalTranscriptionJob -> Lude.Maybe TranscriptionJobStatus) (\s a -> s {transcriptionJobStatus = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjTranscriptionJobStatus "Use generic-lens or generic-optics with 'transcriptionJobStatus' instead." #-}

-- | The type of speech in the transcription job. @CONVERSATION@ is generally used for patient-physician dialogues. @DICTATION@ is the setting for physicians speaking their notes after seeing a patient. For more information, see 'how-it-works-med'
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjType :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe Type)
mtjType = Lens.lens (type' :: MedicalTranscriptionJob -> Lude.Maybe Type) (\s a -> s {type' = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | An object that contains the @MedicalTranscript@ . The @MedicalTranscript@ contains the @TranscriptFileUri@ .
--
-- /Note:/ Consider using 'transcript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjTranscript :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe MedicalTranscript)
mtjTranscript = Lens.lens (transcript :: MedicalTranscriptionJob -> Lude.Maybe MedicalTranscript) (\s a -> s {transcript = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjTranscript "Use generic-lens or generic-optics with 'transcript' instead." #-}

-- | The sample rate, in Hertz, of the source audio containing medical information.
--
-- If you don't specify the sample rate, Amazon Transcribe Medical determines it for you. If you choose to specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleHertz@ blank and let Amazon Transcribe Medical determine the sample rate.
--
-- /Note:/ Consider using 'mediaSampleRateHertz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjMediaSampleRateHertz :: Lens.Lens' MedicalTranscriptionJob (Lude.Maybe Lude.Natural)
mtjMediaSampleRateHertz = Lens.lens (mediaSampleRateHertz :: MedicalTranscriptionJob -> Lude.Maybe Lude.Natural) (\s a -> s {mediaSampleRateHertz = a} :: MedicalTranscriptionJob)
{-# DEPRECATED mtjMediaSampleRateHertz "Use generic-lens or generic-optics with 'mediaSampleRateHertz' instead." #-}

instance Lude.FromJSON MedicalTranscriptionJob where
  parseJSON =
    Lude.withObject
      "MedicalTranscriptionJob"
      ( \x ->
          MedicalTranscriptionJob'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Specialty")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "Settings")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "CompletionTime")
            Lude.<*> (x Lude..:? "Media")
            Lude.<*> (x Lude..:? "MediaFormat")
            Lude.<*> (x Lude..:? "MedicalTranscriptionJobName")
            Lude.<*> (x Lude..:? "TranscriptionJobStatus")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Transcript")
            Lude.<*> (x Lude..:? "MediaSampleRateHertz")
      )
