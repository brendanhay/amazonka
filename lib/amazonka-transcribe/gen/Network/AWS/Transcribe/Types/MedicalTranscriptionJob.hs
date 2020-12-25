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
    mtjCompletionTime,
    mtjCreationTime,
    mtjFailureReason,
    mtjLanguageCode,
    mtjMedia,
    mtjMediaFormat,
    mtjMediaSampleRateHertz,
    mtjMedicalTranscriptionJobName,
    mtjSettings,
    mtjSpecialty,
    mtjStartTime,
    mtjTranscript,
    mtjTranscriptionJobStatus,
    mtjType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.FailureReason as Types
import qualified Network.AWS.Transcribe.Types.LanguageCode as Types
import qualified Network.AWS.Transcribe.Types.Media as Types
import qualified Network.AWS.Transcribe.Types.MediaFormat as Types
import qualified Network.AWS.Transcribe.Types.MedicalTranscript as Types
import qualified Network.AWS.Transcribe.Types.MedicalTranscriptionJobName as Types
import qualified Network.AWS.Transcribe.Types.MedicalTranscriptionSetting as Types
import qualified Network.AWS.Transcribe.Types.Specialty as Types
import qualified Network.AWS.Transcribe.Types.TranscriptionJobStatus as Types
import qualified Network.AWS.Transcribe.Types.Type as Types

-- | The data structure that contains the information for a medical transcription job.
--
-- /See:/ 'mkMedicalTranscriptionJob' smart constructor.
data MedicalTranscriptionJob = MedicalTranscriptionJob'
  { -- | A timestamp that shows when the job was completed.
    completionTime :: Core.Maybe Core.NominalDiffTime,
    -- | A timestamp that shows when the job was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
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
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The language code for the language spoken in the source audio file. US English (en-US) is the only supported language for medical transcriptions. Any other value you enter for language code results in a @BadRequestException@ error.
    languageCode :: Core.Maybe Types.LanguageCode,
    media :: Core.Maybe Types.Media,
    -- | The format of the input media file.
    mediaFormat :: Core.Maybe Types.MediaFormat,
    -- | The sample rate, in Hertz, of the source audio containing medical information.
    --
    -- If you don't specify the sample rate, Amazon Transcribe Medical determines it for you. If you choose to specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleHertz@ blank and let Amazon Transcribe Medical determine the sample rate.
    mediaSampleRateHertz :: Core.Maybe Core.Natural,
    -- | The name for a given medical transcription job.
    medicalTranscriptionJobName :: Core.Maybe Types.MedicalTranscriptionJobName,
    -- | Object that contains object.
    settings :: Core.Maybe Types.MedicalTranscriptionSetting,
    -- | The medical specialty of any clinicians providing a dictation or having a conversation. @PRIMARYCARE@ is the only available setting for this object. This specialty enables you to generate transcriptions for the following medical fields:
    --
    --
    --     * Family Medicine
    specialty :: Core.Maybe Types.Specialty,
    -- | A timestamp that shows when the job started processing.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | An object that contains the @MedicalTranscript@ . The @MedicalTranscript@ contains the @TranscriptFileUri@ .
    transcript :: Core.Maybe Types.MedicalTranscript,
    -- | The completion status of a medical transcription job.
    transcriptionJobStatus :: Core.Maybe Types.TranscriptionJobStatus,
    -- | The type of speech in the transcription job. @CONVERSATION@ is generally used for patient-physician dialogues. @DICTATION@ is the setting for physicians speaking their notes after seeing a patient. For more information, see 'how-it-works-med'
    type' :: Core.Maybe Types.Type
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'MedicalTranscriptionJob' value with any optional fields omitted.
mkMedicalTranscriptionJob ::
  MedicalTranscriptionJob
mkMedicalTranscriptionJob =
  MedicalTranscriptionJob'
    { completionTime = Core.Nothing,
      creationTime = Core.Nothing,
      failureReason = Core.Nothing,
      languageCode = Core.Nothing,
      media = Core.Nothing,
      mediaFormat = Core.Nothing,
      mediaSampleRateHertz = Core.Nothing,
      medicalTranscriptionJobName = Core.Nothing,
      settings = Core.Nothing,
      specialty = Core.Nothing,
      startTime = Core.Nothing,
      transcript = Core.Nothing,
      transcriptionJobStatus = Core.Nothing,
      type' = Core.Nothing
    }

-- | A timestamp that shows when the job was completed.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjCompletionTime :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Core.NominalDiffTime)
mtjCompletionTime = Lens.field @"completionTime"
{-# DEPRECATED mtjCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

-- | A timestamp that shows when the job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjCreationTime :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Core.NominalDiffTime)
mtjCreationTime = Lens.field @"creationTime"
{-# DEPRECATED mtjCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

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
mtjFailureReason :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Types.FailureReason)
mtjFailureReason = Lens.field @"failureReason"
{-# DEPRECATED mtjFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The language code for the language spoken in the source audio file. US English (en-US) is the only supported language for medical transcriptions. Any other value you enter for language code results in a @BadRequestException@ error.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjLanguageCode :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Types.LanguageCode)
mtjLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED mtjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'media' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjMedia :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Types.Media)
mtjMedia = Lens.field @"media"
{-# DEPRECATED mtjMedia "Use generic-lens or generic-optics with 'media' instead." #-}

-- | The format of the input media file.
--
-- /Note:/ Consider using 'mediaFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjMediaFormat :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Types.MediaFormat)
mtjMediaFormat = Lens.field @"mediaFormat"
{-# DEPRECATED mtjMediaFormat "Use generic-lens or generic-optics with 'mediaFormat' instead." #-}

-- | The sample rate, in Hertz, of the source audio containing medical information.
--
-- If you don't specify the sample rate, Amazon Transcribe Medical determines it for you. If you choose to specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleHertz@ blank and let Amazon Transcribe Medical determine the sample rate.
--
-- /Note:/ Consider using 'mediaSampleRateHertz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjMediaSampleRateHertz :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Core.Natural)
mtjMediaSampleRateHertz = Lens.field @"mediaSampleRateHertz"
{-# DEPRECATED mtjMediaSampleRateHertz "Use generic-lens or generic-optics with 'mediaSampleRateHertz' instead." #-}

-- | The name for a given medical transcription job.
--
-- /Note:/ Consider using 'medicalTranscriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjMedicalTranscriptionJobName :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Types.MedicalTranscriptionJobName)
mtjMedicalTranscriptionJobName = Lens.field @"medicalTranscriptionJobName"
{-# DEPRECATED mtjMedicalTranscriptionJobName "Use generic-lens or generic-optics with 'medicalTranscriptionJobName' instead." #-}

-- | Object that contains object.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjSettings :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Types.MedicalTranscriptionSetting)
mtjSettings = Lens.field @"settings"
{-# DEPRECATED mtjSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The medical specialty of any clinicians providing a dictation or having a conversation. @PRIMARYCARE@ is the only available setting for this object. This specialty enables you to generate transcriptions for the following medical fields:
--
--
--     * Family Medicine
--
--
--
-- /Note:/ Consider using 'specialty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjSpecialty :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Types.Specialty)
mtjSpecialty = Lens.field @"specialty"
{-# DEPRECATED mtjSpecialty "Use generic-lens or generic-optics with 'specialty' instead." #-}

-- | A timestamp that shows when the job started processing.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjStartTime :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Core.NominalDiffTime)
mtjStartTime = Lens.field @"startTime"
{-# DEPRECATED mtjStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | An object that contains the @MedicalTranscript@ . The @MedicalTranscript@ contains the @TranscriptFileUri@ .
--
-- /Note:/ Consider using 'transcript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjTranscript :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Types.MedicalTranscript)
mtjTranscript = Lens.field @"transcript"
{-# DEPRECATED mtjTranscript "Use generic-lens or generic-optics with 'transcript' instead." #-}

-- | The completion status of a medical transcription job.
--
-- /Note:/ Consider using 'transcriptionJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjTranscriptionJobStatus :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Types.TranscriptionJobStatus)
mtjTranscriptionJobStatus = Lens.field @"transcriptionJobStatus"
{-# DEPRECATED mtjTranscriptionJobStatus "Use generic-lens or generic-optics with 'transcriptionJobStatus' instead." #-}

-- | The type of speech in the transcription job. @CONVERSATION@ is generally used for patient-physician dialogues. @DICTATION@ is the setting for physicians speaking their notes after seeing a patient. For more information, see 'how-it-works-med'
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtjType :: Lens.Lens' MedicalTranscriptionJob (Core.Maybe Types.Type)
mtjType = Lens.field @"type'"
{-# DEPRECATED mtjType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON MedicalTranscriptionJob where
  parseJSON =
    Core.withObject "MedicalTranscriptionJob" Core.$
      \x ->
        MedicalTranscriptionJob'
          Core.<$> (x Core..:? "CompletionTime")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "FailureReason")
          Core.<*> (x Core..:? "LanguageCode")
          Core.<*> (x Core..:? "Media")
          Core.<*> (x Core..:? "MediaFormat")
          Core.<*> (x Core..:? "MediaSampleRateHertz")
          Core.<*> (x Core..:? "MedicalTranscriptionJobName")
          Core.<*> (x Core..:? "Settings")
          Core.<*> (x Core..:? "Specialty")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "Transcript")
          Core.<*> (x Core..:? "TranscriptionJobStatus")
          Core.<*> (x Core..:? "Type")
