{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscriptionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscriptionJob where

import Network.AWS.Lens
import Network.AWS.Prelude
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
--
--
-- /See:/ 'medicalTranscriptionJob' smart constructor.
data MedicalTranscriptionJob = MedicalTranscriptionJob'
  { _mtjCreationTime ::
      !(Maybe POSIX),
    _mtjSpecialty :: !(Maybe Specialty),
    _mtjFailureReason :: !(Maybe Text),
    _mtjLanguageCode :: !(Maybe LanguageCode),
    _mtjSettings ::
      !(Maybe MedicalTranscriptionSetting),
    _mtjStartTime :: !(Maybe POSIX),
    _mtjCompletionTime :: !(Maybe POSIX),
    _mtjMedia :: !(Maybe Media),
    _mtjMediaFormat :: !(Maybe MediaFormat),
    _mtjMedicalTranscriptionJobName ::
      !(Maybe Text),
    _mtjTranscriptionJobStatus ::
      !(Maybe TranscriptionJobStatus),
    _mtjType :: !(Maybe Type),
    _mtjTranscript ::
      !(Maybe MedicalTranscript),
    _mtjMediaSampleRateHertz :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MedicalTranscriptionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtjCreationTime' - A timestamp that shows when the job was created.
--
-- * 'mtjSpecialty' - The medical specialty of any clinicians providing a dictation or having a conversation. @PRIMARYCARE@ is the only available setting for this object. This specialty enables you to generate transcriptions for the following medical fields:     * Family Medicine
--
-- * 'mtjFailureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed. The @FailureReason@ field contains one of the following values:     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure the two values match.     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.     * @Invalid file size: file size too large@ - The size of your audio file is larger than what Amazon Transcribe Medical can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and Quotas> in the /Amazon Transcribe Medical Guide/      * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe Medical is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/transcribe-medical.html Amazon Transcribe Medical Endpoints and Quotas> in the /Amazon Web Services General Reference/
--
-- * 'mtjLanguageCode' - The language code for the language spoken in the source audio file. US English (en-US) is the only supported language for medical transcriptions. Any other value you enter for language code results in a @BadRequestException@ error.
--
-- * 'mtjSettings' - Object that contains object.
--
-- * 'mtjStartTime' - A timestamp that shows when the job started processing.
--
-- * 'mtjCompletionTime' - A timestamp that shows when the job was completed.
--
-- * 'mtjMedia' - Undocumented member.
--
-- * 'mtjMediaFormat' - The format of the input media file.
--
-- * 'mtjMedicalTranscriptionJobName' - The name for a given medical transcription job.
--
-- * 'mtjTranscriptionJobStatus' - The completion status of a medical transcription job.
--
-- * 'mtjType' - The type of speech in the transcription job. @CONVERSATION@ is generally used for patient-physician dialogues. @DICTATION@ is the setting for physicians speaking their notes after seeing a patient. For more information, see 'how-it-works-med'
--
-- * 'mtjTranscript' - An object that contains the @MedicalTranscript@ . The @MedicalTranscript@ contains the @TranscriptFileUri@ .
--
-- * 'mtjMediaSampleRateHertz' - The sample rate, in Hertz, of the source audio containing medical information. If you don't specify the sample rate, Amazon Transcribe Medical determines it for you. If you choose to specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleHertz@ blank and let Amazon Transcribe Medical determine the sample rate.
medicalTranscriptionJob ::
  MedicalTranscriptionJob
medicalTranscriptionJob =
  MedicalTranscriptionJob'
    { _mtjCreationTime = Nothing,
      _mtjSpecialty = Nothing,
      _mtjFailureReason = Nothing,
      _mtjLanguageCode = Nothing,
      _mtjSettings = Nothing,
      _mtjStartTime = Nothing,
      _mtjCompletionTime = Nothing,
      _mtjMedia = Nothing,
      _mtjMediaFormat = Nothing,
      _mtjMedicalTranscriptionJobName = Nothing,
      _mtjTranscriptionJobStatus = Nothing,
      _mtjType = Nothing,
      _mtjTranscript = Nothing,
      _mtjMediaSampleRateHertz = Nothing
    }

-- | A timestamp that shows when the job was created.
mtjCreationTime :: Lens' MedicalTranscriptionJob (Maybe UTCTime)
mtjCreationTime = lens _mtjCreationTime (\s a -> s {_mtjCreationTime = a}) . mapping _Time

-- | The medical specialty of any clinicians providing a dictation or having a conversation. @PRIMARYCARE@ is the only available setting for this object. This specialty enables you to generate transcriptions for the following medical fields:     * Family Medicine
mtjSpecialty :: Lens' MedicalTranscriptionJob (Maybe Specialty)
mtjSpecialty = lens _mtjSpecialty (\s a -> s {_mtjSpecialty = a})

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed. The @FailureReason@ field contains one of the following values:     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure the two values match.     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.     * @Invalid file size: file size too large@ - The size of your audio file is larger than what Amazon Transcribe Medical can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Guidelines and Quotas> in the /Amazon Transcribe Medical Guide/      * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe Medical is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/transcribe-medical.html Amazon Transcribe Medical Endpoints and Quotas> in the /Amazon Web Services General Reference/
mtjFailureReason :: Lens' MedicalTranscriptionJob (Maybe Text)
mtjFailureReason = lens _mtjFailureReason (\s a -> s {_mtjFailureReason = a})

-- | The language code for the language spoken in the source audio file. US English (en-US) is the only supported language for medical transcriptions. Any other value you enter for language code results in a @BadRequestException@ error.
mtjLanguageCode :: Lens' MedicalTranscriptionJob (Maybe LanguageCode)
mtjLanguageCode = lens _mtjLanguageCode (\s a -> s {_mtjLanguageCode = a})

-- | Object that contains object.
mtjSettings :: Lens' MedicalTranscriptionJob (Maybe MedicalTranscriptionSetting)
mtjSettings = lens _mtjSettings (\s a -> s {_mtjSettings = a})

-- | A timestamp that shows when the job started processing.
mtjStartTime :: Lens' MedicalTranscriptionJob (Maybe UTCTime)
mtjStartTime = lens _mtjStartTime (\s a -> s {_mtjStartTime = a}) . mapping _Time

-- | A timestamp that shows when the job was completed.
mtjCompletionTime :: Lens' MedicalTranscriptionJob (Maybe UTCTime)
mtjCompletionTime = lens _mtjCompletionTime (\s a -> s {_mtjCompletionTime = a}) . mapping _Time

-- | Undocumented member.
mtjMedia :: Lens' MedicalTranscriptionJob (Maybe Media)
mtjMedia = lens _mtjMedia (\s a -> s {_mtjMedia = a})

-- | The format of the input media file.
mtjMediaFormat :: Lens' MedicalTranscriptionJob (Maybe MediaFormat)
mtjMediaFormat = lens _mtjMediaFormat (\s a -> s {_mtjMediaFormat = a})

-- | The name for a given medical transcription job.
mtjMedicalTranscriptionJobName :: Lens' MedicalTranscriptionJob (Maybe Text)
mtjMedicalTranscriptionJobName = lens _mtjMedicalTranscriptionJobName (\s a -> s {_mtjMedicalTranscriptionJobName = a})

-- | The completion status of a medical transcription job.
mtjTranscriptionJobStatus :: Lens' MedicalTranscriptionJob (Maybe TranscriptionJobStatus)
mtjTranscriptionJobStatus = lens _mtjTranscriptionJobStatus (\s a -> s {_mtjTranscriptionJobStatus = a})

-- | The type of speech in the transcription job. @CONVERSATION@ is generally used for patient-physician dialogues. @DICTATION@ is the setting for physicians speaking their notes after seeing a patient. For more information, see 'how-it-works-med'
mtjType :: Lens' MedicalTranscriptionJob (Maybe Type)
mtjType = lens _mtjType (\s a -> s {_mtjType = a})

-- | An object that contains the @MedicalTranscript@ . The @MedicalTranscript@ contains the @TranscriptFileUri@ .
mtjTranscript :: Lens' MedicalTranscriptionJob (Maybe MedicalTranscript)
mtjTranscript = lens _mtjTranscript (\s a -> s {_mtjTranscript = a})

-- | The sample rate, in Hertz, of the source audio containing medical information. If you don't specify the sample rate, Amazon Transcribe Medical determines it for you. If you choose to specify the sample rate, it must match the rate detected by Amazon Transcribe Medical. In most cases, you should leave the @MediaSampleHertz@ blank and let Amazon Transcribe Medical determine the sample rate.
mtjMediaSampleRateHertz :: Lens' MedicalTranscriptionJob (Maybe Natural)
mtjMediaSampleRateHertz = lens _mtjMediaSampleRateHertz (\s a -> s {_mtjMediaSampleRateHertz = a}) . mapping _Nat

instance FromJSON MedicalTranscriptionJob where
  parseJSON =
    withObject
      "MedicalTranscriptionJob"
      ( \x ->
          MedicalTranscriptionJob'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Specialty")
            <*> (x .:? "FailureReason")
            <*> (x .:? "LanguageCode")
            <*> (x .:? "Settings")
            <*> (x .:? "StartTime")
            <*> (x .:? "CompletionTime")
            <*> (x .:? "Media")
            <*> (x .:? "MediaFormat")
            <*> (x .:? "MedicalTranscriptionJobName")
            <*> (x .:? "TranscriptionJobStatus")
            <*> (x .:? "Type")
            <*> (x .:? "Transcript")
            <*> (x .:? "MediaSampleRateHertz")
      )

instance Hashable MedicalTranscriptionJob

instance NFData MedicalTranscriptionJob
