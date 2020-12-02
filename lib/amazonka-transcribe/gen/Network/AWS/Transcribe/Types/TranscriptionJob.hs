{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.TranscriptionJob where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.JobExecutionSettings
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.Media
import Network.AWS.Transcribe.Types.MediaFormat
import Network.AWS.Transcribe.Types.ModelSettings
import Network.AWS.Transcribe.Types.Settings
import Network.AWS.Transcribe.Types.Transcript
import Network.AWS.Transcribe.Types.TranscriptionJobStatus

-- | Describes an asynchronous transcription job that was created with the @StartTranscriptionJob@ operation.
--
--
--
-- /See:/ 'transcriptionJob' smart constructor.
data TranscriptionJob = TranscriptionJob'
  { _tjCreationTime ::
      !(Maybe POSIX),
    _tjFailureReason :: !(Maybe Text),
    _tjContentRedaction :: !(Maybe ContentRedaction),
    _tjIdentifiedLanguageScore :: !(Maybe Double),
    _tjLanguageCode :: !(Maybe LanguageCode),
    _tjLanguageOptions :: !(Maybe (List1 LanguageCode)),
    _tjSettings :: !(Maybe Settings),
    _tjStartTime :: !(Maybe POSIX),
    _tjCompletionTime :: !(Maybe POSIX),
    _tjMedia :: !(Maybe Media),
    _tjMediaFormat :: !(Maybe MediaFormat),
    _tjModelSettings :: !(Maybe ModelSettings),
    _tjTranscriptionJobStatus ::
      !(Maybe TranscriptionJobStatus),
    _tjJobExecutionSettings :: !(Maybe JobExecutionSettings),
    _tjTranscriptionJobName :: !(Maybe Text),
    _tjIdentifyLanguage :: !(Maybe Bool),
    _tjTranscript :: !(Maybe Transcript),
    _tjMediaSampleRateHertz :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TranscriptionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjCreationTime' - A timestamp that shows when the job was created.
--
-- * 'tjFailureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed. The @FailureReason@ field can contain one of the following values:     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure that the two values match.     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.     * @Invalid file size: file size too large@ - The size of your audio file is larger than Amazon Transcribe can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Limits> in the /Amazon Transcribe Developer Guide/ .     * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits-amazon-transcribe Amazon Transcribe Limits> in the /Amazon Web Services General Reference/ .
--
-- * 'tjContentRedaction' - An object that describes content redaction settings for the transcription job.
--
-- * 'tjIdentifiedLanguageScore' - A value between zero and one that Amazon Transcribe assigned to the language that it identified in the source audio. Larger values indicate that Amazon Transcribe has higher confidence in the language it identified.
--
-- * 'tjLanguageCode' - The language code for the input speech.
--
-- * 'tjLanguageOptions' - An object that shows the optional array of languages inputted for transcription jobs with automatic language identification enabled.
--
-- * 'tjSettings' - Optional settings for the transcription job. Use these settings to turn on speaker recognition, to set the maximum number of speakers that should be identified and to specify a custom vocabulary to use when processing the transcription job.
--
-- * 'tjStartTime' - A timestamp that shows with the job was started processing.
--
-- * 'tjCompletionTime' - A timestamp that shows when the job was completed.
--
-- * 'tjMedia' - An object that describes the input media for the transcription job.
--
-- * 'tjMediaFormat' - The format of the input media file.
--
-- * 'tjModelSettings' - An object containing the details of your custom language model.
--
-- * 'tjTranscriptionJobStatus' - The status of the transcription job.
--
-- * 'tjJobExecutionSettings' - Provides information about how a transcription job is executed.
--
-- * 'tjTranscriptionJobName' - The name of the transcription job.
--
-- * 'tjIdentifyLanguage' - A value that shows if automatic language identification was enabled for a transcription job.
--
-- * 'tjTranscript' - An object that describes the output of the transcription job.
--
-- * 'tjMediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.
transcriptionJob ::
  TranscriptionJob
transcriptionJob =
  TranscriptionJob'
    { _tjCreationTime = Nothing,
      _tjFailureReason = Nothing,
      _tjContentRedaction = Nothing,
      _tjIdentifiedLanguageScore = Nothing,
      _tjLanguageCode = Nothing,
      _tjLanguageOptions = Nothing,
      _tjSettings = Nothing,
      _tjStartTime = Nothing,
      _tjCompletionTime = Nothing,
      _tjMedia = Nothing,
      _tjMediaFormat = Nothing,
      _tjModelSettings = Nothing,
      _tjTranscriptionJobStatus = Nothing,
      _tjJobExecutionSettings = Nothing,
      _tjTranscriptionJobName = Nothing,
      _tjIdentifyLanguage = Nothing,
      _tjTranscript = Nothing,
      _tjMediaSampleRateHertz = Nothing
    }

-- | A timestamp that shows when the job was created.
tjCreationTime :: Lens' TranscriptionJob (Maybe UTCTime)
tjCreationTime = lens _tjCreationTime (\s a -> s {_tjCreationTime = a}) . mapping _Time

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed. The @FailureReason@ field can contain one of the following values:     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure that the two values match.     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.     * @Invalid file size: file size too large@ - The size of your audio file is larger than Amazon Transcribe can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Limits> in the /Amazon Transcribe Developer Guide/ .     * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits-amazon-transcribe Amazon Transcribe Limits> in the /Amazon Web Services General Reference/ .
tjFailureReason :: Lens' TranscriptionJob (Maybe Text)
tjFailureReason = lens _tjFailureReason (\s a -> s {_tjFailureReason = a})

-- | An object that describes content redaction settings for the transcription job.
tjContentRedaction :: Lens' TranscriptionJob (Maybe ContentRedaction)
tjContentRedaction = lens _tjContentRedaction (\s a -> s {_tjContentRedaction = a})

-- | A value between zero and one that Amazon Transcribe assigned to the language that it identified in the source audio. Larger values indicate that Amazon Transcribe has higher confidence in the language it identified.
tjIdentifiedLanguageScore :: Lens' TranscriptionJob (Maybe Double)
tjIdentifiedLanguageScore = lens _tjIdentifiedLanguageScore (\s a -> s {_tjIdentifiedLanguageScore = a})

-- | The language code for the input speech.
tjLanguageCode :: Lens' TranscriptionJob (Maybe LanguageCode)
tjLanguageCode = lens _tjLanguageCode (\s a -> s {_tjLanguageCode = a})

-- | An object that shows the optional array of languages inputted for transcription jobs with automatic language identification enabled.
tjLanguageOptions :: Lens' TranscriptionJob (Maybe (NonEmpty LanguageCode))
tjLanguageOptions = lens _tjLanguageOptions (\s a -> s {_tjLanguageOptions = a}) . mapping _List1

-- | Optional settings for the transcription job. Use these settings to turn on speaker recognition, to set the maximum number of speakers that should be identified and to specify a custom vocabulary to use when processing the transcription job.
tjSettings :: Lens' TranscriptionJob (Maybe Settings)
tjSettings = lens _tjSettings (\s a -> s {_tjSettings = a})

-- | A timestamp that shows with the job was started processing.
tjStartTime :: Lens' TranscriptionJob (Maybe UTCTime)
tjStartTime = lens _tjStartTime (\s a -> s {_tjStartTime = a}) . mapping _Time

-- | A timestamp that shows when the job was completed.
tjCompletionTime :: Lens' TranscriptionJob (Maybe UTCTime)
tjCompletionTime = lens _tjCompletionTime (\s a -> s {_tjCompletionTime = a}) . mapping _Time

-- | An object that describes the input media for the transcription job.
tjMedia :: Lens' TranscriptionJob (Maybe Media)
tjMedia = lens _tjMedia (\s a -> s {_tjMedia = a})

-- | The format of the input media file.
tjMediaFormat :: Lens' TranscriptionJob (Maybe MediaFormat)
tjMediaFormat = lens _tjMediaFormat (\s a -> s {_tjMediaFormat = a})

-- | An object containing the details of your custom language model.
tjModelSettings :: Lens' TranscriptionJob (Maybe ModelSettings)
tjModelSettings = lens _tjModelSettings (\s a -> s {_tjModelSettings = a})

-- | The status of the transcription job.
tjTranscriptionJobStatus :: Lens' TranscriptionJob (Maybe TranscriptionJobStatus)
tjTranscriptionJobStatus = lens _tjTranscriptionJobStatus (\s a -> s {_tjTranscriptionJobStatus = a})

-- | Provides information about how a transcription job is executed.
tjJobExecutionSettings :: Lens' TranscriptionJob (Maybe JobExecutionSettings)
tjJobExecutionSettings = lens _tjJobExecutionSettings (\s a -> s {_tjJobExecutionSettings = a})

-- | The name of the transcription job.
tjTranscriptionJobName :: Lens' TranscriptionJob (Maybe Text)
tjTranscriptionJobName = lens _tjTranscriptionJobName (\s a -> s {_tjTranscriptionJobName = a})

-- | A value that shows if automatic language identification was enabled for a transcription job.
tjIdentifyLanguage :: Lens' TranscriptionJob (Maybe Bool)
tjIdentifyLanguage = lens _tjIdentifyLanguage (\s a -> s {_tjIdentifyLanguage = a})

-- | An object that describes the output of the transcription job.
tjTranscript :: Lens' TranscriptionJob (Maybe Transcript)
tjTranscript = lens _tjTranscript (\s a -> s {_tjTranscript = a})

-- | The sample rate, in Hertz, of the audio track in the input media file.
tjMediaSampleRateHertz :: Lens' TranscriptionJob (Maybe Natural)
tjMediaSampleRateHertz = lens _tjMediaSampleRateHertz (\s a -> s {_tjMediaSampleRateHertz = a}) . mapping _Nat

instance FromJSON TranscriptionJob where
  parseJSON =
    withObject
      "TranscriptionJob"
      ( \x ->
          TranscriptionJob'
            <$> (x .:? "CreationTime")
            <*> (x .:? "FailureReason")
            <*> (x .:? "ContentRedaction")
            <*> (x .:? "IdentifiedLanguageScore")
            <*> (x .:? "LanguageCode")
            <*> (x .:? "LanguageOptions")
            <*> (x .:? "Settings")
            <*> (x .:? "StartTime")
            <*> (x .:? "CompletionTime")
            <*> (x .:? "Media")
            <*> (x .:? "MediaFormat")
            <*> (x .:? "ModelSettings")
            <*> (x .:? "TranscriptionJobStatus")
            <*> (x .:? "JobExecutionSettings")
            <*> (x .:? "TranscriptionJobName")
            <*> (x .:? "IdentifyLanguage")
            <*> (x .:? "Transcript")
            <*> (x .:? "MediaSampleRateHertz")
      )

instance Hashable TranscriptionJob

instance NFData TranscriptionJob
