{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Transcribe.Types.Sum

-- | Describes the input media file in a transcription request.
--
--
--
-- /See:/ 'media' smart constructor.
newtype Media = Media'
  { _mMediaFileURI :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Media' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMediaFileURI' - The S3 location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is: @https://s3-<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @  For example: @https://s3-us-east-1.amazonaws.com/examplebucket/example.mp4@  @https://s3-us-east-1.amazonaws.com/examplebucket/mediadocs/example.mp4@  For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
media
    :: Media
media = Media' {_mMediaFileURI = Nothing}


-- | The S3 location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is: @https://s3-<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @  For example: @https://s3-us-east-1.amazonaws.com/examplebucket/example.mp4@  @https://s3-us-east-1.amazonaws.com/examplebucket/mediadocs/example.mp4@  For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
mMediaFileURI :: Lens' Media (Maybe Text)
mMediaFileURI = lens _mMediaFileURI (\ s a -> s{_mMediaFileURI = a})

instance FromJSON Media where
        parseJSON
          = withObject "Media"
              (\ x -> Media' <$> (x .:? "MediaFileUri"))

instance Hashable Media where

instance NFData Media where

instance ToJSON Media where
        toJSON Media'{..}
          = object
              (catMaybes [("MediaFileUri" .=) <$> _mMediaFileURI])

-- | Provides optional settings for the @StartTranscriptionJob@ operation.
--
--
--
-- /See:/ 'settings' smart constructor.
data Settings = Settings'
  { _sVocabularyName        :: !(Maybe Text)
  , _sChannelIdentification :: !(Maybe Bool)
  , _sMaxSpeakerLabels      :: !(Maybe Nat)
  , _sShowSpeakerLabels     :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sVocabularyName' - The name of a vocabulary to use when processing the transcription job.
--
-- * 'sChannelIdentification' - Instructs Amazon Transcribe to process each audio channel separately and then merge the transcription output of each channel into a single transcription.  Amazon Transcribe also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of the item including the confidence that Amazon Transcribe has in the transcription. You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
--
-- * 'sMaxSpeakerLabels' - The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers will be identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
--
-- * 'sShowSpeakerLabels' - Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels @MaxSpeakerLabels@ field. You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
settings
    :: Settings
settings =
  Settings'
    { _sVocabularyName = Nothing
    , _sChannelIdentification = Nothing
    , _sMaxSpeakerLabels = Nothing
    , _sShowSpeakerLabels = Nothing
    }


-- | The name of a vocabulary to use when processing the transcription job.
sVocabularyName :: Lens' Settings (Maybe Text)
sVocabularyName = lens _sVocabularyName (\ s a -> s{_sVocabularyName = a})

-- | Instructs Amazon Transcribe to process each audio channel separately and then merge the transcription output of each channel into a single transcription.  Amazon Transcribe also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of the item including the confidence that Amazon Transcribe has in the transcription. You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
sChannelIdentification :: Lens' Settings (Maybe Bool)
sChannelIdentification = lens _sChannelIdentification (\ s a -> s{_sChannelIdentification = a})

-- | The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers will be identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
sMaxSpeakerLabels :: Lens' Settings (Maybe Natural)
sMaxSpeakerLabels = lens _sMaxSpeakerLabels (\ s a -> s{_sMaxSpeakerLabels = a}) . mapping _Nat

-- | Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels @MaxSpeakerLabels@ field. You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
sShowSpeakerLabels :: Lens' Settings (Maybe Bool)
sShowSpeakerLabels = lens _sShowSpeakerLabels (\ s a -> s{_sShowSpeakerLabels = a})

instance FromJSON Settings where
        parseJSON
          = withObject "Settings"
              (\ x ->
                 Settings' <$>
                   (x .:? "VocabularyName") <*>
                     (x .:? "ChannelIdentification")
                     <*> (x .:? "MaxSpeakerLabels")
                     <*> (x .:? "ShowSpeakerLabels"))

instance Hashable Settings where

instance NFData Settings where

instance ToJSON Settings where
        toJSON Settings'{..}
          = object
              (catMaybes
                 [("VocabularyName" .=) <$> _sVocabularyName,
                  ("ChannelIdentification" .=) <$>
                    _sChannelIdentification,
                  ("MaxSpeakerLabels" .=) <$> _sMaxSpeakerLabels,
                  ("ShowSpeakerLabels" .=) <$> _sShowSpeakerLabels])

-- | Identifies the location of a transcription.
--
--
--
-- /See:/ 'transcript' smart constructor.
newtype Transcript = Transcript'
  { _tTranscriptFileURI :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Transcript' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTranscriptFileURI' - The location where the transcription is stored. Use this URI to access the transcription. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcription in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
transcript
    :: Transcript
transcript = Transcript' {_tTranscriptFileURI = Nothing}


-- | The location where the transcription is stored. Use this URI to access the transcription. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcription in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
tTranscriptFileURI :: Lens' Transcript (Maybe Text)
tTranscriptFileURI = lens _tTranscriptFileURI (\ s a -> s{_tTranscriptFileURI = a})

instance FromJSON Transcript where
        parseJSON
          = withObject "Transcript"
              (\ x -> Transcript' <$> (x .:? "TranscriptFileUri"))

instance Hashable Transcript where

instance NFData Transcript where

-- | Describes an asynchronous transcription job that was created with the @StartTranscriptionJob@ operation.
--
--
--
-- /See:/ 'transcriptionJob' smart constructor.
data TranscriptionJob = TranscriptionJob'
  { _tjCreationTime           :: !(Maybe POSIX)
  , _tjFailureReason          :: !(Maybe Text)
  , _tjLanguageCode           :: !(Maybe LanguageCode)
  , _tjSettings               :: !(Maybe Settings)
  , _tjCompletionTime         :: !(Maybe POSIX)
  , _tjMedia                  :: !(Maybe Media)
  , _tjMediaFormat            :: !(Maybe MediaFormat)
  , _tjTranscriptionJobStatus :: !(Maybe TranscriptionJobStatus)
  , _tjTranscriptionJobName   :: !(Maybe Text)
  , _tjTranscript             :: !(Maybe Transcript)
  , _tjMediaSampleRateHertz   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TranscriptionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjCreationTime' - A timestamp that shows when the job was created.
--
-- * 'tjFailureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed. The @FailureReason@ field can contain one of the following values:     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure that the two values match.     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.     * @Invalid file size: file size too large@ - The size of your audio file is larger than Amazon Transcribe can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Limits> in the /Amazon Transcribe Developer Guide/ .     * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits-amazon-transcribe Amazon Transcribe Limits> in the /Amazon Web Services General Reference/ .
--
-- * 'tjLanguageCode' - The language code for the input speech.
--
-- * 'tjSettings' - Optional settings for the transcription job. Use these settings to turn on speaker recognition, to set the maximum number of speakers that should be identified and to specify a custom vocabulary to use when processing the transcription job.
--
-- * 'tjCompletionTime' - A timestamp that shows when the job was completed.
--
-- * 'tjMedia' - An object that describes the input media for the transcription job.
--
-- * 'tjMediaFormat' - The format of the input media file.
--
-- * 'tjTranscriptionJobStatus' - The status of the transcription job.
--
-- * 'tjTranscriptionJobName' - The name of the transcription job.
--
-- * 'tjTranscript' - An object that describes the output of the transcription job.
--
-- * 'tjMediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.
transcriptionJob
    :: TranscriptionJob
transcriptionJob =
  TranscriptionJob'
    { _tjCreationTime = Nothing
    , _tjFailureReason = Nothing
    , _tjLanguageCode = Nothing
    , _tjSettings = Nothing
    , _tjCompletionTime = Nothing
    , _tjMedia = Nothing
    , _tjMediaFormat = Nothing
    , _tjTranscriptionJobStatus = Nothing
    , _tjTranscriptionJobName = Nothing
    , _tjTranscript = Nothing
    , _tjMediaSampleRateHertz = Nothing
    }


-- | A timestamp that shows when the job was created.
tjCreationTime :: Lens' TranscriptionJob (Maybe UTCTime)
tjCreationTime = lens _tjCreationTime (\ s a -> s{_tjCreationTime = a}) . mapping _Time

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed. The @FailureReason@ field can contain one of the following values:     * @Unsupported media format@ - The media format specified in the @MediaFormat@ field of the request isn't valid. See the description of the @MediaFormat@ field for a list of valid values.     * @The media format provided does not match the detected media format@ - The media format of the audio file doesn't match the format specified in the @MediaFormat@ field in the request. Check the media format of your media file and make sure that the two values match.     * @Invalid sample rate for audio file@ - The sample rate specified in the @MediaSampleRateHertz@ of the request isn't valid. The sample rate must be between 8000 and 48000 Hertz.     * @The sample rate provided does not match the detected sample rate@ - The sample rate in the audio file doesn't match the sample rate specified in the @MediaSampleRateHertz@ field in the request. Check the sample rate of your media file and make sure that the two values match.     * @Invalid file size: file size too large@ - The size of your audio file is larger than Amazon Transcribe can process. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/limits-guidelines.html#limits Limits> in the /Amazon Transcribe Developer Guide/ .     * @Invalid number of channels: number of channels too large@ - Your audio contains more channels than Amazon Transcribe is configured to process. To request additional channels, see <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits-amazon-transcribe Amazon Transcribe Limits> in the /Amazon Web Services General Reference/ .
tjFailureReason :: Lens' TranscriptionJob (Maybe Text)
tjFailureReason = lens _tjFailureReason (\ s a -> s{_tjFailureReason = a})

-- | The language code for the input speech.
tjLanguageCode :: Lens' TranscriptionJob (Maybe LanguageCode)
tjLanguageCode = lens _tjLanguageCode (\ s a -> s{_tjLanguageCode = a})

-- | Optional settings for the transcription job. Use these settings to turn on speaker recognition, to set the maximum number of speakers that should be identified and to specify a custom vocabulary to use when processing the transcription job.
tjSettings :: Lens' TranscriptionJob (Maybe Settings)
tjSettings = lens _tjSettings (\ s a -> s{_tjSettings = a})

-- | A timestamp that shows when the job was completed.
tjCompletionTime :: Lens' TranscriptionJob (Maybe UTCTime)
tjCompletionTime = lens _tjCompletionTime (\ s a -> s{_tjCompletionTime = a}) . mapping _Time

-- | An object that describes the input media for the transcription job.
tjMedia :: Lens' TranscriptionJob (Maybe Media)
tjMedia = lens _tjMedia (\ s a -> s{_tjMedia = a})

-- | The format of the input media file.
tjMediaFormat :: Lens' TranscriptionJob (Maybe MediaFormat)
tjMediaFormat = lens _tjMediaFormat (\ s a -> s{_tjMediaFormat = a})

-- | The status of the transcription job.
tjTranscriptionJobStatus :: Lens' TranscriptionJob (Maybe TranscriptionJobStatus)
tjTranscriptionJobStatus = lens _tjTranscriptionJobStatus (\ s a -> s{_tjTranscriptionJobStatus = a})

-- | The name of the transcription job.
tjTranscriptionJobName :: Lens' TranscriptionJob (Maybe Text)
tjTranscriptionJobName = lens _tjTranscriptionJobName (\ s a -> s{_tjTranscriptionJobName = a})

-- | An object that describes the output of the transcription job.
tjTranscript :: Lens' TranscriptionJob (Maybe Transcript)
tjTranscript = lens _tjTranscript (\ s a -> s{_tjTranscript = a})

-- | The sample rate, in Hertz, of the audio track in the input media file.
tjMediaSampleRateHertz :: Lens' TranscriptionJob (Maybe Natural)
tjMediaSampleRateHertz = lens _tjMediaSampleRateHertz (\ s a -> s{_tjMediaSampleRateHertz = a}) . mapping _Nat

instance FromJSON TranscriptionJob where
        parseJSON
          = withObject "TranscriptionJob"
              (\ x ->
                 TranscriptionJob' <$>
                   (x .:? "CreationTime") <*> (x .:? "FailureReason")
                     <*> (x .:? "LanguageCode")
                     <*> (x .:? "Settings")
                     <*> (x .:? "CompletionTime")
                     <*> (x .:? "Media")
                     <*> (x .:? "MediaFormat")
                     <*> (x .:? "TranscriptionJobStatus")
                     <*> (x .:? "TranscriptionJobName")
                     <*> (x .:? "Transcript")
                     <*> (x .:? "MediaSampleRateHertz"))

instance Hashable TranscriptionJob where

instance NFData TranscriptionJob where

-- | Provides a summary of information about a transcription job. .
--
--
--
-- /See:/ 'transcriptionJobSummary' smart constructor.
data TranscriptionJobSummary = TranscriptionJobSummary'
  { _tjsCreationTime           :: !(Maybe POSIX)
  , _tjsFailureReason          :: !(Maybe Text)
  , _tjsLanguageCode           :: !(Maybe LanguageCode)
  , _tjsOutputLocationType     :: !(Maybe OutputLocationType)
  , _tjsCompletionTime         :: !(Maybe POSIX)
  , _tjsTranscriptionJobStatus :: !(Maybe TranscriptionJobStatus)
  , _tjsTranscriptionJobName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TranscriptionJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjsCreationTime' - A timestamp that shows when the job was created.
--
-- * 'tjsFailureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
--
-- * 'tjsLanguageCode' - The language code for the input speech.
--
-- * 'tjsOutputLocationType' - Indicates the location of the output of the transcription job. If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket specified in the @outputBucketName@ field when the transcription job was started with the @StartTranscriptionJob@ operation. If the value is @SERVICE_BUCKET@ then the output is stored by Amazon Transcribe and can be retrieved using the URI in the @GetTranscriptionJob@ response's @TranscriptFileUri@ field.
--
-- * 'tjsCompletionTime' - A timestamp that shows when the job was completed.
--
-- * 'tjsTranscriptionJobStatus' - The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
--
-- * 'tjsTranscriptionJobName' - The name of the transcription job.
transcriptionJobSummary
    :: TranscriptionJobSummary
transcriptionJobSummary =
  TranscriptionJobSummary'
    { _tjsCreationTime = Nothing
    , _tjsFailureReason = Nothing
    , _tjsLanguageCode = Nothing
    , _tjsOutputLocationType = Nothing
    , _tjsCompletionTime = Nothing
    , _tjsTranscriptionJobStatus = Nothing
    , _tjsTranscriptionJobName = Nothing
    }


-- | A timestamp that shows when the job was created.
tjsCreationTime :: Lens' TranscriptionJobSummary (Maybe UTCTime)
tjsCreationTime = lens _tjsCreationTime (\ s a -> s{_tjsCreationTime = a}) . mapping _Time

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
tjsFailureReason :: Lens' TranscriptionJobSummary (Maybe Text)
tjsFailureReason = lens _tjsFailureReason (\ s a -> s{_tjsFailureReason = a})

-- | The language code for the input speech.
tjsLanguageCode :: Lens' TranscriptionJobSummary (Maybe LanguageCode)
tjsLanguageCode = lens _tjsLanguageCode (\ s a -> s{_tjsLanguageCode = a})

-- | Indicates the location of the output of the transcription job. If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket specified in the @outputBucketName@ field when the transcription job was started with the @StartTranscriptionJob@ operation. If the value is @SERVICE_BUCKET@ then the output is stored by Amazon Transcribe and can be retrieved using the URI in the @GetTranscriptionJob@ response's @TranscriptFileUri@ field.
tjsOutputLocationType :: Lens' TranscriptionJobSummary (Maybe OutputLocationType)
tjsOutputLocationType = lens _tjsOutputLocationType (\ s a -> s{_tjsOutputLocationType = a})

-- | A timestamp that shows when the job was completed.
tjsCompletionTime :: Lens' TranscriptionJobSummary (Maybe UTCTime)
tjsCompletionTime = lens _tjsCompletionTime (\ s a -> s{_tjsCompletionTime = a}) . mapping _Time

-- | The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
tjsTranscriptionJobStatus :: Lens' TranscriptionJobSummary (Maybe TranscriptionJobStatus)
tjsTranscriptionJobStatus = lens _tjsTranscriptionJobStatus (\ s a -> s{_tjsTranscriptionJobStatus = a})

-- | The name of the transcription job.
tjsTranscriptionJobName :: Lens' TranscriptionJobSummary (Maybe Text)
tjsTranscriptionJobName = lens _tjsTranscriptionJobName (\ s a -> s{_tjsTranscriptionJobName = a})

instance FromJSON TranscriptionJobSummary where
        parseJSON
          = withObject "TranscriptionJobSummary"
              (\ x ->
                 TranscriptionJobSummary' <$>
                   (x .:? "CreationTime") <*> (x .:? "FailureReason")
                     <*> (x .:? "LanguageCode")
                     <*> (x .:? "OutputLocationType")
                     <*> (x .:? "CompletionTime")
                     <*> (x .:? "TranscriptionJobStatus")
                     <*> (x .:? "TranscriptionJobName"))

instance Hashable TranscriptionJobSummary where

instance NFData TranscriptionJobSummary where

-- | Provides information about a custom vocabulary.
--
--
--
-- /See:/ 'vocabularyInfo' smart constructor.
data VocabularyInfo = VocabularyInfo'
  { _viLanguageCode     :: !(Maybe LanguageCode)
  , _viVocabularyName   :: !(Maybe Text)
  , _viLastModifiedTime :: !(Maybe POSIX)
  , _viVocabularyState  :: !(Maybe VocabularyState)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VocabularyInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viLanguageCode' - The language code of the vocabulary entries.
--
-- * 'viVocabularyName' - The name of the vocabulary.
--
-- * 'viLastModifiedTime' - The date and time that the vocabulary was last modified.
--
-- * 'viVocabularyState' - The processing state of the vocabulary. If the state is @READY@ you can use the vocabulary in a @StartTranscriptionJob@ request.
vocabularyInfo
    :: VocabularyInfo
vocabularyInfo =
  VocabularyInfo'
    { _viLanguageCode = Nothing
    , _viVocabularyName = Nothing
    , _viLastModifiedTime = Nothing
    , _viVocabularyState = Nothing
    }


-- | The language code of the vocabulary entries.
viLanguageCode :: Lens' VocabularyInfo (Maybe LanguageCode)
viLanguageCode = lens _viLanguageCode (\ s a -> s{_viLanguageCode = a})

-- | The name of the vocabulary.
viVocabularyName :: Lens' VocabularyInfo (Maybe Text)
viVocabularyName = lens _viVocabularyName (\ s a -> s{_viVocabularyName = a})

-- | The date and time that the vocabulary was last modified.
viLastModifiedTime :: Lens' VocabularyInfo (Maybe UTCTime)
viLastModifiedTime = lens _viLastModifiedTime (\ s a -> s{_viLastModifiedTime = a}) . mapping _Time

-- | The processing state of the vocabulary. If the state is @READY@ you can use the vocabulary in a @StartTranscriptionJob@ request.
viVocabularyState :: Lens' VocabularyInfo (Maybe VocabularyState)
viVocabularyState = lens _viVocabularyState (\ s a -> s{_viVocabularyState = a})

instance FromJSON VocabularyInfo where
        parseJSON
          = withObject "VocabularyInfo"
              (\ x ->
                 VocabularyInfo' <$>
                   (x .:? "LanguageCode") <*> (x .:? "VocabularyName")
                     <*> (x .:? "LastModifiedTime")
                     <*> (x .:? "VocabularyState"))

instance Hashable VocabularyInfo where

instance NFData VocabularyInfo where
