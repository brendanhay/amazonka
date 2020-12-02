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
-- * 'mMediaFileURI' - The S3 location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is: @https://<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @  For example: @https://s3-us-east-1.amazonaws.com/examplebucket/example.mp4@  @https://s3-us-east-1.amazonaws.com/examplebucket/mediadocs/example.mp4@  For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
media
    :: Media
media = Media' {_mMediaFileURI = Nothing}


-- | The S3 location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is: @https://<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @  For example: @https://s3-us-east-1.amazonaws.com/examplebucket/example.mp4@  @https://s3-us-east-1.amazonaws.com/examplebucket/mediadocs/example.mp4@  For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
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
  { _sVocabularyName    :: !(Maybe Text)
  , _sMaxSpeakerLabels  :: !(Maybe Nat)
  , _sShowSpeakerLabels :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sVocabularyName' - The name of a vocabulary to use when processing the transcription job.
--
-- * 'sMaxSpeakerLabels' - The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers will be identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
--
-- * 'sShowSpeakerLabels' - Determines whether the transcription job should use speaker recognition to identify different speakers in the input audio. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels @MaxSpeakerLabels@ field.
settings
    :: Settings
settings =
  Settings'
    { _sVocabularyName = Nothing
    , _sMaxSpeakerLabels = Nothing
    , _sShowSpeakerLabels = Nothing
    }


-- | The name of a vocabulary to use when processing the transcription job.
sVocabularyName :: Lens' Settings (Maybe Text)
sVocabularyName = lens _sVocabularyName (\ s a -> s{_sVocabularyName = a})

-- | The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers will be identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
sMaxSpeakerLabels :: Lens' Settings (Maybe Natural)
sMaxSpeakerLabels = lens _sMaxSpeakerLabels (\ s a -> s{_sMaxSpeakerLabels = a}) . mapping _Nat

-- | Determines whether the transcription job should use speaker recognition to identify different speakers in the input audio. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels @MaxSpeakerLabels@ field.
sShowSpeakerLabels :: Lens' Settings (Maybe Bool)
sShowSpeakerLabels = lens _sShowSpeakerLabels (\ s a -> s{_sShowSpeakerLabels = a})

instance FromJSON Settings where
        parseJSON
          = withObject "Settings"
              (\ x ->
                 Settings' <$>
                   (x .:? "VocabularyName") <*>
                     (x .:? "MaxSpeakerLabels")
                     <*> (x .:? "ShowSpeakerLabels"))

instance Hashable Settings where

instance NFData Settings where

instance ToJSON Settings where
        toJSON Settings'{..}
          = object
              (catMaybes
                 [("VocabularyName" .=) <$> _sVocabularyName,
                  ("MaxSpeakerLabels" .=) <$> _sMaxSpeakerLabels,
                  ("ShowSpeakerLabels" .=) <$> _sShowSpeakerLabels])

-- | Describes the output of a transcription job.
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
-- * 'tTranscriptFileURI' - The S3 location where the transcription result is stored. Use this URI to access the results of the transcription job.
transcript
    :: Transcript
transcript = Transcript' {_tTranscriptFileURI = Nothing}


-- | The S3 location where the transcription result is stored. Use this URI to access the results of the transcription job.
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
-- * 'tjCreationTime' - Timestamp of the date and time that the job was created.
--
-- * 'tjFailureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed.
--
-- * 'tjLanguageCode' - The language code for the input speech.
--
-- * 'tjSettings' - Optional settings for the transcription job.
--
-- * 'tjCompletionTime' - Timestamp of the date and time that the job completed.
--
-- * 'tjMedia' - An object that describes the input media for a transcription job.
--
-- * 'tjMediaFormat' - The format of the input media file.
--
-- * 'tjTranscriptionJobStatus' - The status of the transcription job.
--
-- * 'tjTranscriptionJobName' - A name to identify the transcription job.
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


-- | Timestamp of the date and time that the job was created.
tjCreationTime :: Lens' TranscriptionJob (Maybe UTCTime)
tjCreationTime = lens _tjCreationTime (\ s a -> s{_tjCreationTime = a}) . mapping _Time

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains information about why the job failed.
tjFailureReason :: Lens' TranscriptionJob (Maybe Text)
tjFailureReason = lens _tjFailureReason (\ s a -> s{_tjFailureReason = a})

-- | The language code for the input speech.
tjLanguageCode :: Lens' TranscriptionJob (Maybe LanguageCode)
tjLanguageCode = lens _tjLanguageCode (\ s a -> s{_tjLanguageCode = a})

-- | Optional settings for the transcription job.
tjSettings :: Lens' TranscriptionJob (Maybe Settings)
tjSettings = lens _tjSettings (\ s a -> s{_tjSettings = a})

-- | Timestamp of the date and time that the job completed.
tjCompletionTime :: Lens' TranscriptionJob (Maybe UTCTime)
tjCompletionTime = lens _tjCompletionTime (\ s a -> s{_tjCompletionTime = a}) . mapping _Time

-- | An object that describes the input media for a transcription job.
tjMedia :: Lens' TranscriptionJob (Maybe Media)
tjMedia = lens _tjMedia (\ s a -> s{_tjMedia = a})

-- | The format of the input media file.
tjMediaFormat :: Lens' TranscriptionJob (Maybe MediaFormat)
tjMediaFormat = lens _tjMediaFormat (\ s a -> s{_tjMediaFormat = a})

-- | The status of the transcription job.
tjTranscriptionJobStatus :: Lens' TranscriptionJob (Maybe TranscriptionJobStatus)
tjTranscriptionJobStatus = lens _tjTranscriptionJobStatus (\ s a -> s{_tjTranscriptionJobStatus = a})

-- | A name to identify the transcription job.
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

-- | Provides a summary of information about a transcription job.
--
--
--
-- /See:/ 'transcriptionJobSummary' smart constructor.
data TranscriptionJobSummary = TranscriptionJobSummary'
  { _tjsCreationTime           :: !(Maybe POSIX)
  , _tjsFailureReason          :: !(Maybe Text)
  , _tjsLanguageCode           :: !(Maybe LanguageCode)
  , _tjsCompletionTime         :: !(Maybe POSIX)
  , _tjsTranscriptionJobStatus :: !(Maybe TranscriptionJobStatus)
  , _tjsTranscriptionJobName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TranscriptionJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjsCreationTime' - Timestamp of the date and time that the job was created.
--
-- * 'tjsFailureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains a description of the error.
--
-- * 'tjsLanguageCode' - The language code for the input speech.
--
-- * 'tjsCompletionTime' - Timestamp of the date and time that the job completed.
--
-- * 'tjsTranscriptionJobStatus' - The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
--
-- * 'tjsTranscriptionJobName' - The name assigned to the transcription job when it was created.
transcriptionJobSummary
    :: TranscriptionJobSummary
transcriptionJobSummary =
  TranscriptionJobSummary'
    { _tjsCreationTime = Nothing
    , _tjsFailureReason = Nothing
    , _tjsLanguageCode = Nothing
    , _tjsCompletionTime = Nothing
    , _tjsTranscriptionJobStatus = Nothing
    , _tjsTranscriptionJobName = Nothing
    }


-- | Timestamp of the date and time that the job was created.
tjsCreationTime :: Lens' TranscriptionJobSummary (Maybe UTCTime)
tjsCreationTime = lens _tjsCreationTime (\ s a -> s{_tjsCreationTime = a}) . mapping _Time

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , this field contains a description of the error.
tjsFailureReason :: Lens' TranscriptionJobSummary (Maybe Text)
tjsFailureReason = lens _tjsFailureReason (\ s a -> s{_tjsFailureReason = a})

-- | The language code for the input speech.
tjsLanguageCode :: Lens' TranscriptionJobSummary (Maybe LanguageCode)
tjsLanguageCode = lens _tjsLanguageCode (\ s a -> s{_tjsLanguageCode = a})

-- | Timestamp of the date and time that the job completed.
tjsCompletionTime :: Lens' TranscriptionJobSummary (Maybe UTCTime)
tjsCompletionTime = lens _tjsCompletionTime (\ s a -> s{_tjsCompletionTime = a}) . mapping _Time

-- | The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
tjsTranscriptionJobStatus :: Lens' TranscriptionJobSummary (Maybe TranscriptionJobStatus)
tjsTranscriptionJobStatus = lens _tjsTranscriptionJobStatus (\ s a -> s{_tjsTranscriptionJobStatus = a})

-- | The name assigned to the transcription job when it was created.
tjsTranscriptionJobName :: Lens' TranscriptionJobSummary (Maybe Text)
tjsTranscriptionJobName = lens _tjsTranscriptionJobName (\ s a -> s{_tjsTranscriptionJobName = a})

instance FromJSON TranscriptionJobSummary where
        parseJSON
          = withObject "TranscriptionJobSummary"
              (\ x ->
                 TranscriptionJobSummary' <$>
                   (x .:? "CreationTime") <*> (x .:? "FailureReason")
                     <*> (x .:? "LanguageCode")
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
