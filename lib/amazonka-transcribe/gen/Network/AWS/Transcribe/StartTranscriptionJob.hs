{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.StartTranscriptionJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous job to transcribe speech to text.
--
--
module Network.AWS.Transcribe.StartTranscriptionJob
    (
    -- * Creating a Request
      startTranscriptionJob
    , StartTranscriptionJob
    -- * Request Lenses
    , stjSettings
    , stjMediaSampleRateHertz
    , stjTranscriptionJobName
    , stjLanguageCode
    , stjMediaFormat
    , stjMedia

    -- * Destructuring the Response
    , startTranscriptionJobResponse
    , StartTranscriptionJobResponse
    -- * Response Lenses
    , stjrsTranscriptionJob
    , stjrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'startTranscriptionJob' smart constructor.
data StartTranscriptionJob = StartTranscriptionJob'
  { _stjSettings             :: !(Maybe Settings)
  , _stjMediaSampleRateHertz :: !(Maybe Nat)
  , _stjTranscriptionJobName :: !Text
  , _stjLanguageCode         :: !LanguageCode
  , _stjMediaFormat          :: !MediaFormat
  , _stjMedia                :: !Media
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartTranscriptionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stjSettings' - A @Settings@ object that provides optional settings for a transcription job.
--
-- * 'stjMediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.
--
-- * 'stjTranscriptionJobName' - The name of the job. The name must be unique within an AWS account.
--
-- * 'stjLanguageCode' - The language code for the language used in the input media file.
--
-- * 'stjMediaFormat' - The format of the input media file.
--
-- * 'stjMedia' - An object that describes the input media for a transcription job.
startTranscriptionJob
    :: Text -- ^ 'stjTranscriptionJobName'
    -> LanguageCode -- ^ 'stjLanguageCode'
    -> MediaFormat -- ^ 'stjMediaFormat'
    -> Media -- ^ 'stjMedia'
    -> StartTranscriptionJob
startTranscriptionJob pTranscriptionJobName_ pLanguageCode_ pMediaFormat_ pMedia_ =
  StartTranscriptionJob'
    { _stjSettings = Nothing
    , _stjMediaSampleRateHertz = Nothing
    , _stjTranscriptionJobName = pTranscriptionJobName_
    , _stjLanguageCode = pLanguageCode_
    , _stjMediaFormat = pMediaFormat_
    , _stjMedia = pMedia_
    }


-- | A @Settings@ object that provides optional settings for a transcription job.
stjSettings :: Lens' StartTranscriptionJob (Maybe Settings)
stjSettings = lens _stjSettings (\ s a -> s{_stjSettings = a})

-- | The sample rate, in Hertz, of the audio track in the input media file.
stjMediaSampleRateHertz :: Lens' StartTranscriptionJob (Maybe Natural)
stjMediaSampleRateHertz = lens _stjMediaSampleRateHertz (\ s a -> s{_stjMediaSampleRateHertz = a}) . mapping _Nat

-- | The name of the job. The name must be unique within an AWS account.
stjTranscriptionJobName :: Lens' StartTranscriptionJob Text
stjTranscriptionJobName = lens _stjTranscriptionJobName (\ s a -> s{_stjTranscriptionJobName = a})

-- | The language code for the language used in the input media file.
stjLanguageCode :: Lens' StartTranscriptionJob LanguageCode
stjLanguageCode = lens _stjLanguageCode (\ s a -> s{_stjLanguageCode = a})

-- | The format of the input media file.
stjMediaFormat :: Lens' StartTranscriptionJob MediaFormat
stjMediaFormat = lens _stjMediaFormat (\ s a -> s{_stjMediaFormat = a})

-- | An object that describes the input media for a transcription job.
stjMedia :: Lens' StartTranscriptionJob Media
stjMedia = lens _stjMedia (\ s a -> s{_stjMedia = a})

instance AWSRequest StartTranscriptionJob where
        type Rs StartTranscriptionJob =
             StartTranscriptionJobResponse
        request = postJSON transcribe
        response
          = receiveJSON
              (\ s h x ->
                 StartTranscriptionJobResponse' <$>
                   (x .?> "TranscriptionJob") <*> (pure (fromEnum s)))

instance Hashable StartTranscriptionJob where

instance NFData StartTranscriptionJob where

instance ToHeaders StartTranscriptionJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.StartTranscriptionJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartTranscriptionJob where
        toJSON StartTranscriptionJob'{..}
          = object
              (catMaybes
                 [("Settings" .=) <$> _stjSettings,
                  ("MediaSampleRateHertz" .=) <$>
                    _stjMediaSampleRateHertz,
                  Just
                    ("TranscriptionJobName" .= _stjTranscriptionJobName),
                  Just ("LanguageCode" .= _stjLanguageCode),
                  Just ("MediaFormat" .= _stjMediaFormat),
                  Just ("Media" .= _stjMedia)])

instance ToPath StartTranscriptionJob where
        toPath = const "/"

instance ToQuery StartTranscriptionJob where
        toQuery = const mempty

-- | /See:/ 'startTranscriptionJobResponse' smart constructor.
data StartTranscriptionJobResponse = StartTranscriptionJobResponse'
  { _stjrsTranscriptionJob :: !(Maybe TranscriptionJob)
  , _stjrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartTranscriptionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stjrsTranscriptionJob' - An object containing details of the asynchronous transcription job.
--
-- * 'stjrsResponseStatus' - -- | The response status code.
startTranscriptionJobResponse
    :: Int -- ^ 'stjrsResponseStatus'
    -> StartTranscriptionJobResponse
startTranscriptionJobResponse pResponseStatus_ =
  StartTranscriptionJobResponse'
    {_stjrsTranscriptionJob = Nothing, _stjrsResponseStatus = pResponseStatus_}


-- | An object containing details of the asynchronous transcription job.
stjrsTranscriptionJob :: Lens' StartTranscriptionJobResponse (Maybe TranscriptionJob)
stjrsTranscriptionJob = lens _stjrsTranscriptionJob (\ s a -> s{_stjrsTranscriptionJob = a})

-- | -- | The response status code.
stjrsResponseStatus :: Lens' StartTranscriptionJobResponse Int
stjrsResponseStatus = lens _stjrsResponseStatus (\ s a -> s{_stjrsResponseStatus = a})

instance NFData StartTranscriptionJobResponse where
