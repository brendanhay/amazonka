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
-- Module      : Network.AWS.Transcribe.GetTranscriptionJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transcription job. To see the status of the job, check the @TranscriptionJobStatus@ field. If the status is @COMPLETED@ , the job is finished and you can find the results at the location specified in the @TranscriptionFileUri@ field.
--
--
module Network.AWS.Transcribe.GetTranscriptionJob
    (
    -- * Creating a Request
      getTranscriptionJob
    , GetTranscriptionJob
    -- * Request Lenses
    , gtjTranscriptionJobName

    -- * Destructuring the Response
    , getTranscriptionJobResponse
    , GetTranscriptionJobResponse
    -- * Response Lenses
    , gtjrsTranscriptionJob
    , gtjrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'getTranscriptionJob' smart constructor.
newtype GetTranscriptionJob = GetTranscriptionJob'
  { _gtjTranscriptionJobName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTranscriptionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtjTranscriptionJobName' - The name of the job.
getTranscriptionJob
    :: Text -- ^ 'gtjTranscriptionJobName'
    -> GetTranscriptionJob
getTranscriptionJob pTranscriptionJobName_ =
  GetTranscriptionJob' {_gtjTranscriptionJobName = pTranscriptionJobName_}


-- | The name of the job.
gtjTranscriptionJobName :: Lens' GetTranscriptionJob Text
gtjTranscriptionJobName = lens _gtjTranscriptionJobName (\ s a -> s{_gtjTranscriptionJobName = a})

instance AWSRequest GetTranscriptionJob where
        type Rs GetTranscriptionJob =
             GetTranscriptionJobResponse
        request = postJSON transcribe
        response
          = receiveJSON
              (\ s h x ->
                 GetTranscriptionJobResponse' <$>
                   (x .?> "TranscriptionJob") <*> (pure (fromEnum s)))

instance Hashable GetTranscriptionJob where

instance NFData GetTranscriptionJob where

instance ToHeaders GetTranscriptionJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.GetTranscriptionJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTranscriptionJob where
        toJSON GetTranscriptionJob'{..}
          = object
              (catMaybes
                 [Just
                    ("TranscriptionJobName" .=
                       _gtjTranscriptionJobName)])

instance ToPath GetTranscriptionJob where
        toPath = const "/"

instance ToQuery GetTranscriptionJob where
        toQuery = const mempty

-- | /See:/ 'getTranscriptionJobResponse' smart constructor.
data GetTranscriptionJobResponse = GetTranscriptionJobResponse'
  { _gtjrsTranscriptionJob :: !(Maybe TranscriptionJob)
  , _gtjrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTranscriptionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtjrsTranscriptionJob' - An object that contains the results of the transcription job.
--
-- * 'gtjrsResponseStatus' - -- | The response status code.
getTranscriptionJobResponse
    :: Int -- ^ 'gtjrsResponseStatus'
    -> GetTranscriptionJobResponse
getTranscriptionJobResponse pResponseStatus_ =
  GetTranscriptionJobResponse'
    {_gtjrsTranscriptionJob = Nothing, _gtjrsResponseStatus = pResponseStatus_}


-- | An object that contains the results of the transcription job.
gtjrsTranscriptionJob :: Lens' GetTranscriptionJobResponse (Maybe TranscriptionJob)
gtjrsTranscriptionJob = lens _gtjrsTranscriptionJob (\ s a -> s{_gtjrsTranscriptionJob = a})

-- | -- | The response status code.
gtjrsResponseStatus :: Lens' GetTranscriptionJobResponse Int
gtjrsResponseStatus = lens _gtjrsResponseStatus (\ s a -> s{_gtjrsResponseStatus = a})

instance NFData GetTranscriptionJobResponse where
