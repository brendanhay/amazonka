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
-- Module      : Network.AWS.Comprehend.StopDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a dominant language detection job in progress.
--
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is stopped and put into the @STOPPED@ state.
--
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception.
--
-- When a job is stopped, any documents already processed are written to the output location.
--
module Network.AWS.Comprehend.StopDominantLanguageDetectionJob
    (
    -- * Creating a Request
      stopDominantLanguageDetectionJob
    , StopDominantLanguageDetectionJob
    -- * Request Lenses
    , sdldjJobId

    -- * Destructuring the Response
    , stopDominantLanguageDetectionJobResponse
    , StopDominantLanguageDetectionJobResponse
    -- * Response Lenses
    , storsJobId
    , storsJobStatus
    , storsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopDominantLanguageDetectionJob' smart constructor.
newtype StopDominantLanguageDetectionJob = StopDominantLanguageDetectionJob'
  { _sdldjJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopDominantLanguageDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdldjJobId' - The identifier of the dominant language detection job to stop.
stopDominantLanguageDetectionJob
    :: Text -- ^ 'sdldjJobId'
    -> StopDominantLanguageDetectionJob
stopDominantLanguageDetectionJob pJobId_ =
  StopDominantLanguageDetectionJob' {_sdldjJobId = pJobId_}


-- | The identifier of the dominant language detection job to stop.
sdldjJobId :: Lens' StopDominantLanguageDetectionJob Text
sdldjJobId = lens _sdldjJobId (\ s a -> s{_sdldjJobId = a})

instance AWSRequest StopDominantLanguageDetectionJob
         where
        type Rs StopDominantLanguageDetectionJob =
             StopDominantLanguageDetectionJobResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 StopDominantLanguageDetectionJobResponse' <$>
                   (x .?> "JobId") <*> (x .?> "JobStatus") <*>
                     (pure (fromEnum s)))

instance Hashable StopDominantLanguageDetectionJob
         where

instance NFData StopDominantLanguageDetectionJob
         where

instance ToHeaders StopDominantLanguageDetectionJob
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.StopDominantLanguageDetectionJob"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopDominantLanguageDetectionJob
         where
        toJSON StopDominantLanguageDetectionJob'{..}
          = object (catMaybes [Just ("JobId" .= _sdldjJobId)])

instance ToPath StopDominantLanguageDetectionJob
         where
        toPath = const "/"

instance ToQuery StopDominantLanguageDetectionJob
         where
        toQuery = const mempty

-- | /See:/ 'stopDominantLanguageDetectionJobResponse' smart constructor.
data StopDominantLanguageDetectionJobResponse = StopDominantLanguageDetectionJobResponse'
  { _storsJobId          :: !(Maybe Text)
  , _storsJobStatus      :: !(Maybe JobStatus)
  , _storsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopDominantLanguageDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'storsJobId' - The identifier of the dominant language detection job to stop.
--
-- * 'storsJobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopDominantLanguageDetectionJob@ operation.
--
-- * 'storsResponseStatus' - -- | The response status code.
stopDominantLanguageDetectionJobResponse
    :: Int -- ^ 'storsResponseStatus'
    -> StopDominantLanguageDetectionJobResponse
stopDominantLanguageDetectionJobResponse pResponseStatus_ =
  StopDominantLanguageDetectionJobResponse'
    { _storsJobId = Nothing
    , _storsJobStatus = Nothing
    , _storsResponseStatus = pResponseStatus_
    }


-- | The identifier of the dominant language detection job to stop.
storsJobId :: Lens' StopDominantLanguageDetectionJobResponse (Maybe Text)
storsJobId = lens _storsJobId (\ s a -> s{_storsJobId = a})

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopDominantLanguageDetectionJob@ operation.
storsJobStatus :: Lens' StopDominantLanguageDetectionJobResponse (Maybe JobStatus)
storsJobStatus = lens _storsJobStatus (\ s a -> s{_storsJobStatus = a})

-- | -- | The response status code.
storsResponseStatus :: Lens' StopDominantLanguageDetectionJobResponse Int
storsResponseStatus = lens _storsResponseStatus (\ s a -> s{_storsResponseStatus = a})

instance NFData
           StopDominantLanguageDetectionJobResponse
         where
