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
-- Module      : Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a key phrases detection job in progress.
--
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is stopped and put into the @STOPPED@ state.
--
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception.
--
-- When a job is stopped, any documents already processed are written to the output location.
--
module Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
    (
    -- * Creating a Request
      stopKeyPhrasesDetectionJob
    , StopKeyPhrasesDetectionJob
    -- * Request Lenses
    , skpdjJobId

    -- * Destructuring the Response
    , stopKeyPhrasesDetectionJobResponse
    , StopKeyPhrasesDetectionJobResponse
    -- * Response Lenses
    , skpdjrsJobId
    , skpdjrsJobStatus
    , skpdjrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopKeyPhrasesDetectionJob' smart constructor.
newtype StopKeyPhrasesDetectionJob = StopKeyPhrasesDetectionJob'
  { _skpdjJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopKeyPhrasesDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skpdjJobId' - The identifier of the key phrases detection job to stop.
stopKeyPhrasesDetectionJob
    :: Text -- ^ 'skpdjJobId'
    -> StopKeyPhrasesDetectionJob
stopKeyPhrasesDetectionJob pJobId_ =
  StopKeyPhrasesDetectionJob' {_skpdjJobId = pJobId_}


-- | The identifier of the key phrases detection job to stop.
skpdjJobId :: Lens' StopKeyPhrasesDetectionJob Text
skpdjJobId = lens _skpdjJobId (\ s a -> s{_skpdjJobId = a})

instance AWSRequest StopKeyPhrasesDetectionJob where
        type Rs StopKeyPhrasesDetectionJob =
             StopKeyPhrasesDetectionJobResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 StopKeyPhrasesDetectionJobResponse' <$>
                   (x .?> "JobId") <*> (x .?> "JobStatus") <*>
                     (pure (fromEnum s)))

instance Hashable StopKeyPhrasesDetectionJob where

instance NFData StopKeyPhrasesDetectionJob where

instance ToHeaders StopKeyPhrasesDetectionJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.StopKeyPhrasesDetectionJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopKeyPhrasesDetectionJob where
        toJSON StopKeyPhrasesDetectionJob'{..}
          = object (catMaybes [Just ("JobId" .= _skpdjJobId)])

instance ToPath StopKeyPhrasesDetectionJob where
        toPath = const "/"

instance ToQuery StopKeyPhrasesDetectionJob where
        toQuery = const mempty

-- | /See:/ 'stopKeyPhrasesDetectionJobResponse' smart constructor.
data StopKeyPhrasesDetectionJobResponse = StopKeyPhrasesDetectionJobResponse'
  { _skpdjrsJobId          :: !(Maybe Text)
  , _skpdjrsJobStatus      :: !(Maybe JobStatus)
  , _skpdjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopKeyPhrasesDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skpdjrsJobId' - The identifier of the key phrases detection job to stop.
--
-- * 'skpdjrsJobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopKeyPhrasesDetectionJob@ operation.
--
-- * 'skpdjrsResponseStatus' - -- | The response status code.
stopKeyPhrasesDetectionJobResponse
    :: Int -- ^ 'skpdjrsResponseStatus'
    -> StopKeyPhrasesDetectionJobResponse
stopKeyPhrasesDetectionJobResponse pResponseStatus_ =
  StopKeyPhrasesDetectionJobResponse'
    { _skpdjrsJobId = Nothing
    , _skpdjrsJobStatus = Nothing
    , _skpdjrsResponseStatus = pResponseStatus_
    }


-- | The identifier of the key phrases detection job to stop.
skpdjrsJobId :: Lens' StopKeyPhrasesDetectionJobResponse (Maybe Text)
skpdjrsJobId = lens _skpdjrsJobId (\ s a -> s{_skpdjrsJobId = a})

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopKeyPhrasesDetectionJob@ operation.
skpdjrsJobStatus :: Lens' StopKeyPhrasesDetectionJobResponse (Maybe JobStatus)
skpdjrsJobStatus = lens _skpdjrsJobStatus (\ s a -> s{_skpdjrsJobStatus = a})

-- | -- | The response status code.
skpdjrsResponseStatus :: Lens' StopKeyPhrasesDetectionJobResponse Int
skpdjrsResponseStatus = lens _skpdjrsResponseStatus (\ s a -> s{_skpdjrsResponseStatus = a})

instance NFData StopKeyPhrasesDetectionJobResponse
         where
