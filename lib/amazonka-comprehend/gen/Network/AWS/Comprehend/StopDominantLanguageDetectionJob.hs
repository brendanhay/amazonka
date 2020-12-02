{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
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
module Network.AWS.Comprehend.StopDominantLanguageDetectionJob
  ( -- * Creating a Request
    stopDominantLanguageDetectionJob,
    StopDominantLanguageDetectionJob,

    -- * Request Lenses
    sdldjJobId,

    -- * Destructuring the Response
    stopDominantLanguageDetectionJobResponse,
    StopDominantLanguageDetectionJobResponse,

    -- * Response Lenses
    sdldjdrsJobId,
    sdldjdrsJobStatus,
    sdldjdrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopDominantLanguageDetectionJob' smart constructor.
newtype StopDominantLanguageDetectionJob = StopDominantLanguageDetectionJob'
  { _sdldjJobId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopDominantLanguageDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdldjJobId' - The identifier of the dominant language detection job to stop.
stopDominantLanguageDetectionJob ::
  -- | 'sdldjJobId'
  Text ->
  StopDominantLanguageDetectionJob
stopDominantLanguageDetectionJob pJobId_ =
  StopDominantLanguageDetectionJob' {_sdldjJobId = pJobId_}

-- | The identifier of the dominant language detection job to stop.
sdldjJobId :: Lens' StopDominantLanguageDetectionJob Text
sdldjJobId = lens _sdldjJobId (\s a -> s {_sdldjJobId = a})

instance AWSRequest StopDominantLanguageDetectionJob where
  type
    Rs StopDominantLanguageDetectionJob =
      StopDominantLanguageDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          StopDominantLanguageDetectionJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StopDominantLanguageDetectionJob

instance NFData StopDominantLanguageDetectionJob

instance ToHeaders StopDominantLanguageDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Comprehend_20171127.StopDominantLanguageDetectionJob" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopDominantLanguageDetectionJob where
  toJSON StopDominantLanguageDetectionJob' {..} =
    object (catMaybes [Just ("JobId" .= _sdldjJobId)])

instance ToPath StopDominantLanguageDetectionJob where
  toPath = const "/"

instance ToQuery StopDominantLanguageDetectionJob where
  toQuery = const mempty

-- | /See:/ 'stopDominantLanguageDetectionJobResponse' smart constructor.
data StopDominantLanguageDetectionJobResponse = StopDominantLanguageDetectionJobResponse'
  { _sdldjdrsJobId ::
      !( Maybe
           Text
       ),
    _sdldjdrsJobStatus ::
      !( Maybe
           JobStatus
       ),
    _sdldjdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopDominantLanguageDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdldjdrsJobId' - The identifier of the dominant language detection job to stop.
--
-- * 'sdldjdrsJobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopDominantLanguageDetectionJob@ operation.
--
-- * 'sdldjdrsResponseStatus' - -- | The response status code.
stopDominantLanguageDetectionJobResponse ::
  -- | 'sdldjdrsResponseStatus'
  Int ->
  StopDominantLanguageDetectionJobResponse
stopDominantLanguageDetectionJobResponse pResponseStatus_ =
  StopDominantLanguageDetectionJobResponse'
    { _sdldjdrsJobId =
        Nothing,
      _sdldjdrsJobStatus = Nothing,
      _sdldjdrsResponseStatus = pResponseStatus_
    }

-- | The identifier of the dominant language detection job to stop.
sdldjdrsJobId :: Lens' StopDominantLanguageDetectionJobResponse (Maybe Text)
sdldjdrsJobId = lens _sdldjdrsJobId (\s a -> s {_sdldjdrsJobId = a})

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopDominantLanguageDetectionJob@ operation.
sdldjdrsJobStatus :: Lens' StopDominantLanguageDetectionJobResponse (Maybe JobStatus)
sdldjdrsJobStatus = lens _sdldjdrsJobStatus (\s a -> s {_sdldjdrsJobStatus = a})

-- | -- | The response status code.
sdldjdrsResponseStatus :: Lens' StopDominantLanguageDetectionJobResponse Int
sdldjdrsResponseStatus = lens _sdldjdrsResponseStatus (\s a -> s {_sdldjdrsResponseStatus = a})

instance NFData StopDominantLanguageDetectionJobResponse
