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
-- Module      : Network.AWS.Comprehend.StopSentimentDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a sentiment detection job in progress.
--
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is be stopped and put into the @STOPPED@ state.
--
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception.
--
-- When a job is stopped, any documents already processed are written to the output location.
module Network.AWS.Comprehend.StopSentimentDetectionJob
  ( -- * Creating a Request
    stopSentimentDetectionJob,
    StopSentimentDetectionJob,

    -- * Request Lenses
    ssdjJobId,

    -- * Destructuring the Response
    stopSentimentDetectionJobResponse,
    StopSentimentDetectionJobResponse,

    -- * Response Lenses
    storsJobId,
    storsJobStatus,
    storsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopSentimentDetectionJob' smart constructor.
newtype StopSentimentDetectionJob = StopSentimentDetectionJob'
  { _ssdjJobId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopSentimentDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdjJobId' - The identifier of the sentiment detection job to stop.
stopSentimentDetectionJob ::
  -- | 'ssdjJobId'
  Text ->
  StopSentimentDetectionJob
stopSentimentDetectionJob pJobId_ =
  StopSentimentDetectionJob' {_ssdjJobId = pJobId_}

-- | The identifier of the sentiment detection job to stop.
ssdjJobId :: Lens' StopSentimentDetectionJob Text
ssdjJobId = lens _ssdjJobId (\s a -> s {_ssdjJobId = a})

instance AWSRequest StopSentimentDetectionJob where
  type
    Rs StopSentimentDetectionJob =
      StopSentimentDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          StopSentimentDetectionJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StopSentimentDetectionJob

instance NFData StopSentimentDetectionJob

instance ToHeaders StopSentimentDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.StopSentimentDetectionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopSentimentDetectionJob where
  toJSON StopSentimentDetectionJob' {..} =
    object (catMaybes [Just ("JobId" .= _ssdjJobId)])

instance ToPath StopSentimentDetectionJob where
  toPath = const "/"

instance ToQuery StopSentimentDetectionJob where
  toQuery = const mempty

-- | /See:/ 'stopSentimentDetectionJobResponse' smart constructor.
data StopSentimentDetectionJobResponse = StopSentimentDetectionJobResponse'
  { _storsJobId ::
      !(Maybe Text),
    _storsJobStatus ::
      !(Maybe JobStatus),
    _storsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopSentimentDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'storsJobId' - The identifier of the sentiment detection job to stop.
--
-- * 'storsJobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopSentimentDetectionJob@ operation.
--
-- * 'storsResponseStatus' - -- | The response status code.
stopSentimentDetectionJobResponse ::
  -- | 'storsResponseStatus'
  Int ->
  StopSentimentDetectionJobResponse
stopSentimentDetectionJobResponse pResponseStatus_ =
  StopSentimentDetectionJobResponse'
    { _storsJobId = Nothing,
      _storsJobStatus = Nothing,
      _storsResponseStatus = pResponseStatus_
    }

-- | The identifier of the sentiment detection job to stop.
storsJobId :: Lens' StopSentimentDetectionJobResponse (Maybe Text)
storsJobId = lens _storsJobId (\s a -> s {_storsJobId = a})

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopSentimentDetectionJob@ operation.
storsJobStatus :: Lens' StopSentimentDetectionJobResponse (Maybe JobStatus)
storsJobStatus = lens _storsJobStatus (\s a -> s {_storsJobStatus = a})

-- | -- | The response status code.
storsResponseStatus :: Lens' StopSentimentDetectionJobResponse Int
storsResponseStatus = lens _storsResponseStatus (\s a -> s {_storsResponseStatus = a})

instance NFData StopSentimentDetectionJobResponse
