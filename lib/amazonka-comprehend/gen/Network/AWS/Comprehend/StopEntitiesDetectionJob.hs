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
-- Module      : Network.AWS.Comprehend.StopEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an entities detection job in progress.
--
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is stopped and put into the @STOPPED@ state.
--
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception.
--
-- When a job is stopped, any documents already processed are written to the output location.
module Network.AWS.Comprehend.StopEntitiesDetectionJob
  ( -- * Creating a Request
    stopEntitiesDetectionJob,
    StopEntitiesDetectionJob,

    -- * Request Lenses
    sJobId,

    -- * Destructuring the Response
    stopEntitiesDetectionJobResponse,
    StopEntitiesDetectionJobResponse,

    -- * Response Lenses
    srsJobId,
    srsJobStatus,
    srsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopEntitiesDetectionJob' smart constructor.
newtype StopEntitiesDetectionJob = StopEntitiesDetectionJob'
  { _sJobId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sJobId' - The identifier of the entities detection job to stop.
stopEntitiesDetectionJob ::
  -- | 'sJobId'
  Text ->
  StopEntitiesDetectionJob
stopEntitiesDetectionJob pJobId_ =
  StopEntitiesDetectionJob' {_sJobId = pJobId_}

-- | The identifier of the entities detection job to stop.
sJobId :: Lens' StopEntitiesDetectionJob Text
sJobId = lens _sJobId (\s a -> s {_sJobId = a})

instance AWSRequest StopEntitiesDetectionJob where
  type Rs StopEntitiesDetectionJob = StopEntitiesDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          StopEntitiesDetectionJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StopEntitiesDetectionJob

instance NFData StopEntitiesDetectionJob

instance ToHeaders StopEntitiesDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.StopEntitiesDetectionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopEntitiesDetectionJob where
  toJSON StopEntitiesDetectionJob' {..} =
    object (catMaybes [Just ("JobId" .= _sJobId)])

instance ToPath StopEntitiesDetectionJob where
  toPath = const "/"

instance ToQuery StopEntitiesDetectionJob where
  toQuery = const mempty

-- | /See:/ 'stopEntitiesDetectionJobResponse' smart constructor.
data StopEntitiesDetectionJobResponse = StopEntitiesDetectionJobResponse'
  { _srsJobId ::
      !(Maybe Text),
    _srsJobStatus ::
      !(Maybe JobStatus),
    _srsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsJobId' - The identifier of the entities detection job to stop.
--
-- * 'srsJobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopEntitiesDetectionJob@ operation.
--
-- * 'srsResponseStatus' - -- | The response status code.
stopEntitiesDetectionJobResponse ::
  -- | 'srsResponseStatus'
  Int ->
  StopEntitiesDetectionJobResponse
stopEntitiesDetectionJobResponse pResponseStatus_ =
  StopEntitiesDetectionJobResponse'
    { _srsJobId = Nothing,
      _srsJobStatus = Nothing,
      _srsResponseStatus = pResponseStatus_
    }

-- | The identifier of the entities detection job to stop.
srsJobId :: Lens' StopEntitiesDetectionJobResponse (Maybe Text)
srsJobId = lens _srsJobId (\s a -> s {_srsJobId = a})

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopEntitiesDetectionJob@ operation.
srsJobStatus :: Lens' StopEntitiesDetectionJobResponse (Maybe JobStatus)
srsJobStatus = lens _srsJobStatus (\s a -> s {_srsJobStatus = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StopEntitiesDetectionJobResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

instance NFData StopEntitiesDetectionJobResponse
