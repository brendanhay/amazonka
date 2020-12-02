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
-- Module      : Network.AWS.Comprehend.StopPiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a PII entities detection job in progress.
module Network.AWS.Comprehend.StopPiiEntitiesDetectionJob
  ( -- * Creating a Request
    stopPiiEntitiesDetectionJob,
    StopPiiEntitiesDetectionJob,

    -- * Request Lenses
    spedjJobId,

    -- * Destructuring the Response
    stopPiiEntitiesDetectionJobResponse,
    StopPiiEntitiesDetectionJobResponse,

    -- * Response Lenses
    spedjprsJobId,
    spedjprsJobStatus,
    spedjprsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopPiiEntitiesDetectionJob' smart constructor.
newtype StopPiiEntitiesDetectionJob = StopPiiEntitiesDetectionJob'
  { _spedjJobId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopPiiEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spedjJobId' - The identifier of the PII entities detection job to stop.
stopPiiEntitiesDetectionJob ::
  -- | 'spedjJobId'
  Text ->
  StopPiiEntitiesDetectionJob
stopPiiEntitiesDetectionJob pJobId_ =
  StopPiiEntitiesDetectionJob' {_spedjJobId = pJobId_}

-- | The identifier of the PII entities detection job to stop.
spedjJobId :: Lens' StopPiiEntitiesDetectionJob Text
spedjJobId = lens _spedjJobId (\s a -> s {_spedjJobId = a})

instance AWSRequest StopPiiEntitiesDetectionJob where
  type
    Rs StopPiiEntitiesDetectionJob =
      StopPiiEntitiesDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          StopPiiEntitiesDetectionJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StopPiiEntitiesDetectionJob

instance NFData StopPiiEntitiesDetectionJob

instance ToHeaders StopPiiEntitiesDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.StopPiiEntitiesDetectionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopPiiEntitiesDetectionJob where
  toJSON StopPiiEntitiesDetectionJob' {..} =
    object (catMaybes [Just ("JobId" .= _spedjJobId)])

instance ToPath StopPiiEntitiesDetectionJob where
  toPath = const "/"

instance ToQuery StopPiiEntitiesDetectionJob where
  toQuery = const mempty

-- | /See:/ 'stopPiiEntitiesDetectionJobResponse' smart constructor.
data StopPiiEntitiesDetectionJobResponse = StopPiiEntitiesDetectionJobResponse'
  { _spedjprsJobId ::
      !(Maybe Text),
    _spedjprsJobStatus ::
      !(Maybe JobStatus),
    _spedjprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopPiiEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spedjprsJobId' - The identifier of the PII entities detection job to stop.
--
-- * 'spedjprsJobStatus' - The status of the PII entities detection job.
--
-- * 'spedjprsResponseStatus' - -- | The response status code.
stopPiiEntitiesDetectionJobResponse ::
  -- | 'spedjprsResponseStatus'
  Int ->
  StopPiiEntitiesDetectionJobResponse
stopPiiEntitiesDetectionJobResponse pResponseStatus_ =
  StopPiiEntitiesDetectionJobResponse'
    { _spedjprsJobId = Nothing,
      _spedjprsJobStatus = Nothing,
      _spedjprsResponseStatus = pResponseStatus_
    }

-- | The identifier of the PII entities detection job to stop.
spedjprsJobId :: Lens' StopPiiEntitiesDetectionJobResponse (Maybe Text)
spedjprsJobId = lens _spedjprsJobId (\s a -> s {_spedjprsJobId = a})

-- | The status of the PII entities detection job.
spedjprsJobStatus :: Lens' StopPiiEntitiesDetectionJobResponse (Maybe JobStatus)
spedjprsJobStatus = lens _spedjprsJobStatus (\s a -> s {_spedjprsJobStatus = a})

-- | -- | The response status code.
spedjprsResponseStatus :: Lens' StopPiiEntitiesDetectionJobResponse Int
spedjprsResponseStatus = lens _spedjprsResponseStatus (\s a -> s {_spedjprsResponseStatus = a})

instance NFData StopPiiEntitiesDetectionJobResponse
