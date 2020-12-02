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
-- Module      : Network.AWS.Comprehend.DescribeEventsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status and details of an events detection job.
module Network.AWS.Comprehend.DescribeEventsDetectionJob
  ( -- * Creating a Request
    describeEventsDetectionJob,
    DescribeEventsDetectionJob,

    -- * Request Lenses
    dedjJobId,

    -- * Destructuring the Response
    describeEventsDetectionJobResponse,
    DescribeEventsDetectionJobResponse,

    -- * Response Lenses
    dedjrsEventsDetectionJobProperties,
    dedjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventsDetectionJob' smart constructor.
newtype DescribeEventsDetectionJob = DescribeEventsDetectionJob'
  { _dedjJobId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEventsDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedjJobId' - The identifier of the events detection job.
describeEventsDetectionJob ::
  -- | 'dedjJobId'
  Text ->
  DescribeEventsDetectionJob
describeEventsDetectionJob pJobId_ =
  DescribeEventsDetectionJob' {_dedjJobId = pJobId_}

-- | The identifier of the events detection job.
dedjJobId :: Lens' DescribeEventsDetectionJob Text
dedjJobId = lens _dedjJobId (\s a -> s {_dedjJobId = a})

instance AWSRequest DescribeEventsDetectionJob where
  type
    Rs DescribeEventsDetectionJob =
      DescribeEventsDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          DescribeEventsDetectionJobResponse'
            <$> (x .?> "EventsDetectionJobProperties") <*> (pure (fromEnum s))
      )

instance Hashable DescribeEventsDetectionJob

instance NFData DescribeEventsDetectionJob

instance ToHeaders DescribeEventsDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DescribeEventsDetectionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeEventsDetectionJob where
  toJSON DescribeEventsDetectionJob' {..} =
    object (catMaybes [Just ("JobId" .= _dedjJobId)])

instance ToPath DescribeEventsDetectionJob where
  toPath = const "/"

instance ToQuery DescribeEventsDetectionJob where
  toQuery = const mempty

-- | /See:/ 'describeEventsDetectionJobResponse' smart constructor.
data DescribeEventsDetectionJobResponse = DescribeEventsDetectionJobResponse'
  { _dedjrsEventsDetectionJobProperties ::
      !( Maybe
           EventsDetectionJobProperties
       ),
    _dedjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEventsDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedjrsEventsDetectionJobProperties' - An object that contains the properties associated with an event detection job.
--
-- * 'dedjrsResponseStatus' - -- | The response status code.
describeEventsDetectionJobResponse ::
  -- | 'dedjrsResponseStatus'
  Int ->
  DescribeEventsDetectionJobResponse
describeEventsDetectionJobResponse pResponseStatus_ =
  DescribeEventsDetectionJobResponse'
    { _dedjrsEventsDetectionJobProperties =
        Nothing,
      _dedjrsResponseStatus = pResponseStatus_
    }

-- | An object that contains the properties associated with an event detection job.
dedjrsEventsDetectionJobProperties :: Lens' DescribeEventsDetectionJobResponse (Maybe EventsDetectionJobProperties)
dedjrsEventsDetectionJobProperties = lens _dedjrsEventsDetectionJobProperties (\s a -> s {_dedjrsEventsDetectionJobProperties = a})

-- | -- | The response status code.
dedjrsResponseStatus :: Lens' DescribeEventsDetectionJobResponse Int
dedjrsResponseStatus = lens _dedjrsResponseStatus (\s a -> s {_dedjrsResponseStatus = a})

instance NFData DescribeEventsDetectionJobResponse
