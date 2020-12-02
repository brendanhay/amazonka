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
-- Module      : Network.AWS.Comprehend.DescribeEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an entities detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeEntitiesDetectionJob
  ( -- * Creating a Request
    describeEntitiesDetectionJob,
    DescribeEntitiesDetectionJob,

    -- * Request Lenses
    dJobId,

    -- * Destructuring the Response
    describeEntitiesDetectionJobResponse,
    DescribeEntitiesDetectionJobResponse,

    -- * Response Lenses
    desrsEntitiesDetectionJobProperties,
    desrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEntitiesDetectionJob' smart constructor.
newtype DescribeEntitiesDetectionJob = DescribeEntitiesDetectionJob'
  { _dJobId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dJobId' - The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
describeEntitiesDetectionJob ::
  -- | 'dJobId'
  Text ->
  DescribeEntitiesDetectionJob
describeEntitiesDetectionJob pJobId_ =
  DescribeEntitiesDetectionJob' {_dJobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
dJobId :: Lens' DescribeEntitiesDetectionJob Text
dJobId = lens _dJobId (\s a -> s {_dJobId = a})

instance AWSRequest DescribeEntitiesDetectionJob where
  type
    Rs DescribeEntitiesDetectionJob =
      DescribeEntitiesDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          DescribeEntitiesDetectionJobResponse'
            <$> (x .?> "EntitiesDetectionJobProperties") <*> (pure (fromEnum s))
      )

instance Hashable DescribeEntitiesDetectionJob

instance NFData DescribeEntitiesDetectionJob

instance ToHeaders DescribeEntitiesDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DescribeEntitiesDetectionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeEntitiesDetectionJob where
  toJSON DescribeEntitiesDetectionJob' {..} =
    object (catMaybes [Just ("JobId" .= _dJobId)])

instance ToPath DescribeEntitiesDetectionJob where
  toPath = const "/"

instance ToQuery DescribeEntitiesDetectionJob where
  toQuery = const mempty

-- | /See:/ 'describeEntitiesDetectionJobResponse' smart constructor.
data DescribeEntitiesDetectionJobResponse = DescribeEntitiesDetectionJobResponse'
  { _desrsEntitiesDetectionJobProperties ::
      !( Maybe
           EntitiesDetectionJobProperties
       ),
    _desrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsEntitiesDetectionJobProperties' - An object that contains the properties associated with an entities detection job.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeEntitiesDetectionJobResponse ::
  -- | 'desrsResponseStatus'
  Int ->
  DescribeEntitiesDetectionJobResponse
describeEntitiesDetectionJobResponse pResponseStatus_ =
  DescribeEntitiesDetectionJobResponse'
    { _desrsEntitiesDetectionJobProperties =
        Nothing,
      _desrsResponseStatus = pResponseStatus_
    }

-- | An object that contains the properties associated with an entities detection job.
desrsEntitiesDetectionJobProperties :: Lens' DescribeEntitiesDetectionJobResponse (Maybe EntitiesDetectionJobProperties)
desrsEntitiesDetectionJobProperties = lens _desrsEntitiesDetectionJobProperties (\s a -> s {_desrsEntitiesDetectionJobProperties = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeEntitiesDetectionJobResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\s a -> s {_desrsResponseStatus = a})

instance NFData DescribeEntitiesDetectionJobResponse
