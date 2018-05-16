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
-- Module      : Network.AWS.Comprehend.DescribeTopicsDetectionJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a topic detection job. Use this operation to get the status of a detection job.
--
--
module Network.AWS.Comprehend.DescribeTopicsDetectionJob
    (
    -- * Creating a Request
      describeTopicsDetectionJob
    , DescribeTopicsDetectionJob
    -- * Request Lenses
    , dtdjJobId

    -- * Destructuring the Response
    , describeTopicsDetectionJobResponse
    , DescribeTopicsDetectionJobResponse
    -- * Response Lenses
    , dtdjrsTopicsDetectionJobProperties
    , dtdjrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTopicsDetectionJob' smart constructor.
newtype DescribeTopicsDetectionJob = DescribeTopicsDetectionJob'
  { _dtdjJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTopicsDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtdjJobId' - The identifier assigned by the user to the detection job.
describeTopicsDetectionJob
    :: Text -- ^ 'dtdjJobId'
    -> DescribeTopicsDetectionJob
describeTopicsDetectionJob pJobId_ =
  DescribeTopicsDetectionJob' {_dtdjJobId = pJobId_}


-- | The identifier assigned by the user to the detection job.
dtdjJobId :: Lens' DescribeTopicsDetectionJob Text
dtdjJobId = lens _dtdjJobId (\ s a -> s{_dtdjJobId = a})

instance AWSRequest DescribeTopicsDetectionJob where
        type Rs DescribeTopicsDetectionJob =
             DescribeTopicsDetectionJobResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTopicsDetectionJobResponse' <$>
                   (x .?> "TopicsDetectionJobProperties") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeTopicsDetectionJob where

instance NFData DescribeTopicsDetectionJob where

instance ToHeaders DescribeTopicsDetectionJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.DescribeTopicsDetectionJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTopicsDetectionJob where
        toJSON DescribeTopicsDetectionJob'{..}
          = object (catMaybes [Just ("JobId" .= _dtdjJobId)])

instance ToPath DescribeTopicsDetectionJob where
        toPath = const "/"

instance ToQuery DescribeTopicsDetectionJob where
        toQuery = const mempty

-- | /See:/ 'describeTopicsDetectionJobResponse' smart constructor.
data DescribeTopicsDetectionJobResponse = DescribeTopicsDetectionJobResponse'
  { _dtdjrsTopicsDetectionJobProperties :: !(Maybe TopicsDetectionJobProperties)
  , _dtdjrsResponseStatus               :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTopicsDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtdjrsTopicsDetectionJobProperties' - The list of properties for the requested job.
--
-- * 'dtdjrsResponseStatus' - -- | The response status code.
describeTopicsDetectionJobResponse
    :: Int -- ^ 'dtdjrsResponseStatus'
    -> DescribeTopicsDetectionJobResponse
describeTopicsDetectionJobResponse pResponseStatus_ =
  DescribeTopicsDetectionJobResponse'
    { _dtdjrsTopicsDetectionJobProperties = Nothing
    , _dtdjrsResponseStatus = pResponseStatus_
    }


-- | The list of properties for the requested job.
dtdjrsTopicsDetectionJobProperties :: Lens' DescribeTopicsDetectionJobResponse (Maybe TopicsDetectionJobProperties)
dtdjrsTopicsDetectionJobProperties = lens _dtdjrsTopicsDetectionJobProperties (\ s a -> s{_dtdjrsTopicsDetectionJobProperties = a})

-- | -- | The response status code.
dtdjrsResponseStatus :: Lens' DescribeTopicsDetectionJobResponse Int
dtdjrsResponseStatus = lens _dtdjrsResponseStatus (\ s a -> s{_dtdjrsResponseStatus = a})

instance NFData DescribeTopicsDetectionJobResponse
         where
