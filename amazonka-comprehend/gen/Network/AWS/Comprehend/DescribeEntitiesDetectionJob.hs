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
-- Module      : Network.AWS.Comprehend.DescribeEntitiesDetectionJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an entities detection job. Use this operation to get the status of a detection job.
--
--
module Network.AWS.Comprehend.DescribeEntitiesDetectionJob
    (
    -- * Creating a Request
      describeEntitiesDetectionJob
    , DescribeEntitiesDetectionJob
    -- * Request Lenses
    , dedjJobId

    -- * Destructuring the Response
    , describeEntitiesDetectionJobResponse
    , DescribeEntitiesDetectionJobResponse
    -- * Response Lenses
    , dedjrsEntitiesDetectionJobProperties
    , dedjrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEntitiesDetectionJob' smart constructor.
newtype DescribeEntitiesDetectionJob = DescribeEntitiesDetectionJob'
  { _dedjJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedjJobId' - The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
describeEntitiesDetectionJob
    :: Text -- ^ 'dedjJobId'
    -> DescribeEntitiesDetectionJob
describeEntitiesDetectionJob pJobId_ =
  DescribeEntitiesDetectionJob' {_dedjJobId = pJobId_}


-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
dedjJobId :: Lens' DescribeEntitiesDetectionJob Text
dedjJobId = lens _dedjJobId (\ s a -> s{_dedjJobId = a})

instance AWSRequest DescribeEntitiesDetectionJob
         where
        type Rs DescribeEntitiesDetectionJob =
             DescribeEntitiesDetectionJobResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEntitiesDetectionJobResponse' <$>
                   (x .?> "EntitiesDetectionJobProperties") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeEntitiesDetectionJob where

instance NFData DescribeEntitiesDetectionJob where

instance ToHeaders DescribeEntitiesDetectionJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.DescribeEntitiesDetectionJob"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEntitiesDetectionJob where
        toJSON DescribeEntitiesDetectionJob'{..}
          = object (catMaybes [Just ("JobId" .= _dedjJobId)])

instance ToPath DescribeEntitiesDetectionJob where
        toPath = const "/"

instance ToQuery DescribeEntitiesDetectionJob where
        toQuery = const mempty

-- | /See:/ 'describeEntitiesDetectionJobResponse' smart constructor.
data DescribeEntitiesDetectionJobResponse = DescribeEntitiesDetectionJobResponse'
  { _dedjrsEntitiesDetectionJobProperties :: !(Maybe EntitiesDetectionJobProperties)
  , _dedjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedjrsEntitiesDetectionJobProperties' - An object that contains the properties associated with an entities detection job.
--
-- * 'dedjrsResponseStatus' - -- | The response status code.
describeEntitiesDetectionJobResponse
    :: Int -- ^ 'dedjrsResponseStatus'
    -> DescribeEntitiesDetectionJobResponse
describeEntitiesDetectionJobResponse pResponseStatus_ =
  DescribeEntitiesDetectionJobResponse'
    { _dedjrsEntitiesDetectionJobProperties = Nothing
    , _dedjrsResponseStatus = pResponseStatus_
    }


-- | An object that contains the properties associated with an entities detection job.
dedjrsEntitiesDetectionJobProperties :: Lens' DescribeEntitiesDetectionJobResponse (Maybe EntitiesDetectionJobProperties)
dedjrsEntitiesDetectionJobProperties = lens _dedjrsEntitiesDetectionJobProperties (\ s a -> s{_dedjrsEntitiesDetectionJobProperties = a})

-- | -- | The response status code.
dedjrsResponseStatus :: Lens' DescribeEntitiesDetectionJobResponse Int
dedjrsResponseStatus = lens _dedjrsResponseStatus (\ s a -> s{_dedjrsResponseStatus = a})

instance NFData DescribeEntitiesDetectionJobResponse
         where
