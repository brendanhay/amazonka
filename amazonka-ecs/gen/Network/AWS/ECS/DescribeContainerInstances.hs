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
-- Module      : Network.AWS.ECS.DescribeContainerInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon Elastic Container Service container instances. Returns metadata about registered and remaining resources on each container instance requested.
--
--
module Network.AWS.ECS.DescribeContainerInstances
    (
    -- * Creating a Request
      describeContainerInstances
    , DescribeContainerInstances
    -- * Request Lenses
    , dciCluster
    , dciContainerInstances

    -- * Destructuring the Response
    , describeContainerInstancesResponse
    , DescribeContainerInstancesResponse
    -- * Response Lenses
    , dcisrsFailures
    , dcisrsContainerInstances
    , dcisrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeContainerInstances' smart constructor.
data DescribeContainerInstances = DescribeContainerInstances'
  { _dciCluster            :: !(Maybe Text)
  , _dciContainerInstances :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeContainerInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dciCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to describe. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'dciContainerInstances' - A list of container instance IDs or full ARN entries.
describeContainerInstances
    :: DescribeContainerInstances
describeContainerInstances =
  DescribeContainerInstances'
    {_dciCluster = Nothing, _dciContainerInstances = mempty}


-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to describe. If you do not specify a cluster, the default cluster is assumed.
dciCluster :: Lens' DescribeContainerInstances (Maybe Text)
dciCluster = lens _dciCluster (\ s a -> s{_dciCluster = a})

-- | A list of container instance IDs or full ARN entries.
dciContainerInstances :: Lens' DescribeContainerInstances [Text]
dciContainerInstances = lens _dciContainerInstances (\ s a -> s{_dciContainerInstances = a}) . _Coerce

instance AWSRequest DescribeContainerInstances where
        type Rs DescribeContainerInstances =
             DescribeContainerInstancesResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeContainerInstancesResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "containerInstances" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeContainerInstances where

instance NFData DescribeContainerInstances where

instance ToHeaders DescribeContainerInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DescribeContainerInstances"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeContainerInstances where
        toJSON DescribeContainerInstances'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _dciCluster,
                  Just
                    ("containerInstances" .= _dciContainerInstances)])

instance ToPath DescribeContainerInstances where
        toPath = const "/"

instance ToQuery DescribeContainerInstances where
        toQuery = const mempty

-- | /See:/ 'describeContainerInstancesResponse' smart constructor.
data DescribeContainerInstancesResponse = DescribeContainerInstancesResponse'
  { _dcisrsFailures           :: !(Maybe [Failure])
  , _dcisrsContainerInstances :: !(Maybe [ContainerInstance])
  , _dcisrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeContainerInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcisrsFailures' - Any failures associated with the call.
--
-- * 'dcisrsContainerInstances' - The list of container instances.
--
-- * 'dcisrsResponseStatus' - -- | The response status code.
describeContainerInstancesResponse
    :: Int -- ^ 'dcisrsResponseStatus'
    -> DescribeContainerInstancesResponse
describeContainerInstancesResponse pResponseStatus_ =
  DescribeContainerInstancesResponse'
    { _dcisrsFailures = Nothing
    , _dcisrsContainerInstances = Nothing
    , _dcisrsResponseStatus = pResponseStatus_
    }


-- | Any failures associated with the call.
dcisrsFailures :: Lens' DescribeContainerInstancesResponse [Failure]
dcisrsFailures = lens _dcisrsFailures (\ s a -> s{_dcisrsFailures = a}) . _Default . _Coerce

-- | The list of container instances.
dcisrsContainerInstances :: Lens' DescribeContainerInstancesResponse [ContainerInstance]
dcisrsContainerInstances = lens _dcisrsContainerInstances (\ s a -> s{_dcisrsContainerInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
dcisrsResponseStatus :: Lens' DescribeContainerInstancesResponse Int
dcisrsResponseStatus = lens _dcisrsResponseStatus (\ s a -> s{_dcisrsResponseStatus = a})

instance NFData DescribeContainerInstancesResponse
         where
