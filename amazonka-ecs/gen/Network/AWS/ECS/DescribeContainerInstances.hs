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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon EC2 Container Service container instances. Returns
-- metadata about registered and remaining resources on each container
-- instance requested.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeContainerInstances.html AWS API Reference> for DescribeContainerInstances.
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
    , dcisrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeContainerInstances' smart constructor.
data DescribeContainerInstances = DescribeContainerInstances'
    { _dciCluster            :: !(Maybe Text)
    , _dciContainerInstances :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeContainerInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dciCluster'
--
-- * 'dciContainerInstances'
describeContainerInstances
    :: DescribeContainerInstances
describeContainerInstances =
    DescribeContainerInstances'
    { _dciCluster = Nothing
    , _dciContainerInstances = mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances you want to describe. If you do not
-- specify a cluster, the default cluster is assumed.
dciCluster :: Lens' DescribeContainerInstances (Maybe Text)
dciCluster = lens _dciCluster (\ s a -> s{_dciCluster = a});

-- | A space-separated list of container instance UUIDs or full Amazon
-- Resource Name (ARN) entries.
dciContainerInstances :: Lens' DescribeContainerInstances [Text]
dciContainerInstances = lens _dciContainerInstances (\ s a -> s{_dciContainerInstances = a}) . _Coerce;

instance AWSRequest DescribeContainerInstances where
        type Rs DescribeContainerInstances =
             DescribeContainerInstancesResponse
        request = postJSON eCS
        response
          = receiveJSON
              (\ s h x ->
                 DescribeContainerInstancesResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "containerInstances" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
    , _dcisrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeContainerInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcisrsFailures'
--
-- * 'dcisrsContainerInstances'
--
-- * 'dcisrsStatus'
describeContainerInstancesResponse
    :: Int -- ^ 'dcisrsStatus'
    -> DescribeContainerInstancesResponse
describeContainerInstancesResponse pStatus_ =
    DescribeContainerInstancesResponse'
    { _dcisrsFailures = Nothing
    , _dcisrsContainerInstances = Nothing
    , _dcisrsStatus = pStatus_
    }

-- | Undocumented member.
dcisrsFailures :: Lens' DescribeContainerInstancesResponse [Failure]
dcisrsFailures = lens _dcisrsFailures (\ s a -> s{_dcisrsFailures = a}) . _Default . _Coerce;

-- | The list of container instances.
dcisrsContainerInstances :: Lens' DescribeContainerInstancesResponse [ContainerInstance]
dcisrsContainerInstances = lens _dcisrsContainerInstances (\ s a -> s{_dcisrsContainerInstances = a}) . _Default . _Coerce;

-- | The response status code.
dcisrsStatus :: Lens' DescribeContainerInstancesResponse Int
dcisrsStatus = lens _dcisrsStatus (\ s a -> s{_dcisrsStatus = a});
