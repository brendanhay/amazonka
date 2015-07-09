{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeContainerInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Describes Amazon EC2 Container Service container instances. Returns
-- metadata about registered and remaining resources on each container
-- instance requested.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeContainerInstances.html>
module Network.AWS.ECS.DescribeContainerInstances
    (
    -- * Request
      DescribeContainerInstances
    -- ** Request constructor
    , describeContainerInstances
    -- ** Request lenses
    , dciCluster
    , dciContainerInstances

    -- * Response
    , DescribeContainerInstancesResponse
    -- ** Response constructor
    , describeContainerInstancesResponse
    -- ** Response lenses
    , desFailures
    , desContainerInstances
    , desStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeContainerInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dciCluster'
--
-- * 'dciContainerInstances'
data DescribeContainerInstances = DescribeContainerInstances'
    { _dciCluster            :: !(Maybe Text)
    , _dciContainerInstances :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeContainerInstances' smart constructor.
describeContainerInstances :: DescribeContainerInstances
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
dciContainerInstances = lens _dciContainerInstances (\ s a -> s{_dciContainerInstances = a});

instance AWSRequest DescribeContainerInstances where
        type Sv DescribeContainerInstances = ECS
        type Rs DescribeContainerInstances =
             DescribeContainerInstancesResponse
        request = postJSON
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
              ["cluster" .= _dciCluster,
               "containerInstances" .= _dciContainerInstances]

instance ToPath DescribeContainerInstances where
        toPath = const "/"

instance ToQuery DescribeContainerInstances where
        toQuery = const mempty

-- | /See:/ 'describeContainerInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desFailures'
--
-- * 'desContainerInstances'
--
-- * 'desStatus'
data DescribeContainerInstancesResponse = DescribeContainerInstancesResponse'
    { _desFailures           :: !(Maybe [Failure])
    , _desContainerInstances :: !(Maybe [ContainerInstance])
    , _desStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeContainerInstancesResponse' smart constructor.
describeContainerInstancesResponse :: Int -> DescribeContainerInstancesResponse
describeContainerInstancesResponse pStatus =
    DescribeContainerInstancesResponse'
    { _desFailures = Nothing
    , _desContainerInstances = Nothing
    , _desStatus = pStatus
    }

-- | FIXME: Undocumented member.
desFailures :: Lens' DescribeContainerInstancesResponse [Failure]
desFailures = lens _desFailures (\ s a -> s{_desFailures = a}) . _Default;

-- | The list of container instances.
desContainerInstances :: Lens' DescribeContainerInstancesResponse [ContainerInstance]
desContainerInstances = lens _desContainerInstances (\ s a -> s{_desContainerInstances = a}) . _Default;

-- | FIXME: Undocumented member.
desStatus :: Lens' DescribeContainerInstancesResponse Int
desStatus = lens _desStatus (\ s a -> s{_desStatus = a});
