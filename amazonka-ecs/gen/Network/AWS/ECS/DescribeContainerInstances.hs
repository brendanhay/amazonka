{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeContainerInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon EC2 Container Service container instances. Returns
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
    , dcirqCluster
    , dcirqContainerInstances

    -- * Response
    , DescribeContainerInstancesResponse
    -- ** Response constructor
    , describeContainerInstancesResponse
    -- ** Response lenses
    , dcirsFailures
    , dcirsContainerInstances
    , dcirsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeContainerInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcirqCluster'
--
-- * 'dcirqContainerInstances'
data DescribeContainerInstances = DescribeContainerInstances'
    { _dcirqCluster            :: !(Maybe Text)
    , _dcirqContainerInstances :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeContainerInstances' smart constructor.
describeContainerInstances :: DescribeContainerInstances
describeContainerInstances =
    DescribeContainerInstances'
    { _dcirqCluster = Nothing
    , _dcirqContainerInstances = mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances you want to describe. If you do not
-- specify a cluster, the default cluster is assumed.
dcirqCluster :: Lens' DescribeContainerInstances (Maybe Text)
dcirqCluster = lens _dcirqCluster (\ s a -> s{_dcirqCluster = a});

-- | A space-separated list of container instance UUIDs or full Amazon
-- Resource Name (ARN) entries.
dcirqContainerInstances :: Lens' DescribeContainerInstances [Text]
dcirqContainerInstances = lens _dcirqContainerInstances (\ s a -> s{_dcirqContainerInstances = a});

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
              ["cluster" .= _dcirqCluster,
               "containerInstances" .= _dcirqContainerInstances]

instance ToPath DescribeContainerInstances where
        toPath = const "/"

instance ToQuery DescribeContainerInstances where
        toQuery = const mempty

-- | /See:/ 'describeContainerInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcirsFailures'
--
-- * 'dcirsContainerInstances'
--
-- * 'dcirsStatus'
data DescribeContainerInstancesResponse = DescribeContainerInstancesResponse'
    { _dcirsFailures           :: !(Maybe [Failure])
    , _dcirsContainerInstances :: !(Maybe [ContainerInstance])
    , _dcirsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeContainerInstancesResponse' smart constructor.
describeContainerInstancesResponse :: Int -> DescribeContainerInstancesResponse
describeContainerInstancesResponse pStatus =
    DescribeContainerInstancesResponse'
    { _dcirsFailures = Nothing
    , _dcirsContainerInstances = Nothing
    , _dcirsStatus = pStatus
    }

-- | FIXME: Undocumented member.
dcirsFailures :: Lens' DescribeContainerInstancesResponse [Failure]
dcirsFailures = lens _dcirsFailures (\ s a -> s{_dcirsFailures = a}) . _Default;

-- | The list of container instances.
dcirsContainerInstances :: Lens' DescribeContainerInstancesResponse [ContainerInstance]
dcirsContainerInstances = lens _dcirsContainerInstances (\ s a -> s{_dcirsContainerInstances = a}) . _Default;

-- | FIXME: Undocumented member.
dcirsStatus :: Lens' DescribeContainerInstancesResponse Int
dcirsStatus = lens _dcirsStatus (\ s a -> s{_dcirsStatus = a});
