{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeServices
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified services running in your cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeServices.html>
module Network.AWS.ECS.DescribeServices
    (
    -- * Request
      DescribeServices
    -- ** Request constructor
    , describeServices
    -- ** Request lenses
    , drqCluster
    , drqServices

    -- * Response
    , DescribeServicesResponse
    -- ** Response constructor
    , describeServicesResponse
    -- ** Response lenses
    , dssrsFailures
    , dssrsServices
    , dssrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeServices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqCluster'
--
-- * 'drqServices'
data DescribeServices = DescribeServices'
    { _drqCluster  :: !(Maybe Text)
    , _drqServices :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeServices' smart constructor.
describeServices :: DescribeServices
describeServices =
    DescribeServices'
    { _drqCluster = Nothing
    , _drqServices = mempty
    }

-- | The name of the cluster that hosts the service you want to describe.
drqCluster :: Lens' DescribeServices (Maybe Text)
drqCluster = lens _drqCluster (\ s a -> s{_drqCluster = a});

-- | A list of services you want to describe.
drqServices :: Lens' DescribeServices [Text]
drqServices = lens _drqServices (\ s a -> s{_drqServices = a});

instance AWSRequest DescribeServices where
        type Sv DescribeServices = ECS
        type Rs DescribeServices = DescribeServicesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeServicesResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "services" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeServices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DescribeServices"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeServices where
        toJSON DescribeServices'{..}
          = object
              ["cluster" .= _drqCluster,
               "services" .= _drqServices]

instance ToPath DescribeServices where
        toPath = const "/"

instance ToQuery DescribeServices where
        toQuery = const mempty

-- | /See:/ 'describeServicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssrsFailures'
--
-- * 'dssrsServices'
--
-- * 'dssrsStatus'
data DescribeServicesResponse = DescribeServicesResponse'
    { _dssrsFailures :: !(Maybe [Failure])
    , _dssrsServices :: !(Maybe [ContainerService])
    , _dssrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeServicesResponse' smart constructor.
describeServicesResponse :: Int -> DescribeServicesResponse
describeServicesResponse pStatus =
    DescribeServicesResponse'
    { _dssrsFailures = Nothing
    , _dssrsServices = Nothing
    , _dssrsStatus = pStatus
    }

-- | Any failures associated with the call.
dssrsFailures :: Lens' DescribeServicesResponse [Failure]
dssrsFailures = lens _dssrsFailures (\ s a -> s{_dssrsFailures = a}) . _Default;

-- | The list of services described.
dssrsServices :: Lens' DescribeServicesResponse [ContainerService]
dssrsServices = lens _dssrsServices (\ s a -> s{_dssrsServices = a}) . _Default;

-- | FIXME: Undocumented member.
dssrsStatus :: Lens' DescribeServicesResponse Int
dssrsStatus = lens _dssrsStatus (\ s a -> s{_dssrsStatus = a});
