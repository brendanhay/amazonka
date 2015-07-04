{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.ECS.DescribeServices
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the specified services running in your cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeServices.html>
module Network.AWS.ECS.DescribeServices
    (
    -- * Request
      DescribeServices
    -- ** Request constructor
    , describeServices
    -- ** Request lenses
    , desCluster
    , desServices

    -- * Response
    , DescribeServicesResponse
    -- ** Response constructor
    , describeServicesResponse
    -- ** Response lenses
    , dFailures
    , dServices
    , dStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeServices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desCluster'
--
-- * 'desServices'
data DescribeServices = DescribeServices'
    { _desCluster  :: !(Maybe Text)
    , _desServices :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeServices' smart constructor.
describeServices :: DescribeServices
describeServices =
    DescribeServices'
    { _desCluster = Nothing
    , _desServices = mempty
    }

-- | The name of the cluster that hosts the service you want to describe.
desCluster :: Lens' DescribeServices (Maybe Text)
desCluster = lens _desCluster (\ s a -> s{_desCluster = a});

-- | A list of services you want to describe.
desServices :: Lens' DescribeServices [Text]
desServices = lens _desServices (\ s a -> s{_desServices = a});

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
              ["cluster" .= _desCluster,
               "services" .= _desServices]

instance ToPath DescribeServices where
        toPath = const "/"

instance ToQuery DescribeServices where
        toQuery = const mempty

-- | /See:/ 'describeServicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dFailures'
--
-- * 'dServices'
--
-- * 'dStatus'
data DescribeServicesResponse = DescribeServicesResponse'
    { _dFailures :: !(Maybe [Failure])
    , _dServices :: !(Maybe [ContainerService])
    , _dStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeServicesResponse' smart constructor.
describeServicesResponse :: Int -> DescribeServicesResponse
describeServicesResponse pStatus =
    DescribeServicesResponse'
    { _dFailures = Nothing
    , _dServices = Nothing
    , _dStatus = pStatus
    }

-- | Any failures associated with the call.
dFailures :: Lens' DescribeServicesResponse [Failure]
dFailures = lens _dFailures (\ s a -> s{_dFailures = a}) . _Default;

-- | The list of services described.
dServices :: Lens' DescribeServicesResponse [ContainerService]
dServices = lens _dServices (\ s a -> s{_dServices = a}) . _Default;

-- | FIXME: Undocumented member.
dStatus :: Lens' DescribeServicesResponse Int
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});
