{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ECS.DescribeServices
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    , dsrFailures
    , dsrServices
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ECS.Types

-- | /See:/ 'describeServices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desCluster'
--
-- * 'desServices'
data DescribeServices = DescribeServices'{_desCluster :: Maybe Text, _desServices :: [Text]} deriving (Eq, Read, Show)

-- | 'DescribeServices' smart constructor.
describeServices :: [Text] -> DescribeServices
describeServices pServices = DescribeServices'{_desCluster = Nothing, _desServices = pServices};

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
                   x .?> "failures" .!@ mempty <*>
                     x .?> "services" .!@ mempty)

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
-- * 'dsrFailures'
--
-- * 'dsrServices'
data DescribeServicesResponse = DescribeServicesResponse'{_dsrFailures :: [Failure], _dsrServices :: [ContainerService]} deriving (Eq, Read, Show)

-- | 'DescribeServicesResponse' smart constructor.
describeServicesResponse :: DescribeServicesResponse
describeServicesResponse = DescribeServicesResponse'{_dsrFailures = mempty, _dsrServices = mempty};

-- | Any failures associated with the call.
dsrFailures :: Lens' DescribeServicesResponse [Failure]
dsrFailures = lens _dsrFailures (\ s a -> s{_dsrFailures = a});

-- | The list of services described.
dsrServices :: Lens' DescribeServicesResponse [ContainerService]
dsrServices = lens _dsrServices (\ s a -> s{_dsrServices = a});
