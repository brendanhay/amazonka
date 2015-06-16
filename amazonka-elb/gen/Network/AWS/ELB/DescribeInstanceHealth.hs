{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.DescribeInstanceHealth
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

-- | Describes the state of the specified instances registered with the
-- specified load balancer. If no instances are specified, the call
-- describes the state of all instances registered with the load balancer,
-- not including any terminated instances.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeInstanceHealth.html>
module Network.AWS.ELB.DescribeInstanceHealth
    (
    -- * Request
      DescribeInstanceHealth
    -- ** Request constructor
    , describeInstanceHealth
    -- ** Request lenses
    , dihInstances
    , dihLoadBalancerName

    -- * Response
    , DescribeInstanceHealthResponse
    -- ** Response constructor
    , describeInstanceHealthResponse
    -- ** Response lenses
    , dihrInstanceStates
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ELB.Types

-- | /See:/ 'describeInstanceHealth' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dihInstances'
--
-- * 'dihLoadBalancerName'
data DescribeInstanceHealth = DescribeInstanceHealth'{_dihInstances :: Maybe [Instance], _dihLoadBalancerName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeInstanceHealth' smart constructor.
describeInstanceHealth :: Text -> DescribeInstanceHealth
describeInstanceHealth pLoadBalancerName = DescribeInstanceHealth'{_dihInstances = Nothing, _dihLoadBalancerName = pLoadBalancerName};

-- | The IDs of the instances.
dihInstances :: Lens' DescribeInstanceHealth [Instance]
dihInstances = lens _dihInstances (\ s a -> s{_dihInstances = a}) . _Default;

-- | The name of the load balancer.
dihLoadBalancerName :: Lens' DescribeInstanceHealth Text
dihLoadBalancerName = lens _dihLoadBalancerName (\ s a -> s{_dihLoadBalancerName = a});

instance AWSRequest DescribeInstanceHealth where
        type Sv DescribeInstanceHealth = ELB
        type Rs DescribeInstanceHealth =
             DescribeInstanceHealthResponse
        request = post
        response
          = receiveXMLWrapper "DescribeInstanceHealthResult"
              (\ s h x ->
                 DescribeInstanceHealthResponse' <$>
                   (x .@? "InstanceStates" .!@ mempty >>=
                      may (parseXMLList "member")))

instance ToHeaders DescribeInstanceHealth where
        toHeaders = const mempty

instance ToPath DescribeInstanceHealth where
        toPath = const "/"

instance ToQuery DescribeInstanceHealth where
        toQuery DescribeInstanceHealth'{..}
          = mconcat
              ["Action" =:
                 ("DescribeInstanceHealth" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "Instances" =:
                 toQuery (toQueryList "member" <$> _dihInstances),
               "LoadBalancerName" =: _dihLoadBalancerName]

-- | /See:/ 'describeInstanceHealthResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dihrInstanceStates'
newtype DescribeInstanceHealthResponse = DescribeInstanceHealthResponse'{_dihrInstanceStates :: Maybe [InstanceState]} deriving (Eq, Read, Show)

-- | 'DescribeInstanceHealthResponse' smart constructor.
describeInstanceHealthResponse :: DescribeInstanceHealthResponse
describeInstanceHealthResponse = DescribeInstanceHealthResponse'{_dihrInstanceStates = Nothing};

-- | Information about the health of the instances.
dihrInstanceStates :: Lens' DescribeInstanceHealthResponse [InstanceState]
dihrInstanceStates = lens _dihrInstanceStates (\ s a -> s{_dihrInstanceStates = a}) . _Default;
