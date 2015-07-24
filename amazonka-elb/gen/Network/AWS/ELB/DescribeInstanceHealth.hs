{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeInstanceHealth
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the state of the specified instances registered with the
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
    , dihrsInstanceStates
    , dihrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeInstanceHealth' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dihInstances'
--
-- * 'dihLoadBalancerName'
data DescribeInstanceHealth = DescribeInstanceHealth'
    { _dihInstances        :: !(Maybe [Instance])
    , _dihLoadBalancerName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInstanceHealth' smart constructor.
describeInstanceHealth :: Text -> DescribeInstanceHealth
describeInstanceHealth pLoadBalancerName_ =
    DescribeInstanceHealth'
    { _dihInstances = Nothing
    , _dihLoadBalancerName = pLoadBalancerName_
    }

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
        request = post "DescribeInstanceHealth"
        response
          = receiveXMLWrapper "DescribeInstanceHealthResult"
              (\ s h x ->
                 DescribeInstanceHealthResponse' <$>
                   (x .@? "InstanceStates" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
-- * 'dihrsInstanceStates'
--
-- * 'dihrsStatus'
data DescribeInstanceHealthResponse = DescribeInstanceHealthResponse'
    { _dihrsInstanceStates :: !(Maybe [InstanceState])
    , _dihrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInstanceHealthResponse' smart constructor.
describeInstanceHealthResponse :: Int -> DescribeInstanceHealthResponse
describeInstanceHealthResponse pStatus_ =
    DescribeInstanceHealthResponse'
    { _dihrsInstanceStates = Nothing
    , _dihrsStatus = pStatus_
    }

-- | Information about the health of the instances.
dihrsInstanceStates :: Lens' DescribeInstanceHealthResponse [InstanceState]
dihrsInstanceStates = lens _dihrsInstanceStates (\ s a -> s{_dihrsInstanceStates = a}) . _Default;

-- | FIXME: Undocumented member.
dihrsStatus :: Lens' DescribeInstanceHealthResponse Int
dihrsStatus = lens _dihrsStatus (\ s a -> s{_dihrsStatus = a});
