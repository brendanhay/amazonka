{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.RegisterInstancesWithLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified instances to the specified load balancer.
--
-- The instance must be a running instance in the same network as the load
-- balancer (EC2-Classic or the same VPC). If you have EC2-Classic
-- instances and a load balancer in a VPC with ClassicLink enabled, you can
-- link the EC2-Classic instances to that VPC and then register the linked
-- EC2-Classic instances with the load balancer in the VPC.
--
-- Note that @RegisterInstanceWithLoadBalancer@ completes when the request
-- has been registered. Instance registration happens shortly afterwards.
-- To check the state of the registered instances, use
-- DescribeLoadBalancers or DescribeInstanceHealth.
--
-- After the instance is registered, it starts receiving traffic and
-- requests from the load balancer. Any instance that is not in one of the
-- Availability Zones registered for the load balancer is moved to the
-- @OutOfService@ state. If an Availability Zone is added to the load
-- balancer later, any instances registered with the load balancer move to
-- the @InService@ state.
--
-- If you stop an instance registered with a load balancer and then start
-- it, the IP addresses associated with the instance changes. Elastic Load
-- Balancing cannot recognize the new IP address, which prevents it from
-- routing traffic to the instances. We recommend that you use the
-- following sequence: stop the instance, deregister the instance, start
-- the instance, and then register the instance. To deregister instances
-- from a load balancer, use DeregisterInstancesFromLoadBalancer.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/US_DeReg_Reg_Instances.html Deregister and Register EC2 Instances>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_RegisterInstancesWithLoadBalancer.html>
module Network.AWS.ELB.RegisterInstancesWithLoadBalancer
    (
    -- * Request
      RegisterInstancesWithLoadBalancer
    -- ** Request constructor
    , registerInstancesWithLoadBalancer
    -- ** Request lenses
    , riwlbLoadBalancerName
    , riwlbInstances

    -- * Response
    , RegisterInstancesWithLoadBalancerResponse
    -- ** Response constructor
    , registerInstancesWithLoadBalancerResponse
    -- ** Response lenses
    , riwlbrsInstances
    , riwlbrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerInstancesWithLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riwlbLoadBalancerName'
--
-- * 'riwlbInstances'
data RegisterInstancesWithLoadBalancer = RegisterInstancesWithLoadBalancer'
    { _riwlbLoadBalancerName :: !Text
    , _riwlbInstances        :: ![Instance]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterInstancesWithLoadBalancer' smart constructor.
registerInstancesWithLoadBalancer :: Text -> RegisterInstancesWithLoadBalancer
registerInstancesWithLoadBalancer pLoadBalancerName_ =
    RegisterInstancesWithLoadBalancer'
    { _riwlbLoadBalancerName = pLoadBalancerName_
    , _riwlbInstances = mempty
    }

-- | The name of the load balancer.
riwlbLoadBalancerName :: Lens' RegisterInstancesWithLoadBalancer Text
riwlbLoadBalancerName = lens _riwlbLoadBalancerName (\ s a -> s{_riwlbLoadBalancerName = a});

-- | The IDs of the instances.
riwlbInstances :: Lens' RegisterInstancesWithLoadBalancer [Instance]
riwlbInstances = lens _riwlbInstances (\ s a -> s{_riwlbInstances = a});

instance AWSRequest RegisterInstancesWithLoadBalancer
         where
        type Sv RegisterInstancesWithLoadBalancer = ELB
        type Rs RegisterInstancesWithLoadBalancer =
             RegisterInstancesWithLoadBalancerResponse
        request = post "RegisterInstancesWithLoadBalancer"
        response
          = receiveXMLWrapper
              "RegisterInstancesWithLoadBalancerResult"
              (\ s h x ->
                 RegisterInstancesWithLoadBalancerResponse' <$>
                   (x .@? "Instances" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders RegisterInstancesWithLoadBalancer
         where
        toHeaders = const mempty

instance ToPath RegisterInstancesWithLoadBalancer
         where
        toPath = const "/"

instance ToQuery RegisterInstancesWithLoadBalancer
         where
        toQuery RegisterInstancesWithLoadBalancer'{..}
          = mconcat
              ["Action" =:
                 ("RegisterInstancesWithLoadBalancer" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _riwlbLoadBalancerName,
               "Instances" =: toQueryList "member" _riwlbInstances]

-- | /See:/ 'registerInstancesWithLoadBalancerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riwlbrsInstances'
--
-- * 'riwlbrsStatus'
data RegisterInstancesWithLoadBalancerResponse = RegisterInstancesWithLoadBalancerResponse'
    { _riwlbrsInstances :: !(Maybe [Instance])
    , _riwlbrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterInstancesWithLoadBalancerResponse' smart constructor.
registerInstancesWithLoadBalancerResponse :: Int -> RegisterInstancesWithLoadBalancerResponse
registerInstancesWithLoadBalancerResponse pStatus_ =
    RegisterInstancesWithLoadBalancerResponse'
    { _riwlbrsInstances = Nothing
    , _riwlbrsStatus = pStatus_
    }

-- | The updated list of instances for the load balancer.
riwlbrsInstances :: Lens' RegisterInstancesWithLoadBalancerResponse [Instance]
riwlbrsInstances = lens _riwlbrsInstances (\ s a -> s{_riwlbrsInstances = a}) . _Default;

-- | FIXME: Undocumented member.
riwlbrsStatus :: Lens' RegisterInstancesWithLoadBalancerResponse Int
riwlbrsStatus = lens _riwlbrsStatus (\ s a -> s{_riwlbrsStatus = a});
