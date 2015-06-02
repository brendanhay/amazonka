{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.RegisterInstancesWithLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds the specified instances to the specified load balancer.
--
-- The instance must be a running instance in the same network as the load
-- balancer (EC2-Classic or the same VPC). If you have EC2-Classic instances and
-- a load balancer in a VPC with ClassicLink enabled, you can link the
-- EC2-Classic instances to that VPC and then register the linked EC2-Classic
-- instances with the load balancer in the VPC.
--
-- Note that 'RegisterInstanceWithLoadBalancer' completes when the request has
-- been registered. Instance registration happens shortly afterwards. To check
-- the state of the registered instances, use 'DescribeLoadBalancers' or 'DescribeInstanceHealth'.
--
-- After the instance is registered, it starts receiving traffic and requests
-- from the load balancer. Any instance that is not in one of the Availability
-- Zones registered for the load balancer is moved to the 'OutOfService' state. If
-- an Availability Zone is added to the load balancer later, any instances
-- registered with the load balancer move to the 'InService' state.
--
-- If you stop an instance registered with a load balancer and then start it,
-- the IP addresses associated with the instance changes. Elastic Load Balancing
-- cannot recognize the new IP address, which prevents it from routing traffic
-- to the instances. We recommend that you use the following sequence: stop the
-- instance, deregister the instance, start the instance, and then register the
-- instance. To deregister instances from a load balancer, use 'DeregisterInstancesFromLoadBalancer'.
--
-- For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/US_DeReg_Reg_Instances.html Deregister and Register EC2 Instances> in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_RegisterInstancesWithLoadBalancer.html>
module Network.AWS.ELB.RegisterInstancesWithLoadBalancer
    (
    -- * Request
      RegisterInstancesWithLoadBalancer
    -- ** Request constructor
    , registerInstancesWithLoadBalancer
    -- ** Request lenses
    , riwlbInstances
    , riwlbLoadBalancerName

    -- * Response
    , RegisterInstancesWithLoadBalancerResponse
    -- ** Response constructor
    , registerInstancesWithLoadBalancerResponse
    -- ** Response lenses
    , riwlbrInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data RegisterInstancesWithLoadBalancer = RegisterInstancesWithLoadBalancer
    { _riwlbInstances        :: List "member" Instance
    , _riwlbLoadBalancerName :: Text
    } deriving (Eq, Read, Show)

-- | 'RegisterInstancesWithLoadBalancer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riwlbInstances' @::@ ['Instance']
--
-- * 'riwlbLoadBalancerName' @::@ 'Text'
--
registerInstancesWithLoadBalancer :: Text -- ^ 'riwlbLoadBalancerName'
                                  -> RegisterInstancesWithLoadBalancer
registerInstancesWithLoadBalancer p1 = RegisterInstancesWithLoadBalancer
    { _riwlbLoadBalancerName = p1
    , _riwlbInstances        = mempty
    }

-- | The IDs of the instances.
riwlbInstances :: Lens' RegisterInstancesWithLoadBalancer [Instance]
riwlbInstances = lens _riwlbInstances (\s a -> s { _riwlbInstances = a }) . _List

-- | The name of the load balancer.
riwlbLoadBalancerName :: Lens' RegisterInstancesWithLoadBalancer Text
riwlbLoadBalancerName =
    lens _riwlbLoadBalancerName (\s a -> s { _riwlbLoadBalancerName = a })

newtype RegisterInstancesWithLoadBalancerResponse = RegisterInstancesWithLoadBalancerResponse
    { _riwlbrInstances :: List "member" Instance
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList RegisterInstancesWithLoadBalancerResponse where
    type Item RegisterInstancesWithLoadBalancerResponse = Instance

    fromList = RegisterInstancesWithLoadBalancerResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _riwlbrInstances

-- | 'RegisterInstancesWithLoadBalancerResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riwlbrInstances' @::@ ['Instance']
--
registerInstancesWithLoadBalancerResponse :: RegisterInstancesWithLoadBalancerResponse
registerInstancesWithLoadBalancerResponse = RegisterInstancesWithLoadBalancerResponse
    { _riwlbrInstances = mempty
    }

-- | The updated list of instances for the load balancer.
riwlbrInstances :: Lens' RegisterInstancesWithLoadBalancerResponse [Instance]
riwlbrInstances = lens _riwlbrInstances (\s a -> s { _riwlbrInstances = a }) . _List

instance ToPath RegisterInstancesWithLoadBalancer where
    toPath = const "/"

instance ToQuery RegisterInstancesWithLoadBalancer where
    toQuery RegisterInstancesWithLoadBalancer{..} = mconcat
        [ "Instances"        =? _riwlbInstances
        , "LoadBalancerName" =? _riwlbLoadBalancerName
        ]

instance ToHeaders RegisterInstancesWithLoadBalancer

instance AWSRequest RegisterInstancesWithLoadBalancer where
    type Sv RegisterInstancesWithLoadBalancer = ELB
    type Rs RegisterInstancesWithLoadBalancer = RegisterInstancesWithLoadBalancerResponse

    request  = post "RegisterInstancesWithLoadBalancer"
    response = xmlResponse

instance FromXML RegisterInstancesWithLoadBalancerResponse where
    parseXML = withElement "RegisterInstancesWithLoadBalancerResult" $ \x -> RegisterInstancesWithLoadBalancerResponse
        <$> x .@? "Instances" .!@ mempty
