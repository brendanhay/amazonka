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

-- | Adds new instances to the load balancer. Once the instance is registered,
-- it starts receiving traffic and requests from the load balancer. Any
-- instance that is not in any of the Availability Zones registered for the
-- load balancer will be moved to the OutOfService state. It will move to the
-- InService state when the Availability Zone is added to the load balancer.
-- When an instance registered with a load balancer is stopped and then
-- restarted, the IP addresses associated with the instance changes. Elastic
-- Load Balancing cannot recognize the new IP address, which prevents it from
-- routing traffic to the instances. We recommend that you de-register your
-- Amazon EC2 instances from your load balancer after you stop your instance,
-- and then register the load balancer with your instance after you've
-- restarted. To de-register your instances from load balancer, use
-- DeregisterInstancesFromLoadBalancer action. For more information, see
-- De-register and Register Amazon EC2 Instances in the Elastic Load Balancing
-- Developer Guide. In order for this call to be successful, you must provide
-- the same account credentials as those that were used to create the load
-- balancer. Completion of this API does not guarantee that operation has
-- completed. Rather, it means that the request has been registered and the
-- changes will happen shortly. You can use DescribeLoadBalancers or
-- DescribeInstanceHealth action to check the state of the newly registered
-- instances.
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
    { _riwlbInstances        :: List "Instances" Instance
    , _riwlbLoadBalancerName :: Text
    } deriving (Eq, Show)

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

-- | A list of instance IDs that should be registered with the load balancer.
riwlbInstances :: Lens' RegisterInstancesWithLoadBalancer [Instance]
riwlbInstances = lens _riwlbInstances (\s a -> s { _riwlbInstances = a }) . _List

-- | The name associated with the load balancer. The name must be unique
-- within your set of load balancers.
riwlbLoadBalancerName :: Lens' RegisterInstancesWithLoadBalancer Text
riwlbLoadBalancerName =
    lens _riwlbLoadBalancerName (\s a -> s { _riwlbLoadBalancerName = a })

newtype RegisterInstancesWithLoadBalancerResponse = RegisterInstancesWithLoadBalancerResponse
    { _riwlbrInstances :: List "Instances" Instance
    } deriving (Eq, Show, Monoid, Semigroup)

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

-- | An updated list of instances for the load balancer.
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
        <$> x .@? "Instances"
