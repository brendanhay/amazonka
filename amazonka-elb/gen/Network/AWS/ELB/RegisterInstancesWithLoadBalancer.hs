{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.ELB.RegisterInstancesWithLoadBalancer
    (
    -- * Request
      RegisterEndPointsInput
    -- ** Request constructor
    , registerEndPointsInput
    -- ** Request lenses
    , repiInstances
    , repiLoadBalancerName

    -- * Response
    , RegisterEndPointsOutput
    -- ** Response constructor
    , registerEndPointsOutput
    -- ** Response lenses
    , repoInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data RegisterEndPointsInput = RegisterEndPointsInput
    { _repiInstances        :: [Instance]
    , _repiLoadBalancerName :: Text
    } (Eq, Show, Generic)

-- | 'RegisterEndPointsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'repiInstances' @::@ ['Instance']
--
-- * 'repiLoadBalancerName' @::@ 'Text'
--
registerEndPointsInput :: Text -- ^ 'repiLoadBalancerName'
                       -> RegisterEndPointsInput
registerEndPointsInput p1 = RegisterEndPointsInput
    { _repiLoadBalancerName = p1
    , _repiInstances        = mempty
    }

-- | A list of instance IDs that should be registered with the load balancer.
repiInstances :: Lens' RegisterEndPointsInput [Instance]
repiInstances = lens _repiInstances (\s a -> s { _repiInstances = a })

-- | The name associated with the load balancer. The name must be unique
-- within your set of load balancers.
repiLoadBalancerName :: Lens' RegisterEndPointsInput Text
repiLoadBalancerName =
    lens _repiLoadBalancerName (\s a -> s { _repiLoadBalancerName = a })
instance ToQuery RegisterEndPointsInput

instance ToPath RegisterEndPointsInput where
    toPath = const "/"

newtype RegisterEndPointsOutput = RegisterEndPointsOutput
    { _repoInstances :: [Instance]
    } (Eq, Show, Generic, Foldable, Traversable, Monoid, Semigroup)

-- | 'RegisterEndPointsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'repoInstances' @::@ ['Instance']
--
registerEndPointsOutput :: RegisterEndPointsOutput
registerEndPointsOutput = RegisterEndPointsOutput
    { _repoInstances = mempty
    }

-- | An updated list of instances for the load balancer.
repoInstances :: Lens' RegisterEndPointsOutput [Instance]
repoInstances = lens _repoInstances (\s a -> s { _repoInstances = a })

instance FromXML RegisterEndPointsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RegisterEndPointsOutput"

instance AWSRequest RegisterEndPointsInput where
    type Sv RegisterEndPointsInput = ELB
    type Rs RegisterEndPointsInput = RegisterEndPointsOutput

    request  = post "RegisterInstancesWithLoadBalancer"
    response = xmlResponse $ \h x -> RegisterEndPointsOutput
        <$> x %| "Instances"
