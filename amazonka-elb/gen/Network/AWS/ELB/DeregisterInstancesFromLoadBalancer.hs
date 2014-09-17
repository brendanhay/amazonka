{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters instances from the load balancer. Once the instance is
-- deregistered, it will stop receiving traffic from the load balancer. In
-- order to successfully call this API, the same account credentials as those
-- used to create the load balancer must be provided. For more information,
-- see De-register and Register Amazon EC2 Instances in the Elastic Load
-- Balancing Developer Guide. You can use DescribeLoadBalancers to verify if
-- the instance is deregistered from the load balancer. Deregister instance
-- i-e3677ad7 from MyHTTPSLoadBalancer load balancer.
-- https://elasticloadbalancing.amazonaws.com/?Instances.member.1.InstanceId=i-e3677ad7
-- &LoadBalancerName=MyHTTPSLoadBalancer &Version=2012-06-01
-- &Action=DeregisterInstancesFromLoadBalancer &AUTHPARAMS i-6ec63d59
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
    (
    -- * Request
      DeregisterInstancesFromLoadBalancer
    -- ** Request constructor
    , mkDeregisterInstancesFromLoadBalancer
    -- ** Request lenses
    , diflbLoadBalancerName
    , diflbInstances

    -- * Response
    , DeregisterInstancesFromLoadBalancerResponse
    -- ** Response constructor
    , mkDeregisterInstancesFromLoadBalancerResponse
    -- ** Response lenses
    , diflbrInstances
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the DeregisterInstancesFromLoadBalancer action.
data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer
    { _diflbLoadBalancerName :: Text
    , _diflbInstances :: [Instance]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeregisterInstancesFromLoadBalancer' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @Instances ::@ @[Instance]@
--
mkDeregisterInstancesFromLoadBalancer :: Text -- ^ 'diflbLoadBalancerName'
                                      -> [Instance] -- ^ 'diflbInstances'
                                      -> DeregisterInstancesFromLoadBalancer
mkDeregisterInstancesFromLoadBalancer p1 p2 = DeregisterInstancesFromLoadBalancer
    { _diflbLoadBalancerName = p1
    , _diflbInstances = p2
    }

-- | The name associated with the load balancer.
diflbLoadBalancerName :: Lens' DeregisterInstancesFromLoadBalancer Text
diflbLoadBalancerName =
    lens _diflbLoadBalancerName (\s a -> s { _diflbLoadBalancerName = a })

-- | A list of EC2 instance IDs consisting of all instances to be deregistered.
diflbInstances :: Lens' DeregisterInstancesFromLoadBalancer [Instance]
diflbInstances = lens _diflbInstances (\s a -> s { _diflbInstances = a })

instance ToQuery DeregisterInstancesFromLoadBalancer where
    toQuery = genericQuery def

-- | The output for the DeregisterInstancesFromLoadBalancer action.
newtype DeregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse
    { _diflbrInstances :: [Instance]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeregisterInstancesFromLoadBalancerResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Instances ::@ @[Instance]@
--
mkDeregisterInstancesFromLoadBalancerResponse :: DeregisterInstancesFromLoadBalancerResponse
mkDeregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse
    { _diflbrInstances = mempty
    }

-- | An updated list of remaining instances registered with the load balancer.
diflbrInstances :: Lens' DeregisterInstancesFromLoadBalancerResponse [Instance]
diflbrInstances = lens _diflbrInstances (\s a -> s { _diflbrInstances = a })

instance FromXML DeregisterInstancesFromLoadBalancerResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeregisterInstancesFromLoadBalancer where
    type Sv DeregisterInstancesFromLoadBalancer = ELB
    type Rs DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancerResponse

    request = post "DeregisterInstancesFromLoadBalancer"
    response _ = xmlResponse
