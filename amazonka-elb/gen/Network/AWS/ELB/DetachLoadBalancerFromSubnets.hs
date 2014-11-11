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

-- Module      : Network.AWS.ELB.DetachLoadBalancerFromSubnets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes subnets from the set of configured subnets in the Amazon Virtual
-- Private Cloud (Amazon VPC) for the load balancer. After a subnet is removed
-- all of the EC2 instances registered with the load balancer that are in the
-- removed subnet will go into the OutOfService state. When a subnet is
-- removed, the load balancer will balance the traffic among the remaining
-- routable subnets for the load balancer.
module Network.AWS.ELB.DetachLoadBalancerFromSubnets
    (
    -- * Request
      DetachLoadBalancerFromSubnetsInput
    -- ** Request constructor
    , detachLoadBalancerFromSubnetsInput
    -- ** Request lenses
    , dlbfsiLoadBalancerName
    , dlbfsiSubnets

    -- * Response
    , DetachLoadBalancerFromSubnetsOutput
    -- ** Response constructor
    , detachLoadBalancerFromSubnetsOutput
    -- ** Response lenses
    , dlbfsoSubnets
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data DetachLoadBalancerFromSubnetsInput = DetachLoadBalancerFromSubnetsInput
    { _dlbfsiLoadBalancerName :: Text
    , _dlbfsiSubnets          :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DetachLoadBalancerFromSubnetsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbfsiLoadBalancerName' @::@ 'Text'
--
-- * 'dlbfsiSubnets' @::@ ['Text']
--
detachLoadBalancerFromSubnetsInput :: Text -- ^ 'dlbfsiLoadBalancerName'
                                   -> DetachLoadBalancerFromSubnetsInput
detachLoadBalancerFromSubnetsInput p1 = DetachLoadBalancerFromSubnetsInput
    { _dlbfsiLoadBalancerName = p1
    , _dlbfsiSubnets          = mempty
    }

-- | The name associated with the load balancer to be detached.
dlbfsiLoadBalancerName :: Lens' DetachLoadBalancerFromSubnetsInput Text
dlbfsiLoadBalancerName =
    lens _dlbfsiLoadBalancerName (\s a -> s { _dlbfsiLoadBalancerName = a })

-- | A list of subnet IDs to remove from the set of configured subnets for the
-- load balancer.
dlbfsiSubnets :: Lens' DetachLoadBalancerFromSubnetsInput [Text]
dlbfsiSubnets = lens _dlbfsiSubnets (\s a -> s { _dlbfsiSubnets = a })
instance ToQuery DetachLoadBalancerFromSubnetsInput

instance ToPath DetachLoadBalancerFromSubnetsInput where
    toPath = const "/"

newtype DetachLoadBalancerFromSubnetsOutput = DetachLoadBalancerFromSubnetsOutput
    { _dlbfsoSubnets :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DetachLoadBalancerFromSubnetsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbfsoSubnets' @::@ ['Text']
--
detachLoadBalancerFromSubnetsOutput :: DetachLoadBalancerFromSubnetsOutput
detachLoadBalancerFromSubnetsOutput = DetachLoadBalancerFromSubnetsOutput
    { _dlbfsoSubnets = mempty
    }

-- | A list of subnet IDs the load balancer is now attached to.
dlbfsoSubnets :: Lens' DetachLoadBalancerFromSubnetsOutput [Text]
dlbfsoSubnets = lens _dlbfsoSubnets (\s a -> s { _dlbfsoSubnets = a })
instance FromXML DetachLoadBalancerFromSubnetsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DetachLoadBalancerFromSubnetsOutput"

instance AWSRequest DetachLoadBalancerFromSubnetsInput where
    type Sv DetachLoadBalancerFromSubnetsInput = ELB
    type Rs DetachLoadBalancerFromSubnetsInput = DetachLoadBalancerFromSubnetsOutput

    request  = post "DetachLoadBalancerFromSubnets"
    response = xmlResponse $ \h x -> DetachLoadBalancerFromSubnetsOutput
        <$> x %| "Subnets"
