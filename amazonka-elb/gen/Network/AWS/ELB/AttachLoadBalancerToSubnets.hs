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

-- Module      : Network.AWS.ELB.AttachLoadBalancerToSubnets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds one or more subnets to the set of configured subnets in the Amazon
-- Virtual Private Cloud (Amazon VPC) for the load balancer. The load
-- balancers evenly distribute requests across all of the registered subnets.
-- For more information, see Deploy Elastic Load Balancing in Amazon VPC in
-- the Elastic Load Balancing Developer Guide.
module Network.AWS.ELB.AttachLoadBalancerToSubnets
    (
    -- * Request
      AttachLoadBalancerToSubnetsInput
    -- ** Request constructor
    , attachLoadBalancerToSubnets
    -- ** Request lenses
    , albtsiLoadBalancerName
    , albtsiSubnets

    -- * Response
    , AttachLoadBalancerToSubnetsOutput
    -- ** Response constructor
    , attachLoadBalancerToSubnetsResponse
    -- ** Response lenses
    , albtsoSubnets
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data AttachLoadBalancerToSubnetsInput = AttachLoadBalancerToSubnetsInput
    { _albtsiLoadBalancerName :: Text
    , _albtsiSubnets          :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'AttachLoadBalancerToSubnetsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'albtsiLoadBalancerName' @::@ 'Text'
--
-- * 'albtsiSubnets' @::@ ['Text']
--
attachLoadBalancerToSubnets :: Text -- ^ 'albtsiLoadBalancerName'
                            -> AttachLoadBalancerToSubnetsInput
attachLoadBalancerToSubnets p1 = AttachLoadBalancerToSubnetsInput
    { _albtsiLoadBalancerName = p1
    , _albtsiSubnets          = mempty
    }

-- | The name associated with the load balancer. The name must be unique
-- within the set of load balancers associated with your AWS account.
albtsiLoadBalancerName :: Lens' AttachLoadBalancerToSubnetsInput Text
albtsiLoadBalancerName =
    lens _albtsiLoadBalancerName (\s a -> s { _albtsiLoadBalancerName = a })

-- | A list of subnet IDs to add for the load balancer. You can add only one
-- subnet per Availability Zone.
albtsiSubnets :: Lens' AttachLoadBalancerToSubnetsInput [Text]
albtsiSubnets = lens _albtsiSubnets (\s a -> s { _albtsiSubnets = a })

instance ToQuery AttachLoadBalancerToSubnetsInput

instance ToPath AttachLoadBalancerToSubnetsInput where
    toPath = const "/"

newtype AttachLoadBalancerToSubnetsOutput = AttachLoadBalancerToSubnetsOutput
    { _albtsoSubnets :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance IsList AttachLoadBalancerToSubnetsOutput where
    type Item AttachLoadBalancerToSubnetsOutput = Text

    fromList = AttachLoadBalancerToSubnetsOutput . fromList
    toList   = toList . _albtsoSubnets

-- | 'AttachLoadBalancerToSubnetsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'albtsoSubnets' @::@ ['Text']
--
attachLoadBalancerToSubnetsResponse :: AttachLoadBalancerToSubnetsOutput
attachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsOutput
    { _albtsoSubnets = mempty
    }

-- | A list of subnet IDs attached to the load balancer.
albtsoSubnets :: Lens' AttachLoadBalancerToSubnetsOutput [Text]
albtsoSubnets = lens _albtsoSubnets (\s a -> s { _albtsoSubnets = a })

instance FromXML AttachLoadBalancerToSubnetsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AttachLoadBalancerToSubnetsOutput"

instance AWSRequest AttachLoadBalancerToSubnetsInput where
    type Sv AttachLoadBalancerToSubnetsInput = ELB
    type Rs AttachLoadBalancerToSubnetsInput = AttachLoadBalancerToSubnetsOutput

    request  = post "AttachLoadBalancerToSubnets"
    response = xmlResponse $ \h x -> AttachLoadBalancerToSubnetsOutput
        <$> x %| "Subnets"
