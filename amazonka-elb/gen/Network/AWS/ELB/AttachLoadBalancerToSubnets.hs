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
      AttachLoadBalancerToSubnets
    -- ** Request constructor
    , attachLoadBalancerToSubnets
    -- ** Request lenses
    , albtsLoadBalancerName
    , albtsSubnets

    -- * Response
    , AttachLoadBalancerToSubnetsResponse
    -- ** Response constructor
    , attachLoadBalancerToSubnetsResponse
    -- ** Response lenses
    , albtsrSubnets
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets
    { _albtsLoadBalancerName :: Text
    , _albtsSubnets          :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'AttachLoadBalancerToSubnets' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'albtsLoadBalancerName' @::@ 'Text'
--
-- * 'albtsSubnets' @::@ ['Text']
--
attachLoadBalancerToSubnets :: Text -- ^ 'albtsLoadBalancerName'
                            -> AttachLoadBalancerToSubnets
attachLoadBalancerToSubnets p1 = AttachLoadBalancerToSubnets
    { _albtsLoadBalancerName = p1
    , _albtsSubnets          = mempty
    }

-- | The name associated with the load balancer. The name must be unique
-- within the set of load balancers associated with your AWS account.
albtsLoadBalancerName :: Lens' AttachLoadBalancerToSubnets Text
albtsLoadBalancerName =
    lens _albtsLoadBalancerName (\s a -> s { _albtsLoadBalancerName = a })

-- | A list of subnet IDs to add for the load balancer. You can add only one
-- subnet per Availability Zone.
albtsSubnets :: Lens' AttachLoadBalancerToSubnets [Text]
albtsSubnets = lens _albtsSubnets (\s a -> s { _albtsSubnets = a })

instance ToQuery AttachLoadBalancerToSubnets

instance ToPath AttachLoadBalancerToSubnets where
    toPath = const "/"

newtype AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse
    { _albtsrSubnets :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance IsList AttachLoadBalancerToSubnetsResponse where
    type Item AttachLoadBalancerToSubnetsResponse = Text

    fromList = AttachLoadBalancerToSubnetsResponse . fromList
    toList   = toList . _albtsrSubnets

-- | 'AttachLoadBalancerToSubnetsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'albtsrSubnets' @::@ ['Text']
--
attachLoadBalancerToSubnetsResponse :: AttachLoadBalancerToSubnetsResponse
attachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse
    { _albtsrSubnets = mempty
    }

-- | A list of subnet IDs attached to the load balancer.
albtsrSubnets :: Lens' AttachLoadBalancerToSubnetsResponse [Text]
albtsrSubnets = lens _albtsrSubnets (\s a -> s { _albtsrSubnets = a })

instance FromXML AttachLoadBalancerToSubnetsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AttachLoadBalancerToSubnetsResponse"

instance AWSRequest AttachLoadBalancerToSubnets where
    type Sv AttachLoadBalancerToSubnets = ELB
    type Rs AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnetsResponse

    request  = post "AttachLoadBalancerToSubnets"
    response = xmlResponse $ \h x -> AttachLoadBalancerToSubnetsResponse
        <$> x %| "Subnets"
