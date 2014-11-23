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
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DetachLoadBalancerFromSubnets.html>
module Network.AWS.ELB.DetachLoadBalancerFromSubnets
    (
    -- * Request
      DetachLoadBalancerFromSubnets
    -- ** Request constructor
    , detachLoadBalancerFromSubnets
    -- ** Request lenses
    , dlbfsLoadBalancerName
    , dlbfsSubnets

    -- * Response
    , DetachLoadBalancerFromSubnetsResponse
    -- ** Response constructor
    , detachLoadBalancerFromSubnetsResponse
    -- ** Response lenses
    , dlbfsrSubnets
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets
    { _dlbfsLoadBalancerName :: Text
    , _dlbfsSubnets          :: List "Subnets" Text
    } deriving (Eq, Ord, Show)

-- | 'DetachLoadBalancerFromSubnets' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbfsLoadBalancerName' @::@ 'Text'
--
-- * 'dlbfsSubnets' @::@ ['Text']
--
detachLoadBalancerFromSubnets :: Text -- ^ 'dlbfsLoadBalancerName'
                              -> DetachLoadBalancerFromSubnets
detachLoadBalancerFromSubnets p1 = DetachLoadBalancerFromSubnets
    { _dlbfsLoadBalancerName = p1
    , _dlbfsSubnets          = mempty
    }

-- | The name associated with the load balancer to be detached.
dlbfsLoadBalancerName :: Lens' DetachLoadBalancerFromSubnets Text
dlbfsLoadBalancerName =
    lens _dlbfsLoadBalancerName (\s a -> s { _dlbfsLoadBalancerName = a })

-- | A list of subnet IDs to remove from the set of configured subnets for the
-- load balancer.
dlbfsSubnets :: Lens' DetachLoadBalancerFromSubnets [Text]
dlbfsSubnets = lens _dlbfsSubnets (\s a -> s { _dlbfsSubnets = a }) . _List

newtype DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse
    { _dlbfsrSubnets :: List "Subnets" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DetachLoadBalancerFromSubnetsResponse where
    type Item DetachLoadBalancerFromSubnetsResponse = Text

    fromList = DetachLoadBalancerFromSubnetsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dlbfsrSubnets

-- | 'DetachLoadBalancerFromSubnetsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbfsrSubnets' @::@ ['Text']
--
detachLoadBalancerFromSubnetsResponse :: DetachLoadBalancerFromSubnetsResponse
detachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse
    { _dlbfsrSubnets = mempty
    }

-- | A list of subnet IDs the load balancer is now attached to.
dlbfsrSubnets :: Lens' DetachLoadBalancerFromSubnetsResponse [Text]
dlbfsrSubnets = lens _dlbfsrSubnets (\s a -> s { _dlbfsrSubnets = a }) . _List

instance ToPath DetachLoadBalancerFromSubnets where
    toPath = const "/"

instance ToQuery DetachLoadBalancerFromSubnets where
    toQuery DetachLoadBalancerFromSubnets{..} = mconcat
        [ "LoadBalancerName" =? _dlbfsLoadBalancerName
        , "Subnets"          =? _dlbfsSubnets
        ]

instance ToHeaders DetachLoadBalancerFromSubnets

instance AWSRequest DetachLoadBalancerFromSubnets where
    type Sv DetachLoadBalancerFromSubnets = ELB
    type Rs DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnetsResponse

    request  = post "DetachLoadBalancerFromSubnets"
    response = xmlResponse

instance FromXML DetachLoadBalancerFromSubnetsResponse where
    parseXML = withElement "DetachLoadBalancerFromSubnetsResult" $ \x -> DetachLoadBalancerFromSubnetsResponse
        <$> x .@? "Subnets"
