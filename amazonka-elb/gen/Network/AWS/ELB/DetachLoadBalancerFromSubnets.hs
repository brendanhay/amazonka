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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Removes the specified subnets from the set of configured subnets for the load
-- balancer.
--
-- After a subnet is removed, all EC2 instances registered with the load
-- balancer in the removed subnet go into the 'OutOfService' state. Then, the load
-- balancer balances the traffic among the remaining routable subnets.
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
    , _dlbfsSubnets          :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

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

-- | The name of the load balancer.
dlbfsLoadBalancerName :: Lens' DetachLoadBalancerFromSubnets Text
dlbfsLoadBalancerName =
    lens _dlbfsLoadBalancerName (\s a -> s { _dlbfsLoadBalancerName = a })

-- | The IDs of the subnets.
dlbfsSubnets :: Lens' DetachLoadBalancerFromSubnets [Text]
dlbfsSubnets = lens _dlbfsSubnets (\s a -> s { _dlbfsSubnets = a }) . _List

newtype DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse
    { _dlbfsrSubnets :: List "member" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

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

-- | The IDs of the remaining subnets for the load balancer.
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
        <$> x .@? "Subnets" .!@ mempty
