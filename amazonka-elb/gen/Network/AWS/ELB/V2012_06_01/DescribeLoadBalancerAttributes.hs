{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DescribeLoadBalancerAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns detailed information about all of the attributes associated with
-- the specified load balancer.
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeLoadBalancerAttributes &AUTHPARAMS true
-- my-loadbalancer-logs testprefix 5 30 true true 60
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB.V2012_06_01.DescribeLoadBalancerAttributes
    (
    -- * Request
      DescribeLoadBalancerAttributes
    -- ** Request constructor
    , describeLoadBalancerAttributes
    -- ** Request lenses
    , dlbaiLoadBalancerName

    -- * Response
    , DescribeLoadBalancerAttributesResponse
    -- ** Response lenses
    , dlbaoLoadBalancerAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeLoadBalancerAttributes' request.
describeLoadBalancerAttributes :: Text -- ^ 'dlbaiLoadBalancerName'
                               -> DescribeLoadBalancerAttributes
describeLoadBalancerAttributes p1 = DescribeLoadBalancerAttributes
    { _dlbaiLoadBalancerName = p1
    }

data DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes
    { _dlbaiLoadBalancerName :: Text
      -- ^ The name of the load balancer.
    } deriving (Show, Generic)

-- | The name of the load balancer.
dlbaiLoadBalancerName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeLoadBalancerAttributes
    -> f DescribeLoadBalancerAttributes
dlbaiLoadBalancerName f x =
    (\y -> x { _dlbaiLoadBalancerName = y })
       <$> f (_dlbaiLoadBalancerName x)
{-# INLINE dlbaiLoadBalancerName #-}

instance ToQuery DescribeLoadBalancerAttributes where
    toQuery = genericQuery def

data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse
    { _dlbaoLoadBalancerAttributes :: Maybe LoadBalancerAttributes
      -- ^ The load balancer attributes structure.
    } deriving (Show, Generic)

-- | The load balancer attributes structure.
dlbaoLoadBalancerAttributes
    :: Functor f
    => (Maybe LoadBalancerAttributes
    -> f (Maybe LoadBalancerAttributes))
    -> DescribeLoadBalancerAttributesResponse
    -> f DescribeLoadBalancerAttributesResponse
dlbaoLoadBalancerAttributes f x =
    (\y -> x { _dlbaoLoadBalancerAttributes = y })
       <$> f (_dlbaoLoadBalancerAttributes x)
{-# INLINE dlbaoLoadBalancerAttributes #-}

instance FromXML DescribeLoadBalancerAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoadBalancerAttributes where
    type Sv DescribeLoadBalancerAttributes = ELB
    type Rs DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributesResponse

    request = post "DescribeLoadBalancerAttributes"
    response _ = xmlResponse
