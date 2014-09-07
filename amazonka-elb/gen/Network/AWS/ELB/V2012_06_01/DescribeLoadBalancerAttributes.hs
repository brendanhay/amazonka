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
    , mkDescribeLoadBalancerAttributes
    -- ** Request lenses
    , dlbaLoadBalancerName

    -- * Response
    , DescribeLoadBalancerAttributesResponse
    -- ** Response lenses
    , dlbarsLoadBalancerAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | The input for the DescribeLoadBalancerAttributes action.
newtype DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes
    { _dlbaLoadBalancerName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBalancerAttributes' request.
mkDescribeLoadBalancerAttributes :: Text -- ^ 'dlbaLoadBalancerName'
                                 -> DescribeLoadBalancerAttributes
mkDescribeLoadBalancerAttributes p1 = DescribeLoadBalancerAttributes
    { _dlbaLoadBalancerName = p1
    }

-- | The name of the load balancer.
dlbaLoadBalancerName :: Lens' DescribeLoadBalancerAttributes Text
dlbaLoadBalancerName =
    lens _dlbaLoadBalancerName (\s a -> s { _dlbaLoadBalancerName = a })

instance ToQuery DescribeLoadBalancerAttributes where
    toQuery = genericQuery def

-- | The following element is returned in a structure named
-- DescribeLoadBalancerAttributesResult.
newtype DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse
    { _dlbarsLoadBalancerAttributes :: Maybe LoadBalancerAttributes
    } deriving (Show, Generic)

-- | The load balancer attributes structure.
dlbarsLoadBalancerAttributes :: Lens' DescribeLoadBalancerAttributesResponse (Maybe LoadBalancerAttributes)
dlbarsLoadBalancerAttributes =
    lens _dlbarsLoadBalancerAttributes
         (\s a -> s { _dlbarsLoadBalancerAttributes = a })

instance FromXML DescribeLoadBalancerAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoadBalancerAttributes where
    type Sv DescribeLoadBalancerAttributes = ELB
    type Rs DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributesResponse

    request = post "DescribeLoadBalancerAttributes"
    response _ = xmlResponse
