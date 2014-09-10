{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB
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
module Network.AWS.ELB
    (
    -- * Request
      DescribeLoadBalancerAttributes
    -- ** Request constructor
    , mkDescribeLoadBalancerAttributes
    -- ** Request lenses
    , dlbaLoadBalancerName

    -- * Response
    , DescribeLoadBalancerAttributesResponse
    -- ** Response constructor
    , mkDescribeLoadBalancerAttributesResponse
    -- ** Response lenses
    , dlbarLoadBalancerAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the DescribeLoadBalancerAttributes action.
newtype DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes
    { _dlbaLoadBalancerName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBalancerAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
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
    { _dlbarLoadBalancerAttributes :: Maybe LoadBalancerAttributes
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBalancerAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerAttributes ::@ @Maybe LoadBalancerAttributes@
--
mkDescribeLoadBalancerAttributesResponse :: DescribeLoadBalancerAttributesResponse
mkDescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse
    { _dlbarLoadBalancerAttributes = Nothing
    }

-- | The load balancer attributes structure.
dlbarLoadBalancerAttributes :: Lens' DescribeLoadBalancerAttributesResponse (Maybe LoadBalancerAttributes)
dlbarLoadBalancerAttributes =
    lens _dlbarLoadBalancerAttributes
         (\s a -> s { _dlbarLoadBalancerAttributes = a })

instance FromXML DescribeLoadBalancerAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoadBalancerAttributes where
    type Sv DescribeLoadBalancerAttributes = ELB
    type Rs DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributesResponse

    request = post "DescribeLoadBalancerAttributes"
    response _ = xmlResponse
