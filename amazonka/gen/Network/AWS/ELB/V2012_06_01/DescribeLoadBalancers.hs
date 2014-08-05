{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DescribeLoadBalancers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns detailed configuration information for all the load balancers
-- created for the account. If you specify load balancer names, the action
-- returns configuration information of the specified load balancers. In order
-- to retrieve this information, you must provide the same account credentials
-- that was used to create the load balancer. Description of a specified load
-- balancer
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerNames.member.1=MyLoadBalancer
-- &Version=2012-06-01 &Action=DescribeLoadBalancers &AUTHPARAMS
-- MyLoadBalancer 2013-05-24T21:15:31.280Z 90 HTTP:80/ 2 60 10 HTTP 80 HTTP 80
-- i-e4cbe38d us-east-1a ZZZZZZZZZZZ123X
-- MyLoadBalancer-123456789.us-east-1.elb.amazonaws.com internet-facing
-- amazon-elb amazon-elb-sg
-- MyLoadBalancer-123456789.us-east-1.elb.amazonaws.com
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB.V2012_06_01.DescribeLoadBalancers where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeLoadBalancers' request.
describeLoadBalancers :: DescribeLoadBalancers
describeLoadBalancers = DescribeLoadBalancers
    { _dapiLoadBalancerNames = mempty
    , _dapiMarker = Nothing
    , _dapiPageSize = Nothing
    }

data DescribeLoadBalancers = DescribeLoadBalancers
    { _dapiLoadBalancerNames :: [Text]
      -- ^ A list of load balancer names associated with the account.
    , _dapiMarker :: Maybe Text
      -- ^ An optional parameter reserved for future use.
    , _dapiPageSize :: Maybe Integer
      -- ^ The size of the page returned by this call.
    } deriving (Show, Generic)

makeLenses ''DescribeLoadBalancers

instance ToQuery DescribeLoadBalancers where
    toQuery = genericToQuery def

data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse
    { _dapoLoadBalancerDescriptions :: [LoadBalancerDescription]
      -- ^ A list of load balancer description structures.
    , _dapoNextMarker :: Maybe Text
      -- ^ An optional parameter reserved for future use.
    } deriving (Show, Generic)

makeLenses ''DescribeLoadBalancersResponse

instance AWSRequest DescribeLoadBalancers where
    type Sv DescribeLoadBalancers = ELB
    type Rs DescribeLoadBalancers = DescribeLoadBalancersResponse

    request = post "DescribeLoadBalancers"
    response _ = cursorResponse $ \hs xml ->
        pure DescribeLoadBalancersResponse
            <*> xml %| "LoadBalancerDescriptions"
            <*> xml %|? "Marker"

instance AWSPager DescribeLoadBalancers where
    next rq rs = (\x -> rq { _dapiMarker = Just x })
        <$> (_dapoNextMarker rs)
