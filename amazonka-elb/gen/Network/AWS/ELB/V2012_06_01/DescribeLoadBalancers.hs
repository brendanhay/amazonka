{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.ELB.V2012_06_01.DescribeLoadBalancers
    (
    -- * Request
      DescribeLoadBalancers
    -- ** Request constructor
    , mkDescribeAccessPointsInput
    -- ** Request lenses
    , dapjLoadBalancerNames
    , dapjMarker
    , dapjPageSize

    -- * Response
    , DescribeLoadBalancersResponse
    -- ** Response lenses
    , dappLoadBalancerDescriptions
    , dappNextMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBalancers' request.
mkDescribeAccessPointsInput :: DescribeLoadBalancers
mkDescribeAccessPointsInput = DescribeLoadBalancers
    { _dapjLoadBalancerNames = mempty
    , _dapjMarker = Nothing
    , _dapjPageSize = Nothing
    }
{-# INLINE mkDescribeAccessPointsInput #-}

data DescribeLoadBalancers = DescribeLoadBalancers
    { _dapjLoadBalancerNames :: [Text]
      -- ^ A list of load balancer names associated with the account.
    , _dapjMarker :: Maybe Text
      -- ^ An optional parameter used for pagination of results from this
      -- call. If specified, the response includes only records beyond the
      -- marker.
    , _dapjPageSize :: Maybe Integer
      -- ^ The number of results returned in each page. The default is 400.
      -- You cannot specify a page size greater than 400 or less than 1.
    } deriving (Show, Generic)

-- | A list of load balancer names associated with the account.
dapjLoadBalancerNames :: Lens' DescribeLoadBalancers ([Text])
dapjLoadBalancerNames = lens _dapjLoadBalancerNames (\s a -> s { _dapjLoadBalancerNames = a })
{-# INLINE dapjLoadBalancerNames #-}

-- | An optional parameter used for pagination of results from this call. If
-- specified, the response includes only records beyond the marker.
dapjMarker :: Lens' DescribeLoadBalancers (Maybe Text)
dapjMarker = lens _dapjMarker (\s a -> s { _dapjMarker = a })
{-# INLINE dapjMarker #-}

-- | The number of results returned in each page. The default is 400. You cannot
-- specify a page size greater than 400 or less than 1.
dapjPageSize :: Lens' DescribeLoadBalancers (Maybe Integer)
dapjPageSize = lens _dapjPageSize (\s a -> s { _dapjPageSize = a })
{-# INLINE dapjPageSize #-}

instance ToQuery DescribeLoadBalancers where
    toQuery = genericQuery def

data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse
    { _dappLoadBalancerDescriptions :: [LoadBalancerDescription]
      -- ^ A list of load balancer description structures.
    , _dappNextMarker :: Maybe Text
      -- ^ Specifies the value of next marker if the request returned more
      -- than one page of results.
    } deriving (Show, Generic)

-- | A list of load balancer description structures.
dappLoadBalancerDescriptions :: Lens' DescribeLoadBalancersResponse ([LoadBalancerDescription])
dappLoadBalancerDescriptions = lens _dappLoadBalancerDescriptions (\s a -> s { _dappLoadBalancerDescriptions = a })
{-# INLINE dappLoadBalancerDescriptions #-}

-- | Specifies the value of next marker if the request returned more than one
-- page of results.
dappNextMarker :: Lens' DescribeLoadBalancersResponse (Maybe Text)
dappNextMarker = lens _dappNextMarker (\s a -> s { _dappNextMarker = a })
{-# INLINE dappNextMarker #-}

instance FromXML DescribeLoadBalancersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoadBalancers where
    type Sv DescribeLoadBalancers = ELB
    type Rs DescribeLoadBalancers = DescribeLoadBalancersResponse

    request = post "DescribeLoadBalancers"
    response _ = xmlResponse

instance AWSPager DescribeLoadBalancers where
    next rq rs = (\x -> rq { _dapjMarker = Just x })
        <$> (_dappNextMarker rs)
