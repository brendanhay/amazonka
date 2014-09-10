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
module Network.AWS.ELB
    (
    -- * Request
      DescribeLoadBalancers
    -- ** Request constructor
    , mkDescribeLoadBalancers
    -- ** Request lenses
    , dlb1LoadBalancerNames
    , dlb1Marker
    , dlb1PageSize

    -- * Response
    , DescribeLoadBalancersResponse
    -- ** Response constructor
    , mkDescribeLoadBalancersResponse
    -- ** Response lenses
    , dlbrrLoadBalancerDescriptions
    , dlbrrNextMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the DescribeLoadBalancers action.
data DescribeLoadBalancers = DescribeLoadBalancers
    { _dlb1LoadBalancerNames :: [Text]
    , _dlb1Marker :: Maybe Text
    , _dlb1PageSize :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBalancers' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerNames ::@ @[Text]@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @PageSize ::@ @Maybe Integer@
--
mkDescribeLoadBalancers :: DescribeLoadBalancers
mkDescribeLoadBalancers = DescribeLoadBalancers
    { _dlb1LoadBalancerNames = mempty
    , _dlb1Marker = Nothing
    , _dlb1PageSize = Nothing
    }

-- | A list of load balancer names associated with the account.
dlb1LoadBalancerNames :: Lens' DescribeLoadBalancers [Text]
dlb1LoadBalancerNames =
    lens _dlb1LoadBalancerNames (\s a -> s { _dlb1LoadBalancerNames = a })

-- | An optional parameter used for pagination of results from this call. If
-- specified, the response includes only records beyond the marker.
dlb1Marker :: Lens' DescribeLoadBalancers (Maybe Text)
dlb1Marker = lens _dlb1Marker (\s a -> s { _dlb1Marker = a })

-- | The number of results returned in each page. The default is 400. You cannot
-- specify a page size greater than 400 or less than 1.
dlb1PageSize :: Lens' DescribeLoadBalancers (Maybe Integer)
dlb1PageSize = lens _dlb1PageSize (\s a -> s { _dlb1PageSize = a })

instance ToQuery DescribeLoadBalancers where
    toQuery = genericQuery def

-- | The output for the DescribeLoadBalancers action.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse
    { _dlbrrLoadBalancerDescriptions :: [LoadBalancerDescription]
    , _dlbrrNextMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBalancersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerDescriptions ::@ @[LoadBalancerDescription]@
--
-- * @NextMarker ::@ @Maybe Text@
--
mkDescribeLoadBalancersResponse :: DescribeLoadBalancersResponse
mkDescribeLoadBalancersResponse = DescribeLoadBalancersResponse
    { _dlbrrLoadBalancerDescriptions = mempty
    , _dlbrrNextMarker = Nothing
    }

-- | A list of load balancer description structures.
dlbrrLoadBalancerDescriptions :: Lens' DescribeLoadBalancersResponse [LoadBalancerDescription]
dlbrrLoadBalancerDescriptions =
    lens _dlbrrLoadBalancerDescriptions
         (\s a -> s { _dlbrrLoadBalancerDescriptions = a })

-- | Specifies the value of next marker if the request returned more than one
-- page of results.
dlbrrNextMarker :: Lens' DescribeLoadBalancersResponse (Maybe Text)
dlbrrNextMarker = lens _dlbrrNextMarker (\s a -> s { _dlbrrNextMarker = a })

instance FromXML DescribeLoadBalancersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoadBalancers where
    type Sv DescribeLoadBalancers = ELB
    type Rs DescribeLoadBalancers = DescribeLoadBalancersResponse

    request = post "DescribeLoadBalancers"
    response _ = xmlResponse

instance AWSPager DescribeLoadBalancers where
    next rq rs = (\x -> rq & dlb1Marker ?~ x)
        <$> (rs ^. dlbrrNextMarker)
