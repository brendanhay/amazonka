{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.DescribeLoadBalancers
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
-- that was used to create the load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancers.html>
module Network.AWS.ELB.DescribeLoadBalancers
    (
    -- * Request
      DescribeLoadBalancers
    -- ** Request constructor
    , describeLoadBalancers
    -- ** Request lenses
    , dlbLoadBalancerNames
    , dlbMarker
    , dlbPageSize

    -- * Response
    , DescribeLoadBalancersResponse
    -- ** Response constructor
    , describeLoadBalancersResponse
    -- ** Response lenses
    , dlbrLoadBalancerDescriptions
    , dlbrNextMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data DescribeLoadBalancers = DescribeLoadBalancers
    { _dlbLoadBalancerNames :: [Text]
    , _dlbMarker            :: Maybe Text
    , _dlbPageSize          :: Maybe Nat
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeLoadBalancers' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbLoadBalancerNames' @::@ ['Text']
--
-- * 'dlbMarker' @::@ 'Maybe' 'Text'
--
-- * 'dlbPageSize' @::@ 'Maybe' 'Natural'
--
describeLoadBalancers :: DescribeLoadBalancers
describeLoadBalancers = DescribeLoadBalancers
    { _dlbLoadBalancerNames = mempty
    , _dlbMarker            = Nothing
    , _dlbPageSize          = Nothing
    }

-- | A list of load balancer names associated with the account.
dlbLoadBalancerNames :: Lens' DescribeLoadBalancers [Text]
dlbLoadBalancerNames =
    lens _dlbLoadBalancerNames (\s a -> s { _dlbLoadBalancerNames = a })

-- | An optional parameter used for pagination of results from this call. If
-- specified, the response includes only records beyond the marker.
dlbMarker :: Lens' DescribeLoadBalancers (Maybe Text)
dlbMarker = lens _dlbMarker (\s a -> s { _dlbMarker = a })

-- | The number of results returned in each page. The default is 400. You
-- cannot specify a page size greater than 400 or less than 1.
dlbPageSize :: Lens' DescribeLoadBalancers (Maybe Natural)
dlbPageSize = lens _dlbPageSize (\s a -> s { _dlbPageSize = a })
    . mapping _Nat

data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse
    { _dlbrLoadBalancerDescriptions :: [LoadBalancerDescription]
    , _dlbrNextMarker               :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeLoadBalancersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbrLoadBalancerDescriptions' @::@ ['LoadBalancerDescription']
--
-- * 'dlbrNextMarker' @::@ 'Maybe' 'Text'
--
describeLoadBalancersResponse :: DescribeLoadBalancersResponse
describeLoadBalancersResponse = DescribeLoadBalancersResponse
    { _dlbrLoadBalancerDescriptions = mempty
    , _dlbrNextMarker               = Nothing
    }

-- | A list of load balancer description structures.
dlbrLoadBalancerDescriptions :: Lens' DescribeLoadBalancersResponse [LoadBalancerDescription]
dlbrLoadBalancerDescriptions =
    lens _dlbrLoadBalancerDescriptions
        (\s a -> s { _dlbrLoadBalancerDescriptions = a })

-- | Specifies the value of next marker if the request returned more than one
-- page of results.
dlbrNextMarker :: Lens' DescribeLoadBalancersResponse (Maybe Text)
dlbrNextMarker = lens _dlbrNextMarker (\s a -> s { _dlbrNextMarker = a })

instance ToPath DescribeLoadBalancers where
    toPath = const "/"

instance ToQuery DescribeLoadBalancers

instance ToHeaders DescribeLoadBalancers

instance AWSRequest DescribeLoadBalancers where
    type Sv DescribeLoadBalancers = ELB
    type Rs DescribeLoadBalancers = DescribeLoadBalancersResponse

    request  = post "DescribeLoadBalancers"
    response = xmlResponse

instance FromXML DescribeLoadBalancersResponse where
    parseXML c = DescribeLoadBalancersResponse
        <$> c .: "LoadBalancerDescriptions"
        <*> c .:? "NextMarker"
