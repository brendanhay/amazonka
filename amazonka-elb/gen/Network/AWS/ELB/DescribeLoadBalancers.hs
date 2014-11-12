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
module Network.AWS.ELB.DescribeLoadBalancers
    (
    -- * Request
      DescribeAccessPointsInput
    -- ** Request constructor
    , describeAccessPointsInput
    -- ** Request lenses
    , dapiLoadBalancerNames
    , dapiMarker
    , dapiPageSize

    -- * Response
    , DescribeAccessPointsOutput
    -- ** Response constructor
    , describeAccessPointsOutput
    -- ** Response lenses
    , dapoLoadBalancerDescriptions
    , dapoNextMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data DescribeAccessPointsInput = DescribeAccessPointsInput
    { _dapiLoadBalancerNames :: [Text]
    , _dapiMarker            :: Maybe Text
    , _dapiPageSize          :: Maybe Natural
    } (Eq, Ord, Show, Generic)

-- | 'DescribeAccessPointsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dapiLoadBalancerNames' @::@ ['Text']
--
-- * 'dapiMarker' @::@ 'Maybe' 'Text'
--
-- * 'dapiPageSize' @::@ 'Maybe' 'Natural'
--
describeAccessPointsInput :: DescribeAccessPointsInput
describeAccessPointsInput = DescribeAccessPointsInput
    { _dapiLoadBalancerNames = mempty
    , _dapiMarker            = Nothing
    , _dapiPageSize          = Nothing
    }

-- | A list of load balancer names associated with the account.
dapiLoadBalancerNames :: Lens' DescribeAccessPointsInput [Text]
dapiLoadBalancerNames =
    lens _dapiLoadBalancerNames (\s a -> s { _dapiLoadBalancerNames = a })

-- | An optional parameter used for pagination of results from this call. If
-- specified, the response includes only records beyond the marker.
dapiMarker :: Lens' DescribeAccessPointsInput (Maybe Text)
dapiMarker = lens _dapiMarker (\s a -> s { _dapiMarker = a })

-- | The number of results returned in each page. The default is 400. You
-- cannot specify a page size greater than 400 or less than 1.
dapiPageSize :: Lens' DescribeAccessPointsInput (Maybe Natural)
dapiPageSize = lens _dapiPageSize (\s a -> s { _dapiPageSize = a })
instance ToQuery DescribeAccessPointsInput

instance ToPath DescribeAccessPointsInput where
    toPath = const "/"

data DescribeAccessPointsOutput = DescribeAccessPointsOutput
    { _dapoLoadBalancerDescriptions :: [LoadBalancerDescription]
    , _dapoNextMarker               :: Maybe Text
    } (Eq, Show, Generic)

-- | 'DescribeAccessPointsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dapoLoadBalancerDescriptions' @::@ ['LoadBalancerDescription']
--
-- * 'dapoNextMarker' @::@ 'Maybe' 'Text'
--
describeAccessPointsOutput :: DescribeAccessPointsOutput
describeAccessPointsOutput = DescribeAccessPointsOutput
    { _dapoLoadBalancerDescriptions = mempty
    , _dapoNextMarker               = Nothing
    }

-- | A list of load balancer description structures.
dapoLoadBalancerDescriptions :: Lens' DescribeAccessPointsOutput [LoadBalancerDescription]
dapoLoadBalancerDescriptions =
    lens _dapoLoadBalancerDescriptions
        (\s a -> s { _dapoLoadBalancerDescriptions = a })

-- | Specifies the value of next marker if the request returned more than one
-- page of results.
dapoNextMarker :: Lens' DescribeAccessPointsOutput (Maybe Text)
dapoNextMarker = lens _dapoNextMarker (\s a -> s { _dapoNextMarker = a })

instance FromXML DescribeAccessPointsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeAccessPointsOutput"

instance AWSRequest DescribeAccessPointsInput where
    type Sv DescribeAccessPointsInput = ELB
    type Rs DescribeAccessPointsInput = DescribeAccessPointsOutput

    request  = post "DescribeLoadBalancers"
    response = xmlResponse $ \h x -> DescribeAccessPointsOutput
        <$> x %| "LoadBalancerDescriptions"
        <*> x %| "NextMarker"
