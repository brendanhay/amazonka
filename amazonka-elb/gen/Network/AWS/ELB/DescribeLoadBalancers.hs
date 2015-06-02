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

-- Module      : Network.AWS.ELB.DescribeLoadBalancers
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

-- | Describes the specified the load balancers. If no load balancers are
-- specified, the call describes all of your load balancers.
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
    { _dlbLoadBalancerNames :: List "member" Text
    , _dlbMarker            :: Maybe Text
    , _dlbPageSize          :: Maybe Nat
    } deriving (Eq, Ord, Read, Show)

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

-- | The names of the load balancers.
dlbLoadBalancerNames :: Lens' DescribeLoadBalancers [Text]
dlbLoadBalancerNames =
    lens _dlbLoadBalancerNames (\s a -> s { _dlbLoadBalancerNames = a })
        . _List

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
dlbMarker :: Lens' DescribeLoadBalancers (Maybe Text)
dlbMarker = lens _dlbMarker (\s a -> s { _dlbMarker = a })

-- | The maximum number of results to return with this call (a number from 1 to
-- 400). The default is 400.
dlbPageSize :: Lens' DescribeLoadBalancers (Maybe Natural)
dlbPageSize = lens _dlbPageSize (\s a -> s { _dlbPageSize = a }) . mapping _Nat

data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse
    { _dlbrLoadBalancerDescriptions :: List "member" LoadBalancerDescription
    , _dlbrNextMarker               :: Maybe Text
    } deriving (Eq, Read, Show)

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

-- | Information about the load balancers.
dlbrLoadBalancerDescriptions :: Lens' DescribeLoadBalancersResponse [LoadBalancerDescription]
dlbrLoadBalancerDescriptions =
    lens _dlbrLoadBalancerDescriptions
        (\s a -> s { _dlbrLoadBalancerDescriptions = a })
            . _List

-- | The marker to use when requesting the next set of results. If there are no
-- additional results, the string is empty.
dlbrNextMarker :: Lens' DescribeLoadBalancersResponse (Maybe Text)
dlbrNextMarker = lens _dlbrNextMarker (\s a -> s { _dlbrNextMarker = a })

instance ToPath DescribeLoadBalancers where
    toPath = const "/"

instance ToQuery DescribeLoadBalancers where
    toQuery DescribeLoadBalancers{..} = mconcat
        [ "LoadBalancerNames" =? _dlbLoadBalancerNames
        , "Marker"            =? _dlbMarker
        , "PageSize"          =? _dlbPageSize
        ]

instance ToHeaders DescribeLoadBalancers

instance AWSRequest DescribeLoadBalancers where
    type Sv DescribeLoadBalancers = ELB
    type Rs DescribeLoadBalancers = DescribeLoadBalancersResponse

    request  = post "DescribeLoadBalancers"
    response = xmlResponse

instance FromXML DescribeLoadBalancersResponse where
    parseXML = withElement "DescribeLoadBalancersResult" $ \x -> DescribeLoadBalancersResponse
        <$> x .@? "LoadBalancerDescriptions" .!@ mempty
        <*> x .@? "NextMarker"

instance AWSPager DescribeLoadBalancers where
    page rq rs
        | stop (rs ^. dlbrNextMarker) = Nothing
        | otherwise = (\x -> rq & dlbMarker ?~ x)
            <$> (rs ^. dlbrNextMarker)
