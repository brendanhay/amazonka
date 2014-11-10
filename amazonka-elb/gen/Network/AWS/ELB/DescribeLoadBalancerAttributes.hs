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

-- Module      : Network.AWS.ELB.DescribeLoadBalancerAttributes
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
module Network.AWS.ELB.DescribeLoadBalancerAttributes
    (
    -- * Request
      DescribeLoadBalancerAttributesInput
    -- ** Request constructor
    , describeLoadBalancerAttributes
    -- ** Request lenses
    , dlbaiLoadBalancerName

    -- * Response
    , DescribeLoadBalancerAttributesOutput
    -- ** Response constructor
    , describeLoadBalancerAttributesResponse
    -- ** Response lenses
    , dlbaoLoadBalancerAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

newtype DescribeLoadBalancerAttributesInput = DescribeLoadBalancerAttributesInput
    { _dlbaiLoadBalancerName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DescribeLoadBalancerAttributesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbaiLoadBalancerName' @::@ 'Text'
--
describeLoadBalancerAttributes :: Text -- ^ 'dlbaiLoadBalancerName'
                               -> DescribeLoadBalancerAttributesInput
describeLoadBalancerAttributes p1 = DescribeLoadBalancerAttributesInput
    { _dlbaiLoadBalancerName = p1
    }

-- | The name of the load balancer.
dlbaiLoadBalancerName :: Lens' DescribeLoadBalancerAttributesInput Text
dlbaiLoadBalancerName =
    lens _dlbaiLoadBalancerName (\s a -> s { _dlbaiLoadBalancerName = a })

instance ToPath DescribeLoadBalancerAttributesInput where
    toPath = const "/"

instance ToQuery DescribeLoadBalancerAttributesInput

newtype DescribeLoadBalancerAttributesOutput = DescribeLoadBalancerAttributesOutput
    { _dlbaoLoadBalancerAttributes :: Maybe LoadBalancerAttributes
    } deriving (Eq, Show, Generic)

-- | 'DescribeLoadBalancerAttributesOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbaoLoadBalancerAttributes' @::@ 'Maybe' 'LoadBalancerAttributes'
--
describeLoadBalancerAttributesResponse :: DescribeLoadBalancerAttributesOutput
describeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesOutput
    { _dlbaoLoadBalancerAttributes = Nothing
    }

-- | The load balancer attributes structure.
dlbaoLoadBalancerAttributes :: Lens' DescribeLoadBalancerAttributesOutput (Maybe LoadBalancerAttributes)
dlbaoLoadBalancerAttributes =
    lens _dlbaoLoadBalancerAttributes
        (\s a -> s { _dlbaoLoadBalancerAttributes = a })

instance AWSRequest DescribeLoadBalancerAttributesInput where
    type Sv DescribeLoadBalancerAttributesInput = ELB
    type Rs DescribeLoadBalancerAttributesInput = DescribeLoadBalancerAttributesOutput

    request  = post "DescribeLoadBalancerAttributes"
    response = xmlResponse $ \h x -> DescribeLoadBalancerAttributesOutput
        <$> x %| "LoadBalancerAttributes"
