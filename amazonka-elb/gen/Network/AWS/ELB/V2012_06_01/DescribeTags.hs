{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DescribeTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the tags associated with one or more load balancers.
-- https://elasticloadbalancing.amazonaws.com//?Action=DescribeTags
-- &LoadBalancerNames.member.1=my-test-loadbalancer &Version=2012-06-01
-- &AUTHPARAMS my-test-project project test environment my-test-loadbalancer
-- 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
module Network.AWS.ELB.V2012_06_01.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , mkDescribeTags
    -- ** Request lenses
    , dtLoadBalancerNames

    -- * Response
    , DescribeTagsResponse
    -- ** Response lenses
    , dtrsTagDescriptions
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | The input for the DescribeTags action.
newtype DescribeTags = DescribeTags
    { _dtLoadBalancerNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTags' request.
mkDescribeTags :: [Text] -- ^ 'dtLoadBalancerNames'
               -> DescribeTags
mkDescribeTags p1 = DescribeTags
    { _dtLoadBalancerNames = p1
    }
{-# INLINE mkDescribeTags #-}

-- | The names of the load balancers.
dtLoadBalancerNames :: Lens' DescribeTags [Text]
dtLoadBalancerNames =
    lens _dtLoadBalancerNames (\s a -> s { _dtLoadBalancerNames = a })
{-# INLINE dtLoadBalancerNames #-}

instance ToQuery DescribeTags where
    toQuery = genericQuery def

-- | The output for the DescribeTags action.
newtype DescribeTagsResponse = DescribeTagsResponse
    { _dtrsTagDescriptions :: [TagDescription]
    } deriving (Show, Generic)

-- | A list of tag description structures.
dtrsTagDescriptions :: Lens' DescribeTagsResponse [TagDescription]
dtrsTagDescriptions =
    lens _dtrsTagDescriptions (\s a -> s { _dtrsTagDescriptions = a })
{-# INLINE dtrsTagDescriptions #-}

instance FromXML DescribeTagsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeTags where
    type Sv DescribeTags = ELB
    type Rs DescribeTags = DescribeTagsResponse

    request = post "DescribeTags"
    response _ = xmlResponse
