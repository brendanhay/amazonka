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
    , mkDescribeTagsInput
    -- ** Request lenses
    , dtiLoadBalancerNames

    -- * Response
    , DescribeTagsResponse
    -- ** Response lenses
    , dtoTagDescriptions
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTags' request.
mkDescribeTagsInput :: [Text] -- ^ 'dtiLoadBalancerNames'
                    -> DescribeTags
mkDescribeTagsInput p1 = DescribeTags
    { _dtiLoadBalancerNames = p1
    }
{-# INLINE mkDescribeTagsInput #-}

newtype DescribeTags = DescribeTags
    { _dtiLoadBalancerNames :: [Text]
      -- ^ The names of the load balancers.
    } deriving (Show, Generic)

-- | The names of the load balancers.
dtiLoadBalancerNames :: Lens' DescribeTags ([Text])
dtiLoadBalancerNames = lens _dtiLoadBalancerNames (\s a -> s { _dtiLoadBalancerNames = a })
{-# INLINE dtiLoadBalancerNames #-}

instance ToQuery DescribeTags where
    toQuery = genericQuery def

newtype DescribeTagsResponse = DescribeTagsResponse
    { _dtoTagDescriptions :: [TagDescription]
      -- ^ A list of tag description structures.
    } deriving (Show, Generic)

-- | A list of tag description structures.
dtoTagDescriptions :: Lens' DescribeTagsResponse ([TagDescription])
dtoTagDescriptions = lens _dtoTagDescriptions (\s a -> s { _dtoTagDescriptions = a })
{-# INLINE dtoTagDescriptions #-}

instance FromXML DescribeTagsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeTags where
    type Sv DescribeTags = ELB
    type Rs DescribeTags = DescribeTagsResponse

    request = post "DescribeTags"
    response _ = xmlResponse
