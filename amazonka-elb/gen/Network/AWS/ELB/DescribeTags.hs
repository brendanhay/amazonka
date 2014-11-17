{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.DescribeTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the tags associated with one or more load balancers.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeTags.html>
module Network.AWS.ELB.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , describeTags
    -- ** Request lenses
    , dtLoadBalancerNames

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , describeTagsResponse
    -- ** Response lenses
    , dtrTagDescriptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

newtype DescribeTags = DescribeTags
    { _dtLoadBalancerNames :: List1 Text
    } deriving (Eq, Ord, Show, Generic, Semigroup)

-- | 'DescribeTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtLoadBalancerNames' @::@ 'NonEmpty' 'Text'
--
describeTags :: NonEmpty Text -- ^ 'dtLoadBalancerNames'
             -> DescribeTags
describeTags p1 = DescribeTags
    { _dtLoadBalancerNames = withIso _List1 (const id) p1
    }

-- | The names of the load balancers.
dtLoadBalancerNames :: Lens' DescribeTags (NonEmpty Text)
dtLoadBalancerNames =
    lens _dtLoadBalancerNames (\s a -> s { _dtLoadBalancerNames = a })
        . _List1

newtype DescribeTagsResponse = DescribeTagsResponse
    { _dtrTagDescriptions :: [TagDescription]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeTagsResponse where
    type Item DescribeTagsResponse = TagDescription

    fromList = DescribeTagsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dtrTagDescriptions

-- | 'DescribeTagsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTagDescriptions' @::@ ['TagDescription']
--
describeTagsResponse :: DescribeTagsResponse
describeTagsResponse = DescribeTagsResponse
    { _dtrTagDescriptions = mempty
    }

-- | A list of tag description structures.
dtrTagDescriptions :: Lens' DescribeTagsResponse [TagDescription]
dtrTagDescriptions =
    lens _dtrTagDescriptions (\s a -> s { _dtrTagDescriptions = a })

instance ToPath DescribeTags where
    toPath = const "/"

instance ToQuery DescribeTags

instance ToHeaders DescribeTags

instance AWSRequest DescribeTags where
    type Sv DescribeTags = ELB
    type Rs DescribeTags = DescribeTagsResponse

    request  = post "DescribeTags"
    response = xmlResponse

instance FromXML DescribeTagsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeTagsResponse"
