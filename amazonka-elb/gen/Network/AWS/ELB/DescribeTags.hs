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
module Network.AWS.ELB.DescribeTags
    (
    -- * Request
      DescribeTagsInput
    -- ** Request constructor
    , describeTagsInput
    -- ** Request lenses
    , dtiLoadBalancerNames

    -- * Response
    , DescribeTagsOutput
    -- ** Response constructor
    , describeTagsOutput
    -- ** Response lenses
    , dtoTagDescriptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

newtype DescribeTagsInput = DescribeTagsInput
    { _dtiLoadBalancerNames :: List1 Text
    } deriving (Eq, Ord, Show, Generic, Semigroup, IsString)

-- | 'DescribeTagsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtiLoadBalancerNames' @::@ 'NonEmpty' 'Text'
--
describeTagsInput :: NonEmpty Text -- ^ 'dtiLoadBalancerNames'
                  -> DescribeTagsInput
describeTagsInput p1 = DescribeTagsInput
    { _dtiLoadBalancerNames = withIso _List1 (const id) p1
    }

-- | The names of the load balancers.
dtiLoadBalancerNames :: Lens' DescribeTagsInput (NonEmpty Text)
dtiLoadBalancerNames =
    lens _dtiLoadBalancerNames (\s a -> s { _dtiLoadBalancerNames = a })
        . _List1

instance ToQuery DescribeTagsInput

instance ToPath DescribeTagsInput where
    toPath = const "/"

newtype DescribeTagsOutput = DescribeTagsOutput
    { _dtoTagDescriptions :: [TagDescription]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeTagsOutput where
    type Item DescribeTagsOutput = TagDescription

    fromList = DescribeTagsOutput . fromList
    toList   = toList . _dtoTagDescriptions

-- | 'DescribeTagsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtoTagDescriptions' @::@ ['TagDescription']
--
describeTagsOutput :: DescribeTagsOutput
describeTagsOutput = DescribeTagsOutput
    { _dtoTagDescriptions = mempty
    }

-- | A list of tag description structures.
dtoTagDescriptions :: Lens' DescribeTagsOutput [TagDescription]
dtoTagDescriptions =
    lens _dtoTagDescriptions (\s a -> s { _dtoTagDescriptions = a })

instance FromXML DescribeTagsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeTagsOutput"

instance AWSRequest DescribeTagsInput where
    type Sv DescribeTagsInput = ELB
    type Rs DescribeTagsInput = DescribeTagsOutput

    request  = post "DescribeTags"
    response = xmlResponse $ \h x -> DescribeTagsOutput
        <$> x %| "TagDescriptions"
