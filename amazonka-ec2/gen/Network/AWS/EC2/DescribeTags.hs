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

-- Module      : Network.AWS.EC2.DescribeTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of the tags for your EC2 resources. For more
-- information about tags, see Tagging Your Resources in the Amazon Elastic
-- Compute Cloud User Guide.
module Network.AWS.EC2.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , describeTags
    -- ** Request lenses
    , dtDryRun
    , dtFilters
    , dtMaxResults
    , dtNextToken

    -- * Response
    , DescribeTagsResult
    -- ** Response constructor
    , describeTagsResult
    -- ** Response lenses
    , dtrNextToken
    , dtrTags
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeTags = DescribeTags
    { _dtDryRun     :: Maybe Bool
    , _dtFilters    :: [Filter]
    , _dtMaxResults :: Maybe Int
    , _dtNextToken  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dtFilters' @::@ ['Filter']
--
-- * 'dtMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dtNextToken' @::@ 'Maybe' 'Text'
--
describeTags :: DescribeTags
describeTags = DescribeTags
    { _dtDryRun     = Nothing
    , _dtFilters    = mempty
    , _dtMaxResults = Nothing
    , _dtNextToken  = Nothing
    }

dtDryRun :: Lens' DescribeTags (Maybe Bool)
dtDryRun = lens _dtDryRun (\s a -> s { _dtDryRun = a })

-- | One or more filters. key - The tag key. resource-id - The resource ID.
-- resource-type - The resource type (customer-gateway | dhcp-options |
-- image | instance | internet-gateway | network-acl | network-interface |
-- reserved-instances | route-table | security-group | snapshot |
-- spot-instances-request | subnet | volume | vpc | vpn-connection |
-- vpn-gateway). value - The tag value.
dtFilters :: Lens' DescribeTags [Filter]
dtFilters = lens _dtFilters (\s a -> s { _dtFilters = a })

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the next
-- set of results. If the value is greater than 1000, we return only 1000
-- items.
dtMaxResults :: Lens' DescribeTags (Maybe Int)
dtMaxResults = lens _dtMaxResults (\s a -> s { _dtMaxResults = a })

-- | The token for the next set of items to return. (You received this token
-- from a prior call.).
dtNextToken :: Lens' DescribeTags (Maybe Text)
dtNextToken = lens _dtNextToken (\s a -> s { _dtNextToken = a })

instance ToPath DescribeTags where
    toPath = const "/"

instance ToQuery DescribeTags

data DescribeTagsResult = DescribeTagsResult
    { _dtrNextToken :: Maybe Text
    , _dtrTags      :: [TagDescription]
    } deriving (Eq, Show, Generic)

-- | 'DescribeTagsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dtrTags' @::@ ['TagDescription']
--
describeTagsResult :: DescribeTagsResult
describeTagsResult = DescribeTagsResult
    { _dtrTags      = mempty
    , _dtrNextToken = Nothing
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dtrNextToken :: Lens' DescribeTagsResult (Maybe Text)
dtrNextToken = lens _dtrNextToken (\s a -> s { _dtrNextToken = a })

-- | A list of tags.
dtrTags :: Lens' DescribeTagsResult [TagDescription]
dtrTags = lens _dtrTags (\s a -> s { _dtrTags = a })

instance AWSRequest DescribeTags where
    type Sv DescribeTags = EC2
    type Rs DescribeTags = DescribeTagsResult

    request  = post "DescribeTags"
    response = const . xmlResponse $ \h x -> DescribeTagsResult
        <$> x %| "nextToken"
        <*> x %| "tagSet"
