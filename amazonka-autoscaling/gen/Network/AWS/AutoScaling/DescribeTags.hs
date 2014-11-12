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

-- Module      : Network.AWS.AutoScaling.DescribeTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the Auto Scaling group tags. You can use filters to limit results
-- when describing tags. For example, you can query for tags of a particular
-- Auto Scaling group. You can specify multiple values for a filter. A tag
-- must match at least one of the specified values for it to be included in
-- the results. You can also specify multiple filters. The result includes
-- information for a particular tag only if it matches all your filters. If
-- there's no match, no special message is returned.
module Network.AWS.AutoScaling.DescribeTags
    (
    -- * Request
      DescribeTagsType
    -- ** Request constructor
    , describeTagsType
    -- ** Request lenses
    , dttFilters
    , dttMaxRecords
    , dttNextToken

    -- * Response
    , TagsType
    -- ** Response constructor
    , tagsType
    -- ** Response lenses
    , ttNextToken
    , ttTags
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeTagsType = DescribeTagsType
    { _dttFilters    :: [Filter]
    , _dttMaxRecords :: Maybe Int
    , _dttNextToken  :: Maybe Text
    } (Eq, Show, Generic)

-- | 'DescribeTagsType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dttFilters' @::@ ['Filter']
--
-- * 'dttMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dttNextToken' @::@ 'Maybe' 'Text'
--
describeTagsType :: DescribeTagsType
describeTagsType = DescribeTagsType
    { _dttFilters    = mempty
    , _dttNextToken  = Nothing
    , _dttMaxRecords = Nothing
    }

-- | The value of the filter type used to identify the tags to be returned.
-- For example, you can filter so that tags are returned according to Auto
-- Scaling group, the key and value, or whether the new tag will be applied
-- to instances launched after the tag is created (PropagateAtLaunch).
dttFilters :: Lens' DescribeTagsType [Filter]
dttFilters = lens _dttFilters (\s a -> s { _dttFilters = a })

-- | The maximum number of records to return.
dttMaxRecords :: Lens' DescribeTagsType (Maybe Int)
dttMaxRecords = lens _dttMaxRecords (\s a -> s { _dttMaxRecords = a })

-- | A string that marks the start of the next batch of returned results.
dttNextToken :: Lens' DescribeTagsType (Maybe Text)
dttNextToken = lens _dttNextToken (\s a -> s { _dttNextToken = a })
instance ToQuery DescribeTagsType

instance ToPath DescribeTagsType where
    toPath = const "/"

data TagsType = TagsType
    { _ttNextToken :: Maybe Text
    , _ttTags      :: [TagDescription]
    } (Eq, Show, Generic)

-- | 'TagsType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ttNextToken' @::@ 'Maybe' 'Text'
--
-- * 'ttTags' @::@ ['TagDescription']
--
tagsType :: TagsType
tagsType = TagsType
    { _ttTags      = mempty
    , _ttNextToken = Nothing
    }

-- | A string used to mark the start of the next batch of returned results.
ttNextToken :: Lens' TagsType (Maybe Text)
ttNextToken = lens _ttNextToken (\s a -> s { _ttNextToken = a })

-- | The list of tags.
ttTags :: Lens' TagsType [TagDescription]
ttTags = lens _ttTags (\s a -> s { _ttTags = a })

instance FromXML TagsType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagsType"

instance AWSRequest DescribeTagsType where
    type Sv DescribeTagsType = AutoScaling
    type Rs DescribeTagsType = TagsType

    request  = post "DescribeTags"
    response = xmlResponse $ \h x -> TagsType
        <$> x %| "NextToken"
        <*> x %| "Tags"
