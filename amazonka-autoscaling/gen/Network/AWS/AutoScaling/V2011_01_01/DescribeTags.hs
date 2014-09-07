{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeTags
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
-- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeTags
-- &AUTHPARAMS my-test-asg true 1.0 version auto-scaling-group
-- 086265fd-bf3e-11e2-85fc-fbb1EXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , mkDescribeTags
    -- ** Request lenses
    , dt1Filters
    , dt1NextToken
    , dt1MaxRecords

    -- * Response
    , DescribeTagsResponse
    -- ** Response lenses
    , dtrsTags
    , dtrsNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | 
data DescribeTags = DescribeTags
    { _dt1Filters :: [Filter]
    , _dt1NextToken :: Maybe Text
    , _dt1MaxRecords :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTags' request.
mkDescribeTags :: DescribeTags
mkDescribeTags = DescribeTags
    { _dt1Filters = mempty
    , _dt1NextToken = Nothing
    , _dt1MaxRecords = Nothing
    }

-- | The value of the filter type used to identify the tags to be returned. For
-- example, you can filter so that tags are returned according to Auto Scaling
-- group, the key and value, or whether the new tag will be applied to
-- instances launched after the tag is created (PropagateAtLaunch).
dt1Filters :: Lens' DescribeTags [Filter]
dt1Filters = lens _dt1Filters (\s a -> s { _dt1Filters = a })

-- | A string that marks the start of the next batch of returned results.
dt1NextToken :: Lens' DescribeTags (Maybe Text)
dt1NextToken = lens _dt1NextToken (\s a -> s { _dt1NextToken = a })

-- | The maximum number of records to return.
dt1MaxRecords :: Lens' DescribeTags (Maybe Integer)
dt1MaxRecords = lens _dt1MaxRecords (\s a -> s { _dt1MaxRecords = a })

instance ToQuery DescribeTags where
    toQuery = genericQuery def

-- | 
data DescribeTagsResponse = DescribeTagsResponse
    { _dtrsTags :: [TagDescription]
    , _dtrsNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | The list of tags.
dtrsTags :: Lens' DescribeTagsResponse [TagDescription]
dtrsTags = lens _dtrsTags (\s a -> s { _dtrsTags = a })

-- | A string used to mark the start of the next batch of returned results.
dtrsNextToken :: Lens' DescribeTagsResponse (Maybe Text)
dtrsNextToken = lens _dtrsNextToken (\s a -> s { _dtrsNextToken = a })

instance FromXML DescribeTagsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeTags where
    type Sv DescribeTags = AutoScaling
    type Rs DescribeTags = DescribeTagsResponse

    request = post "DescribeTags"
    response _ = xmlResponse

instance AWSPager DescribeTags where
    next rq rs = (\x -> rq & dt1NextToken ?~ x) <$> (rs ^. dtrsNextToken)

