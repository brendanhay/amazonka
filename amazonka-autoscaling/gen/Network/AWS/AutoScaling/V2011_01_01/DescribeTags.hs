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
    , mkDescribeTagsType
    -- ** Request lenses
    , dtuFilters
    , dtuNextToken
    , dtuMaxRecords

    -- * Response
    , DescribeTagsResponse
    -- ** Response lenses
    , ttkTags
    , ttkNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTags' request.
mkDescribeTagsType :: DescribeTags
mkDescribeTagsType = DescribeTags
    { _dtuFilters = mempty
    , _dtuNextToken = Nothing
    , _dtuMaxRecords = Nothing
    }
{-# INLINE mkDescribeTagsType #-}

data DescribeTags = DescribeTags
    { _dtuFilters :: [Filter]
      -- ^ The value of the filter type used to identify the tags to be
      -- returned. For example, you can filter so that tags are returned
      -- according to Auto Scaling group, the key and value, or whether
      -- the new tag will be applied to instances launched after the tag
      -- is created (PropagateAtLaunch).
    , _dtuNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    , _dtuMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to return.
    } deriving (Show, Generic)

-- | The value of the filter type used to identify the tags to be returned. For
-- example, you can filter so that tags are returned according to Auto Scaling
-- group, the key and value, or whether the new tag will be applied to
-- instances launched after the tag is created (PropagateAtLaunch).
dtuFilters :: Lens' DescribeTags ([Filter])
dtuFilters = lens _dtuFilters (\s a -> s { _dtuFilters = a })
{-# INLINE dtuFilters #-}

-- | A string that marks the start of the next batch of returned results.
dtuNextToken :: Lens' DescribeTags (Maybe Text)
dtuNextToken = lens _dtuNextToken (\s a -> s { _dtuNextToken = a })
{-# INLINE dtuNextToken #-}

-- | The maximum number of records to return.
dtuMaxRecords :: Lens' DescribeTags (Maybe Integer)
dtuMaxRecords = lens _dtuMaxRecords (\s a -> s { _dtuMaxRecords = a })
{-# INLINE dtuMaxRecords #-}

instance ToQuery DescribeTags where
    toQuery = genericQuery def

data DescribeTagsResponse = DescribeTagsResponse
    { _ttkTags :: [TagDescription]
      -- ^ The list of tags.
    , _ttkNextToken :: Maybe Text
      -- ^ A string used to mark the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | The list of tags.
ttkTags :: Lens' DescribeTagsResponse ([TagDescription])
ttkTags = lens _ttkTags (\s a -> s { _ttkTags = a })
{-# INLINE ttkTags #-}

-- | A string used to mark the start of the next batch of returned results.
ttkNextToken :: Lens' DescribeTagsResponse (Maybe Text)
ttkNextToken = lens _ttkNextToken (\s a -> s { _ttkNextToken = a })
{-# INLINE ttkNextToken #-}

instance FromXML DescribeTagsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeTags where
    type Sv DescribeTags = AutoScaling
    type Rs DescribeTags = DescribeTagsResponse

    request = post "DescribeTags"
    response _ = xmlResponse

instance AWSPager DescribeTags where
    next rq rs = (\x -> rq { _dtuNextToken = Just x })
        <$> (_ttkNextToken rs)
