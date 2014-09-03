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
    , describeTags
    -- ** Request lenses
    , dtuFilters
    , dtuMaxRecords
    , dtuNextToken

    -- * Response
    , DescribeTagsResponse
    -- ** Response lenses
    , ttkTags
    , ttkNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeTags' request.
describeTags :: DescribeTags
describeTags = DescribeTags
    { _dtuFilters = mempty
    , _dtuMaxRecords = Nothing
    , _dtuNextToken = Nothing
    }

data DescribeTags = DescribeTags
    { _dtuFilters :: [Filter]
      -- ^ The value of the filter type used to identify the tags to be
      -- returned. For example, you can filter so that tags are returned
      -- according to Auto Scaling group, the key and value, or whether
      -- the new tag will be applied to instances launched after the tag
      -- is created (PropagateAtLaunch).
    , _dtuMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to return.
    , _dtuNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | The value of the filter type used to identify the tags to be returned. For
-- example, you can filter so that tags are returned according to Auto Scaling
-- group, the key and value, or whether the new tag will be applied to
-- instances launched after the tag is created (PropagateAtLaunch).
dtuFilters
    :: Functor f
    => ([Filter]
    -> f ([Filter]))
    -> DescribeTags
    -> f DescribeTags
dtuFilters f x =
    (\y -> x { _dtuFilters = y })
       <$> f (_dtuFilters x)
{-# INLINE dtuFilters #-}

-- | The maximum number of records to return.
dtuMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeTags
    -> f DescribeTags
dtuMaxRecords f x =
    (\y -> x { _dtuMaxRecords = y })
       <$> f (_dtuMaxRecords x)
{-# INLINE dtuMaxRecords #-}

-- | A string that marks the start of the next batch of returned results.
dtuNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeTags
    -> f DescribeTags
dtuNextToken f x =
    (\y -> x { _dtuNextToken = y })
       <$> f (_dtuNextToken x)
{-# INLINE dtuNextToken #-}

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
ttkTags
    :: Functor f
    => ([TagDescription]
    -> f ([TagDescription]))
    -> DescribeTagsResponse
    -> f DescribeTagsResponse
ttkTags f x =
    (\y -> x { _ttkTags = y })
       <$> f (_ttkTags x)
{-# INLINE ttkTags #-}

-- | A string used to mark the start of the next batch of returned results.
ttkNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeTagsResponse
    -> f DescribeTagsResponse
ttkNextToken f x =
    (\y -> x { _ttkNextToken = y })
       <$> f (_ttkNextToken x)
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
