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

-- Module      : Network.AWS.CloudWatchLogs.DescribeMetricFilters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all the metrics filters associated with the specified log group.
-- The list returned in the response is ASCII-sorted by filter name. By
-- default, this operation returns up to 50 metric filters. If there are more
-- metric filters to list, the response would contain a nextToken value in the
-- response body. You can also limit the number of metric filters returned in
-- the response by specifying the limit parameter in the request.
module Network.AWS.CloudWatchLogs.DescribeMetricFilters
    (
    -- * Request
      DescribeMetricFilters
    -- ** Request constructor
    , describeMetricFilters
    -- ** Request lenses
    , dmfFilterNamePrefix
    , dmfLimit
    , dmfLogGroupName
    , dmfNextToken

    -- * Response
    , DescribeMetricFiltersResponse
    -- ** Response constructor
    , describeMetricFiltersResponse
    -- ** Response lenses
    , dmfrMetricFilters
    , dmfrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudWatchLogs.Types

data DescribeMetricFilters = DescribeMetricFilters
    { _dmfFilterNamePrefix :: Maybe Text
    , _dmfLimit            :: Maybe Natural
    , _dmfLogGroupName     :: Text
    , _dmfNextToken        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeMetricFilters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmfFilterNamePrefix' @::@ 'Maybe' 'Text'
--
-- * 'dmfLimit' @::@ 'Maybe' 'Natural'
--
-- * 'dmfLogGroupName' @::@ 'Text'
--
-- * 'dmfNextToken' @::@ 'Maybe' 'Text'
--
describeMetricFilters :: Text -- ^ 'dmfLogGroupName'
                      -> DescribeMetricFilters
describeMetricFilters p1 = DescribeMetricFilters
    { _dmfLogGroupName     = p1
    , _dmfFilterNamePrefix = Nothing
    , _dmfNextToken        = Nothing
    , _dmfLimit            = Nothing
    }

dmfFilterNamePrefix :: Lens' DescribeMetricFilters (Maybe Text)
dmfFilterNamePrefix =
    lens _dmfFilterNamePrefix (\s a -> s { _dmfFilterNamePrefix = a })

-- | The maximum number of items returned in the response. If you don't
-- specify a value, the request would return up to 50 items.
dmfLimit :: Lens' DescribeMetricFilters (Maybe Natural)
dmfLimit = lens _dmfLimit (\s a -> s { _dmfLimit = a })

dmfLogGroupName :: Lens' DescribeMetricFilters Text
dmfLogGroupName = lens _dmfLogGroupName (\s a -> s { _dmfLogGroupName = a })

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- DescribeMetricFilters request.
dmfNextToken :: Lens' DescribeMetricFilters (Maybe Text)
dmfNextToken = lens _dmfNextToken (\s a -> s { _dmfNextToken = a })

instance ToPath DescribeMetricFilters where
    toPath = const "/"

instance ToQuery DescribeMetricFilters where
    toQuery = const mempty

instance ToHeaders DescribeMetricFilters

instance ToBody DescribeMetricFilters where
    toBody = toBody . encode . _dmfLogGroupName

data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse
    { _dmfrMetricFilters :: [MetricFilter]
    , _dmfrNextToken     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeMetricFiltersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmfrMetricFilters' @::@ ['MetricFilter']
--
-- * 'dmfrNextToken' @::@ 'Maybe' 'Text'
--
describeMetricFiltersResponse :: DescribeMetricFiltersResponse
describeMetricFiltersResponse = DescribeMetricFiltersResponse
    { _dmfrMetricFilters = mempty
    , _dmfrNextToken     = Nothing
    }

dmfrMetricFilters :: Lens' DescribeMetricFiltersResponse [MetricFilter]
dmfrMetricFilters =
    lens _dmfrMetricFilters (\s a -> s { _dmfrMetricFilters = a })

dmfrNextToken :: Lens' DescribeMetricFiltersResponse (Maybe Text)
dmfrNextToken = lens _dmfrNextToken (\s a -> s { _dmfrNextToken = a })

-- FromJSON

instance AWSRequest DescribeMetricFilters where
    type Sv DescribeMetricFilters = CloudWatchLogs
    type Rs DescribeMetricFilters = DescribeMetricFiltersResponse

    request  = post'
    response = jsonResponse $ \h o -> DescribeMetricFiltersResponse
        <$> o .: "metricFilters"
        <*> o .: "nextToken"
