{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.ListMetrics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of valid metrics stored for the AWS account owner. Returned
-- metrics can be used with GetMetricStatistics to obtain statistical data for
-- a given metric. Up to 500 results are returned for any one call. To
-- retrieve further results, use returned NextToken values with subsequent
-- ListMetrics operations. If you create a metric with the PutMetricData
-- action, allow up to fifteen minutes for the metric to appear in calls to
-- the ListMetrics action.
module Network.AWS.CloudWatch.V2010_08_01.ListMetrics
    (
    -- * Request
      ListMetrics
    -- ** Request constructor
    , mkListMetricsInput
    -- ** Request lenses
    , lmiNamespace
    , lmiMetricName
    , lmiDimensions
    , lmiNextToken

    -- * Response
    , ListMetricsResponse
    -- ** Response lenses
    , lmoMetrics
    , lmoNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListMetrics' request.
mkListMetricsInput :: ListMetrics
mkListMetricsInput = ListMetrics
    { _lmiNamespace = Nothing
    , _lmiMetricName = Nothing
    , _lmiDimensions = mempty
    , _lmiNextToken = Nothing
    }
{-# INLINE mkListMetricsInput #-}

data ListMetrics = ListMetrics
    { _lmiNamespace :: Maybe Text
      -- ^ The namespace to filter against.
    , _lmiMetricName :: Maybe Text
      -- ^ The name of the metric to filter against.
    , _lmiDimensions :: [DimensionFilter]
      -- ^ A list of dimensions to filter against.
    , _lmiNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    } deriving (Show, Generic)

-- | The namespace to filter against.
lmiNamespace :: Lens' ListMetrics (Maybe Text)
lmiNamespace = lens _lmiNamespace (\s a -> s { _lmiNamespace = a })
{-# INLINE lmiNamespace #-}

-- | The name of the metric to filter against.
lmiMetricName :: Lens' ListMetrics (Maybe Text)
lmiMetricName = lens _lmiMetricName (\s a -> s { _lmiMetricName = a })
{-# INLINE lmiMetricName #-}

-- | A list of dimensions to filter against.
lmiDimensions :: Lens' ListMetrics ([DimensionFilter])
lmiDimensions = lens _lmiDimensions (\s a -> s { _lmiDimensions = a })
{-# INLINE lmiDimensions #-}

-- | The token returned by a previous call to indicate that there is more data
-- available.
lmiNextToken :: Lens' ListMetrics (Maybe Text)
lmiNextToken = lens _lmiNextToken (\s a -> s { _lmiNextToken = a })
{-# INLINE lmiNextToken #-}

instance ToQuery ListMetrics where
    toQuery = genericQuery def

data ListMetricsResponse = ListMetricsResponse
    { _lmoMetrics :: [Metric]
      -- ^ A list of metrics used to generate statistics for an AWS account.
    , _lmoNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | A list of metrics used to generate statistics for an AWS account.
lmoMetrics :: Lens' ListMetricsResponse ([Metric])
lmoMetrics = lens _lmoMetrics (\s a -> s { _lmoMetrics = a })
{-# INLINE lmoMetrics #-}

-- | A string that marks the start of the next batch of returned results.
lmoNextToken :: Lens' ListMetricsResponse (Maybe Text)
lmoNextToken = lens _lmoNextToken (\s a -> s { _lmoNextToken = a })
{-# INLINE lmoNextToken #-}

instance FromXML ListMetricsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListMetrics where
    type Sv ListMetrics = CloudWatch
    type Rs ListMetrics = ListMetricsResponse

    request = post "ListMetrics"
    response _ = xmlResponse

instance AWSPager ListMetrics where
    next rq rs = (\x -> rq { _lmiNextToken = Just x })
        <$> (_lmoNextToken rs)
