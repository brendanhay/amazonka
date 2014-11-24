{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.ListMetrics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of valid metrics stored for the AWS account owner. Returned
-- metrics can be used with GetMetricStatistics> to obtain statistical data
-- for a given metric.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html>
module Network.AWS.CloudWatch.ListMetrics
    (
    -- * Request
      ListMetrics
    -- ** Request constructor
    , listMetrics
    -- ** Request lenses
    , lmDimensions
    , lmMetricName
    , lmNamespace
    , lmNextToken

    -- * Response
    , ListMetricsResponse
    -- ** Response constructor
    , listMetricsResponse
    -- ** Response lenses
    , lmrMetrics
    , lmrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import qualified GHC.Exts

data ListMetrics = ListMetrics
    { _lmDimensions :: List "Dimensions" DimensionFilter
    , _lmMetricName :: Maybe Text
    , _lmNamespace  :: Maybe Text
    , _lmNextToken  :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListMetrics' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmDimensions' @::@ ['DimensionFilter']
--
-- * 'lmMetricName' @::@ 'Maybe' 'Text'
--
-- * 'lmNamespace' @::@ 'Maybe' 'Text'
--
-- * 'lmNextToken' @::@ 'Maybe' 'Text'
--
listMetrics :: ListMetrics
listMetrics = ListMetrics
    { _lmNamespace  = Nothing
    , _lmMetricName = Nothing
    , _lmDimensions = mempty
    , _lmNextToken  = Nothing
    }

-- | A list of dimensions to filter against.
lmDimensions :: Lens' ListMetrics [DimensionFilter]
lmDimensions = lens _lmDimensions (\s a -> s { _lmDimensions = a }) . _List

-- | The name of the metric to filter against.
lmMetricName :: Lens' ListMetrics (Maybe Text)
lmMetricName = lens _lmMetricName (\s a -> s { _lmMetricName = a })

-- | The namespace to filter against.
lmNamespace :: Lens' ListMetrics (Maybe Text)
lmNamespace = lens _lmNamespace (\s a -> s { _lmNamespace = a })

-- | The token returned by a previous call to indicate that there is more data
-- available.
lmNextToken :: Lens' ListMetrics (Maybe Text)
lmNextToken = lens _lmNextToken (\s a -> s { _lmNextToken = a })

data ListMetricsResponse = ListMetricsResponse
    { _lmrMetrics   :: List "Metrics" Metric
    , _lmrNextToken :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListMetricsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmrMetrics' @::@ ['Metric']
--
-- * 'lmrNextToken' @::@ 'Maybe' 'Text'
--
listMetricsResponse :: ListMetricsResponse
listMetricsResponse = ListMetricsResponse
    { _lmrMetrics   = mempty
    , _lmrNextToken = Nothing
    }

-- | A list of metrics used to generate statistics for an AWS account.
lmrMetrics :: Lens' ListMetricsResponse [Metric]
lmrMetrics = lens _lmrMetrics (\s a -> s { _lmrMetrics = a }) . _List

-- | A string that marks the start of the next batch of returned results.
lmrNextToken :: Lens' ListMetricsResponse (Maybe Text)
lmrNextToken = lens _lmrNextToken (\s a -> s { _lmrNextToken = a })

instance ToPath ListMetrics where
    toPath = const "/"

instance ToQuery ListMetrics where
    toQuery ListMetrics{..} = mconcat
        [ "Dimensions" =? _lmDimensions
        , "MetricName" =? _lmMetricName
        , "Namespace"  =? _lmNamespace
        , "NextToken"  =? _lmNextToken
        ]

instance ToHeaders ListMetrics

instance AWSRequest ListMetrics where
    type Sv ListMetrics = CloudWatch
    type Rs ListMetrics = ListMetricsResponse

    request  = post "ListMetrics"
    response = xmlResponse

instance FromXML ListMetricsResponse where
    parseXML = withElement "ListMetricsResult" $ \x -> ListMetricsResponse
        <$> x .@  "Metrics"
        <*> x .@? "NextToken"

instance AWSPager ListMetrics where
    page rq rs
        | stop (rq ^. lmNextToken) = Nothing
        | otherwise = (\x -> rq & lmNextToken ?~ x)
            <$> (rs ^. lmrNextToken)
