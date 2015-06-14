{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatch.ListMetrics
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of valid metrics stored for the AWS account owner.
-- Returned metrics can be used with GetMetricStatistics to obtain
-- statistical data for a given metric.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html>
module Network.AWS.CloudWatch.ListMetrics
    (
    -- * Request
      ListMetrics
    -- ** Request constructor
    , listMetrics
    -- ** Request lenses
    , lmNextToken
    , lmDimensions
    , lmMetricName
    , lmNamespace

    -- * Response
    , ListMetricsResponse
    -- ** Response constructor
    , listMetricsResponse
    -- ** Response lenses
    , lmrMetrics
    , lmrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudWatch.Types

-- | /See:/ 'listMetrics' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmNextToken'
--
-- * 'lmDimensions'
--
-- * 'lmMetricName'
--
-- * 'lmNamespace'
data ListMetrics = ListMetrics'{_lmNextToken :: Maybe Text, _lmDimensions :: [DimensionFilter], _lmMetricName :: Text, _lmNamespace :: Text} deriving (Eq, Read, Show)

-- | 'ListMetrics' smart constructor.
listMetrics :: Text -> Text -> ListMetrics
listMetrics pMetricName pNamespace = ListMetrics'{_lmNextToken = Nothing, _lmDimensions = mempty, _lmMetricName = pMetricName, _lmNamespace = pNamespace};

-- | The token returned by a previous call to indicate that there is more
-- data available.
lmNextToken :: Lens' ListMetrics (Maybe Text)
lmNextToken = lens _lmNextToken (\ s a -> s{_lmNextToken = a});

-- | A list of dimensions to filter against.
lmDimensions :: Lens' ListMetrics [DimensionFilter]
lmDimensions = lens _lmDimensions (\ s a -> s{_lmDimensions = a});

-- | The name of the metric to filter against.
lmMetricName :: Lens' ListMetrics Text
lmMetricName = lens _lmMetricName (\ s a -> s{_lmMetricName = a});

-- | The namespace to filter against.
lmNamespace :: Lens' ListMetrics Text
lmNamespace = lens _lmNamespace (\ s a -> s{_lmNamespace = a});

instance AWSRequest ListMetrics where
        type Sv ListMetrics = CloudWatch
        type Rs ListMetrics = ListMetricsResponse
        request = post
        response
          = receiveXMLWrapper "ListMetricsResult"
              (\ s h x ->
                 ListMetricsResponse' <$>
                   (x .@? "Metrics" .!@ mempty >>=
                      parseXMLList "member")
                     <*> x .@? "NextToken")

instance ToHeaders ListMetrics where
        toHeaders = const mempty

instance ToPath ListMetrics where
        toPath = const "/"

instance ToQuery ListMetrics where
        toQuery ListMetrics'{..}
          = mconcat
              ["Action" =: ("ListMetrics" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "NextToken" =: _lmNextToken,
               "Dimensions" =: "member" =: _lmDimensions,
               "MetricName" =: _lmMetricName,
               "Namespace" =: _lmNamespace]

-- | /See:/ 'listMetricsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmrMetrics'
--
-- * 'lmrNextToken'
data ListMetricsResponse = ListMetricsResponse'{_lmrMetrics :: [Metric], _lmrNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListMetricsResponse' smart constructor.
listMetricsResponse :: ListMetricsResponse
listMetricsResponse = ListMetricsResponse'{_lmrMetrics = mempty, _lmrNextToken = Nothing};

-- | A list of metrics used to generate statistics for an AWS account.
lmrMetrics :: Lens' ListMetricsResponse [Metric]
lmrMetrics = lens _lmrMetrics (\ s a -> s{_lmrMetrics = a});

-- | A string that marks the start of the next batch of returned results.
lmrNextToken :: Lens' ListMetricsResponse (Maybe Text)
lmrNextToken = lens _lmrNextToken (\ s a -> s{_lmrNextToken = a});
