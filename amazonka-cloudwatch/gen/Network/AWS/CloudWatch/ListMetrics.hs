{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , lmMetricName
    , lmNamespace
    , lmNextToken
    , lmDimensions

    -- * Response
    , ListMetricsResponse
    -- ** Response constructor
    , listMetricsResponse
    -- ** Response lenses
    , lmrMetrics
    , lmrNextToken
    , lmrStatus
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listMetrics' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmMetricName'
--
-- * 'lmNamespace'
--
-- * 'lmNextToken'
--
-- * 'lmDimensions'
data ListMetrics = ListMetrics'
    { _lmMetricName :: !(Maybe Text)
    , _lmNamespace  :: !(Maybe Text)
    , _lmNextToken  :: !(Maybe Text)
    , _lmDimensions :: !(Maybe [DimensionFilter])
    } deriving (Eq,Read,Show)

-- | 'ListMetrics' smart constructor.
listMetrics :: ListMetrics
listMetrics =
    ListMetrics'
    { _lmMetricName = Nothing
    , _lmNamespace = Nothing
    , _lmNextToken = Nothing
    , _lmDimensions = Nothing
    }

-- | The name of the metric to filter against.
lmMetricName :: Lens' ListMetrics (Maybe Text)
lmMetricName = lens _lmMetricName (\ s a -> s{_lmMetricName = a});

-- | The namespace to filter against.
lmNamespace :: Lens' ListMetrics (Maybe Text)
lmNamespace = lens _lmNamespace (\ s a -> s{_lmNamespace = a});

-- | The token returned by a previous call to indicate that there is more
-- data available.
lmNextToken :: Lens' ListMetrics (Maybe Text)
lmNextToken = lens _lmNextToken (\ s a -> s{_lmNextToken = a});

-- | A list of dimensions to filter against.
lmDimensions :: Lens' ListMetrics [DimensionFilter]
lmDimensions = lens _lmDimensions (\ s a -> s{_lmDimensions = a}) . _Default;

instance AWSPager ListMetrics where
        page rq rs
          | stop (rs ^. lmrNextToken) = Nothing
          | stop (rs ^. lmrMetrics) = Nothing
          | otherwise =
            Just $ rq & lmNextToken .~ rs ^. lmrNextToken

instance AWSRequest ListMetrics where
        type Sv ListMetrics = CloudWatch
        type Rs ListMetrics = ListMetricsResponse
        request = post
        response
          = receiveXMLWrapper "ListMetricsResult"
              (\ s h x ->
                 ListMetricsResponse' <$>
                   (x .@? "Metrics" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListMetrics where
        toHeaders = const mempty

instance ToPath ListMetrics where
        toPath = const "/"

instance ToQuery ListMetrics where
        toQuery ListMetrics'{..}
          = mconcat
              ["Action" =: ("ListMetrics" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "MetricName" =: _lmMetricName,
               "Namespace" =: _lmNamespace,
               "NextToken" =: _lmNextToken,
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _lmDimensions)]

-- | The output for the ListMetrics action.
--
-- /See:/ 'listMetricsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmrMetrics'
--
-- * 'lmrNextToken'
--
-- * 'lmrStatus'
data ListMetricsResponse = ListMetricsResponse'
    { _lmrMetrics   :: !(Maybe [Metric])
    , _lmrNextToken :: !(Maybe Text)
    , _lmrStatus    :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListMetricsResponse' smart constructor.
listMetricsResponse :: Int -> ListMetricsResponse
listMetricsResponse pStatus =
    ListMetricsResponse'
    { _lmrMetrics = Nothing
    , _lmrNextToken = Nothing
    , _lmrStatus = pStatus
    }

-- | A list of metrics used to generate statistics for an AWS account.
lmrMetrics :: Lens' ListMetricsResponse [Metric]
lmrMetrics = lens _lmrMetrics (\ s a -> s{_lmrMetrics = a}) . _Default;

-- | A string that marks the start of the next batch of returned results.
lmrNextToken :: Lens' ListMetricsResponse (Maybe Text)
lmrNextToken = lens _lmrNextToken (\ s a -> s{_lmrNextToken = a});

-- | FIXME: Undocumented member.
lmrStatus :: Lens' ListMetricsResponse Int
lmrStatus = lens _lmrStatus (\ s a -> s{_lmrStatus = a});
