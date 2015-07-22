{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.ListMetrics
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of valid metrics stored for the AWS account owner.
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
    , lmrqMetricName
    , lmrqNamespace
    , lmrqNextToken
    , lmrqDimensions

    -- * Response
    , ListMetricsResponse
    -- ** Response constructor
    , listMetricsResponse
    -- ** Response lenses
    , lmrsMetrics
    , lmrsNextToken
    , lmrsStatus
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
-- * 'lmrqMetricName'
--
-- * 'lmrqNamespace'
--
-- * 'lmrqNextToken'
--
-- * 'lmrqDimensions'
data ListMetrics = ListMetrics'
    { _lmrqMetricName :: !(Maybe Text)
    , _lmrqNamespace  :: !(Maybe Text)
    , _lmrqNextToken  :: !(Maybe Text)
    , _lmrqDimensions :: !(Maybe [DimensionFilter])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListMetrics' smart constructor.
listMetrics :: ListMetrics
listMetrics =
    ListMetrics'
    { _lmrqMetricName = Nothing
    , _lmrqNamespace = Nothing
    , _lmrqNextToken = Nothing
    , _lmrqDimensions = Nothing
    }

-- | The name of the metric to filter against.
lmrqMetricName :: Lens' ListMetrics (Maybe Text)
lmrqMetricName = lens _lmrqMetricName (\ s a -> s{_lmrqMetricName = a});

-- | The namespace to filter against.
lmrqNamespace :: Lens' ListMetrics (Maybe Text)
lmrqNamespace = lens _lmrqNamespace (\ s a -> s{_lmrqNamespace = a});

-- | The token returned by a previous call to indicate that there is more
-- data available.
lmrqNextToken :: Lens' ListMetrics (Maybe Text)
lmrqNextToken = lens _lmrqNextToken (\ s a -> s{_lmrqNextToken = a});

-- | A list of dimensions to filter against.
lmrqDimensions :: Lens' ListMetrics [DimensionFilter]
lmrqDimensions = lens _lmrqDimensions (\ s a -> s{_lmrqDimensions = a}) . _Default;

instance AWSPager ListMetrics where
        page rq rs
          | stop (rs ^. lmrsNextToken) = Nothing
          | stop (rs ^. lmrsMetrics) = Nothing
          | otherwise =
            Just $ rq & lmrqNextToken .~ rs ^. lmrsNextToken

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
               "MetricName" =: _lmrqMetricName,
               "Namespace" =: _lmrqNamespace,
               "NextToken" =: _lmrqNextToken,
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _lmrqDimensions)]

-- | The output for the ListMetrics action.
--
-- /See:/ 'listMetricsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmrsMetrics'
--
-- * 'lmrsNextToken'
--
-- * 'lmrsStatus'
data ListMetricsResponse = ListMetricsResponse'
    { _lmrsMetrics   :: !(Maybe [Metric])
    , _lmrsNextToken :: !(Maybe Text)
    , _lmrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListMetricsResponse' smart constructor.
listMetricsResponse :: Int -> ListMetricsResponse
listMetricsResponse pStatus =
    ListMetricsResponse'
    { _lmrsMetrics = Nothing
    , _lmrsNextToken = Nothing
    , _lmrsStatus = pStatus
    }

-- | A list of metrics used to generate statistics for an AWS account.
lmrsMetrics :: Lens' ListMetricsResponse [Metric]
lmrsMetrics = lens _lmrsMetrics (\ s a -> s{_lmrsMetrics = a}) . _Default;

-- | A string that marks the start of the next batch of returned results.
lmrsNextToken :: Lens' ListMetricsResponse (Maybe Text)
lmrsNextToken = lens _lmrsNextToken (\ s a -> s{_lmrsNextToken = a});

-- | FIXME: Undocumented member.
lmrsStatus :: Lens' ListMetricsResponse Int
lmrsStatus = lens _lmrsStatus (\ s a -> s{_lmrsStatus = a});
