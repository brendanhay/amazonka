{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudWatchLogs.PutMetricFilter
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

-- | Creates or updates a metric filter and associates it with the specified
-- log group. Metric filters allow you to configure rules to extract metric
-- data from log events ingested through @PutLogEvents@ requests.
--
-- The maximum number of metric filters that can be associated with a log
-- group is 100.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutMetricFilter.html>
module Network.AWS.CloudWatchLogs.PutMetricFilter
    (
    -- * Request
      PutMetricFilter
    -- ** Request constructor
    , putMetricFilter
    -- ** Request lenses
    , pmfLogGroupName
    , pmfFilterName
    , pmfFilterPattern
    , pmfMetricTransformations

    -- * Response
    , PutMetricFilterResponse
    -- ** Response constructor
    , putMetricFilterResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putMetricFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmfLogGroupName'
--
-- * 'pmfFilterName'
--
-- * 'pmfFilterPattern'
--
-- * 'pmfMetricTransformations'
data PutMetricFilter = PutMetricFilter'
    { _pmfLogGroupName          :: Text
    , _pmfFilterName            :: Text
    , _pmfFilterPattern         :: Text
    , _pmfMetricTransformations :: List1 MetricTransformation
    } deriving (Eq,Read,Show)

-- | 'PutMetricFilter' smart constructor.
putMetricFilter :: Text -> Text -> Text -> NonEmpty MetricTransformation -> PutMetricFilter
putMetricFilter pLogGroupName pFilterName pFilterPattern pMetricTransformations =
    PutMetricFilter'
    { _pmfLogGroupName = pLogGroupName
    , _pmfFilterName = pFilterName
    , _pmfFilterPattern = pFilterPattern
    , _pmfMetricTransformations = _List1 # pMetricTransformations
    }

-- | The name of the log group to associate the metric filter with.
pmfLogGroupName :: Lens' PutMetricFilter Text
pmfLogGroupName = lens _pmfLogGroupName (\ s a -> s{_pmfLogGroupName = a});

-- | A name for the metric filter.
pmfFilterName :: Lens' PutMetricFilter Text
pmfFilterName = lens _pmfFilterName (\ s a -> s{_pmfFilterName = a});

-- | A valid CloudWatch Logs filter pattern for extracting metric data out of
-- ingested log events.
pmfFilterPattern :: Lens' PutMetricFilter Text
pmfFilterPattern = lens _pmfFilterPattern (\ s a -> s{_pmfFilterPattern = a});

-- | A collection of information needed to define how metric data gets
-- emitted.
pmfMetricTransformations :: Lens' PutMetricFilter (NonEmpty MetricTransformation)
pmfMetricTransformations = lens _pmfMetricTransformations (\ s a -> s{_pmfMetricTransformations = a}) . _List1;

instance AWSRequest PutMetricFilter where
        type Sv PutMetricFilter = CloudWatchLogs
        type Rs PutMetricFilter = PutMetricFilterResponse
        request = postJSON
        response = receiveNull PutMetricFilterResponse'

instance ToHeaders PutMetricFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.PutMetricFilter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutMetricFilter where
        toJSON PutMetricFilter'{..}
          = object
              ["logGroupName" .= _pmfLogGroupName,
               "filterName" .= _pmfFilterName,
               "filterPattern" .= _pmfFilterPattern,
               "metricTransformations" .= _pmfMetricTransformations]

instance ToPath PutMetricFilter where
        toPath = const "/"

instance ToQuery PutMetricFilter where
        toQuery = const mempty

-- | /See:/ 'putMetricFilterResponse' smart constructor.
data PutMetricFilterResponse =
    PutMetricFilterResponse'
    deriving (Eq,Read,Show)

-- | 'PutMetricFilterResponse' smart constructor.
putMetricFilterResponse :: PutMetricFilterResponse
putMetricFilterResponse = PutMetricFilterResponse'
