{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutMetricData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes metric data points to Amazon CloudWatch. CloudWatch associates the data points with the specified metric. If the specified metric does not exist, CloudWatch creates the metric. When CloudWatch creates a metric, it can take up to fifteen minutes for the metric to appear in calls to 'ListMetrics' .
--
--
-- Each @PutMetricData@ request is limited to 40 KB in size for HTTP POST requests.
--
-- Although the @Value@ parameter accepts numbers of type @Double@ , CloudWatch rejects values that are either too small or too large. Values must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- You can use up to 10 dimensions per metric to further clarify what data the metric collects. For more information about specifying dimensions, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publishing Metrics> in the /Amazon CloudWatch User Guide/ .
--
-- Data points with time stamps from 24 hours ago or longer can take at least 48 hours to become available for 'GetMetricStatistics' from the time they are submitted.
--
-- CloudWatch needs raw data points to calculate percentile statistics. If you publish data using a statistic set instead, you can only retrieve percentile statistics for this data if one of the following conditions is true:
--
--     * The SampleCount value of the statistic set is 1
--
--     * The Min and the Max values of the statistic set are equal
--
--
--
module Network.AWS.CloudWatch.PutMetricData
    (
    -- * Creating a Request
      putMetricData
    , PutMetricData
    -- * Request Lenses
    , pmdNamespace
    , pmdMetricData

    -- * Destructuring the Response
    , putMetricDataResponse
    , PutMetricDataResponse
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putMetricData' smart constructor.
data PutMetricData = PutMetricData'
  { _pmdNamespace  :: !Text
  , _pmdMetricData :: ![MetricDatum]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmdNamespace' - The namespace for the metric data. You cannot specify a namespace that begins with "AWS/". Namespaces that begin with "AWS/" are reserved for use by Amazon Web Services products.
--
-- * 'pmdMetricData' - The data for the metric.
putMetricData
    :: Text -- ^ 'pmdNamespace'
    -> PutMetricData
putMetricData pNamespace_ =
  PutMetricData' {_pmdNamespace = pNamespace_, _pmdMetricData = mempty}


-- | The namespace for the metric data. You cannot specify a namespace that begins with "AWS/". Namespaces that begin with "AWS/" are reserved for use by Amazon Web Services products.
pmdNamespace :: Lens' PutMetricData Text
pmdNamespace = lens _pmdNamespace (\ s a -> s{_pmdNamespace = a})

-- | The data for the metric.
pmdMetricData :: Lens' PutMetricData [MetricDatum]
pmdMetricData = lens _pmdMetricData (\ s a -> s{_pmdMetricData = a}) . _Coerce

instance AWSRequest PutMetricData where
        type Rs PutMetricData = PutMetricDataResponse
        request = postQuery cloudWatch
        response = receiveNull PutMetricDataResponse'

instance Hashable PutMetricData where

instance NFData PutMetricData where

instance ToHeaders PutMetricData where
        toHeaders = const mempty

instance ToPath PutMetricData where
        toPath = const "/"

instance ToQuery PutMetricData where
        toQuery PutMetricData'{..}
          = mconcat
              ["Action" =: ("PutMetricData" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "Namespace" =: _pmdNamespace,
               "MetricData" =: toQueryList "member" _pmdMetricData]

-- | /See:/ 'putMetricDataResponse' smart constructor.
data PutMetricDataResponse =
  PutMetricDataResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutMetricDataResponse' with the minimum fields required to make a request.
--
putMetricDataResponse
    :: PutMetricDataResponse
putMetricDataResponse = PutMetricDataResponse'


instance NFData PutMetricDataResponse where
