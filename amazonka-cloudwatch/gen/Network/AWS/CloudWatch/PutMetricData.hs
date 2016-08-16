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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes metric data points to Amazon CloudWatch. Amazon CloudWatch associates the data points with the specified metric. If the specified metric does not exist, Amazon CloudWatch creates the metric. When Amazon CloudWatch creates a metric, it can take up to fifteen minutes for the metric to appear in calls to < ListMetrics>.
--
-- Each 'PutMetricData' request is limited to 8 KB in size for HTTP GET requests and is limited to 40 KB in size for HTTP POST requests.
--
-- Although the 'Value' parameter accepts numbers of type 'Double', Amazon CloudWatch rejects values that are either too small or too large. Values must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In addition, special values (e.g., NaN, +Infinity, -Infinity) are not supported.
--
-- Data that is timestamped 24 hours or more in the past may take in excess of 48 hours to become available from submission time using 'GetMetricStatistics'.
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

import           Network.AWS.CloudWatch.Types
import           Network.AWS.CloudWatch.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Describes the inputs for PutMetricData.
--
-- /See:/ 'putMetricData' smart constructor.
data PutMetricData = PutMetricData'
    { _pmdNamespace  :: !Text
    , _pmdMetricData :: ![MetricDatum]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmdNamespace'
--
-- * 'pmdMetricData'
putMetricData
    :: Text -- ^ 'pmdNamespace'
    -> PutMetricData
putMetricData pNamespace_ =
    PutMetricData'
    { _pmdNamespace = pNamespace_
    , _pmdMetricData = mempty
    }

-- | The namespace for the metric data.
--
-- You cannot specify a namespace that begins with \"AWS\/\". Namespaces that begin with \"AWS\/\" are reserved for other Amazon Web Services products that send metrics to Amazon CloudWatch.
pmdNamespace :: Lens' PutMetricData Text
pmdNamespace = lens _pmdNamespace (\ s a -> s{_pmdNamespace = a});

-- | A list of data describing the metric.
pmdMetricData :: Lens' PutMetricData [MetricDatum]
pmdMetricData = lens _pmdMetricData (\ s a -> s{_pmdMetricData = a}) . _Coerce;

instance AWSRequest PutMetricData where
        type Rs PutMetricData = PutMetricDataResponse
        request = postQuery cloudWatch
        response = receiveNull PutMetricDataResponse'

instance Hashable PutMetricData

instance NFData PutMetricData

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutMetricDataResponse' with the minimum fields required to make a request.
--
putMetricDataResponse
    :: PutMetricDataResponse
putMetricDataResponse = PutMetricDataResponse'

instance NFData PutMetricDataResponse
