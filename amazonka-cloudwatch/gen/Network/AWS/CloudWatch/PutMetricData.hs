{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutMetricData
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Publishes metric data points to Amazon CloudWatch. Amazon Cloudwatch
-- associates the data points with the specified metric. If the specified
-- metric does not exist, Amazon CloudWatch creates the metric. It can take
-- up to fifteen minutes for a new metric to appear in calls to the
-- ListMetrics action.
--
-- The size of a PutMetricData request is limited to 8 KB for HTTP GET
-- requests and 40 KB for HTTP POST requests.
--
-- Although the @Value@ parameter accepts numbers of type @Double@, Amazon
-- CloudWatch truncates values with very large exponents. Values with
-- base-10 exponents greater than 126 (1 x 10^126) are truncated. Likewise,
-- values with base-10 exponents less than -130 (1 x 10^-130) are also
-- truncated.
--
-- Data that is timestamped 24 hours or more in the past may take in excess
-- of 48 hours to become available from submission time using
-- @GetMetricStatistics@.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_PutMetricData.html>
module Network.AWS.CloudWatch.PutMetricData
    (
    -- * Request
      PutMetricData
    -- ** Request constructor
    , putMetricData
    -- ** Request lenses
    , pmdNamespace
    , pmdMetricData

    -- * Response
    , PutMetricDataResponse
    -- ** Response constructor
    , putMetricDataResponse
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putMetricData' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmdNamespace'
--
-- * 'pmdMetricData'
data PutMetricData = PutMetricData'
    { _pmdNamespace  :: !Text
    , _pmdMetricData :: ![MetricDatum]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutMetricData' smart constructor.
putMetricData :: Text -> PutMetricData
putMetricData pNamespace_ =
    PutMetricData'
    { _pmdNamespace = pNamespace_
    , _pmdMetricData = mempty
    }

-- | The namespace for the metric data.
pmdNamespace :: Lens' PutMetricData Text
pmdNamespace = lens _pmdNamespace (\ s a -> s{_pmdNamespace = a});

-- | A list of data describing the metric.
pmdMetricData :: Lens' PutMetricData [MetricDatum]
pmdMetricData = lens _pmdMetricData (\ s a -> s{_pmdMetricData = a});

instance AWSRequest PutMetricData where
        type Sv PutMetricData = CloudWatch
        type Rs PutMetricData = PutMetricDataResponse
        request = postQuery
        response = receiveNull PutMetricDataResponse'

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

-- | 'PutMetricDataResponse' smart constructor.
putMetricDataResponse :: PutMetricDataResponse
putMetricDataResponse = PutMetricDataResponse'
