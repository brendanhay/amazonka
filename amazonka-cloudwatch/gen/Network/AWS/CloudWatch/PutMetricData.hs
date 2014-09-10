{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.PutMetricData
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Publishes metric data points to Amazon CloudWatch. Amazon Cloudwatch
-- associates the data points with the specified metric. If the specified
-- metric does not exist, Amazon CloudWatch creates the metric. If you create
-- a metric with the PutMetricData action, allow up to fifteen minutes for the
-- metric to appear in calls to the ListMetrics action. The size of a
-- PutMetricData request is limited to 8 KB for HTTP GET requests and 40 KB
-- for HTTP POST requests. Although the Value parameter accepts numbers of
-- type Double, Amazon CloudWatch truncates values with very large exponents.
-- Values with base-10 exponents greater than 126 (1 x 10^126) are truncated.
-- Likewise, values with base-10 exponents less than -130 (1 x 10^-130) are
-- also truncated.
module Network.AWS.CloudWatch.PutMetricData
    (
    -- * Request
      PutMetricData
    -- ** Request constructor
    , mkPutMetricData
    -- ** Request lenses
    , pmdNamespace
    , pmdMetricData

    -- * Response
    , PutMetricDataResponse
    -- ** Response constructor
    , mkPutMetricDataResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import Network.AWS.Prelude

data PutMetricData = PutMetricData
    { _pmdNamespace :: !Text
    , _pmdMetricData :: [MetricDatum]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutMetricData' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Namespace ::@ @Text@
--
-- * @MetricData ::@ @[MetricDatum]@
--
mkPutMetricData :: Text -- ^ 'pmdNamespace'
                -> [MetricDatum] -- ^ 'pmdMetricData'
                -> PutMetricData
mkPutMetricData p1 p2 = PutMetricData
    { _pmdNamespace = p1
    , _pmdMetricData = p2
    }

-- | The namespace for the metric data.
pmdNamespace :: Lens' PutMetricData Text
pmdNamespace = lens _pmdNamespace (\s a -> s { _pmdNamespace = a })

-- | A list of data describing the metric.
pmdMetricData :: Lens' PutMetricData [MetricDatum]
pmdMetricData = lens _pmdMetricData (\s a -> s { _pmdMetricData = a })

instance ToQuery PutMetricData where
    toQuery = genericQuery def

data PutMetricDataResponse = PutMetricDataResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutMetricDataResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkPutMetricDataResponse :: PutMetricDataResponse
mkPutMetricDataResponse = PutMetricDataResponse

instance AWSRequest PutMetricData where
    type Sv PutMetricData = CloudWatch
    type Rs PutMetricData = PutMetricDataResponse

    request = post "PutMetricData"
    response _ = nullaryResponse PutMetricDataResponse
