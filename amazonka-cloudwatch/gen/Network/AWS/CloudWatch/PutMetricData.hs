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
-- associates the data points with the specified metric. If the specified metric
-- does not exist, Amazon CloudWatch creates the metric. It can take up to
-- fifteen minutes for a new metric to appear in calls to the 'ListMetrics' action.
--
-- The size of a PutMetricData request is limited to 8 KB for HTTP GET
-- requests and 40 KB for HTTP POST requests.
--
-- Although the 'Value' parameter accepts numbers of type 'Double', Amazon
-- CloudWatch truncates values with very large exponents. Values with base-10
-- exponents greater than 126 (1 x 10^126) are truncated. Likewise, values with
-- base-10 exponents less than -130 (1 x 10^-130) are also truncated.  Data that
-- is timestamped 24 hours or more in the past may take in excess of 48 hours to
-- become available from submission time using 'GetMetricStatistics'.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_PutMetricData.html>
module Network.AWS.CloudWatch.PutMetricData
    (
    -- * Request
      PutMetricData
    -- ** Request constructor
    , putMetricData
    -- ** Request lenses
    , pmdMetricData
    , pmdNamespace

    -- * Response
    , PutMetricDataResponse
    -- ** Response constructor
    , putMetricDataResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import qualified GHC.Exts

data PutMetricData = PutMetricData
    { _pmdMetricData :: List "MetricData" MetricDatum
    , _pmdNamespace  :: Text
    } deriving (Eq, Show)

-- | 'PutMetricData' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmdMetricData' @::@ ['MetricDatum']
--
-- * 'pmdNamespace' @::@ 'Text'
--
putMetricData :: Text -- ^ 'pmdNamespace'
              -> PutMetricData
putMetricData p1 = PutMetricData
    { _pmdNamespace  = p1
    , _pmdMetricData = mempty
    }

-- | A list of data describing the metric.
pmdMetricData :: Lens' PutMetricData [MetricDatum]
pmdMetricData = lens _pmdMetricData (\s a -> s { _pmdMetricData = a }) . _List

-- | The namespace for the metric data.
pmdNamespace :: Lens' PutMetricData Text
pmdNamespace = lens _pmdNamespace (\s a -> s { _pmdNamespace = a })

data PutMetricDataResponse = PutMetricDataResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutMetricDataResponse' constructor.
putMetricDataResponse :: PutMetricDataResponse
putMetricDataResponse = PutMetricDataResponse

instance ToPath PutMetricData where
    toPath = const "/"

instance ToQuery PutMetricData where
    toQuery PutMetricData{..} = mconcat
        [ "MetricData" =? _pmdMetricData
        , "Namespace"  =? _pmdNamespace
        ]

instance ToHeaders PutMetricData

instance AWSRequest PutMetricData where
    type Sv PutMetricData = CloudWatch
    type Rs PutMetricData = PutMetricDataResponse

    request  = post "PutMetricData"
    response = nullResponse PutMetricDataResponse
