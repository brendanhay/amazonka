{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- metric does not exist, Amazon CloudWatch creates the metric. It can take up
-- to fifteen minutes for a new metric to appear in calls to the ListMetrics
-- action. The size of a PutMetricData request is limited to 8 KB for HTTP GET
-- requests and 40 KB for HTTP POST requests. Although the Value parameter
-- accepts numbers of type Double, Amazon CloudWatch truncates values with
-- very large exponents. Values with base-10 exponents greater than 126 (1 x
-- 10^126) are truncated. Likewise, values with base-10 exponents less than
-- -130 (1 x 10^-130) are also truncated. Data that is timestamped 24 hours or
-- more in the past may take in excess of 48 hours to become available from
-- submission time using GetMetricStatistics.
module Network.AWS.CloudWatch.PutMetricData
    (
    -- * Request
      PutMetricDataInput
    -- ** Request constructor
    , putMetricDataInput
    -- ** Request lenses
    , pmdiMetricData
    , pmdiNamespace

    -- * Response
    , PutMetricDataResponse
    -- ** Response constructor
    , putMetricDataResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

data PutMetricDataInput = PutMetricDataInput
    { _pmdiMetricData :: [MetricDatum]
    , _pmdiNamespace  :: Text
    } deriving (Eq, Show, Generic)

-- | 'PutMetricDataInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmdiMetricData' @::@ ['MetricDatum']
--
-- * 'pmdiNamespace' @::@ 'Text'
--
putMetricDataInput :: Text -- ^ 'pmdiNamespace'
                   -> PutMetricDataInput
putMetricDataInput p1 = PutMetricDataInput
    { _pmdiNamespace  = p1
    , _pmdiMetricData = mempty
    }

-- | A list of data describing the metric.
pmdiMetricData :: Lens' PutMetricDataInput [MetricDatum]
pmdiMetricData = lens _pmdiMetricData (\s a -> s { _pmdiMetricData = a })

-- | The namespace for the metric data.
pmdiNamespace :: Lens' PutMetricDataInput Text
pmdiNamespace = lens _pmdiNamespace (\s a -> s { _pmdiNamespace = a })
instance ToQuery PutMetricDataInput

instance ToPath PutMetricDataInput where
    toPath = const "/"

data PutMetricDataResponse = PutMetricDataResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutMetricDataResponse' constructor.
putMetricDataResponse :: PutMetricDataResponse
putMetricDataResponse = PutMetricDataResponse
instance FromXML PutMetricDataResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutMetricDataResponse"

instance AWSRequest PutMetricDataInput where
    type Sv PutMetricDataInput = CloudWatch
    type Rs PutMetricDataInput = PutMetricDataResponse

    request  = post "PutMetricData"
    response = nullaryResponse PutMetricDataResponse
