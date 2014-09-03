{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.PutMetricFilter
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates a metric filter and associates it with the specified log
-- group. Metric filters allow you to configure rules to extract metric data
-- from log events ingested through PutLogEvents requests. Create or update a
-- metric filter The following is an example of a PutMetricFilter request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.PutMetricFilter { "logGroupName":
-- "exampleLogGroupName", "filterName": "exampleMetricFilterName",
-- "filterPattern": "[ip, identity, user_id, timestamp, request, status_code,
-- size]", "metricTransformations": [ { "metricValue": "$size",
-- "metricNamespace": "MyApp", "metricName": "Volume" }, { "metricValue": "1",
-- "metricNamespace": "MyApp", "metricName": "RequestCount" } ] } HTTP/1.1 200
-- OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
module Network.AWS.CloudWatchLogs.V2014_03_28.PutMetricFilter
    (
    -- * Request
      PutMetricFilter
    -- ** Request constructor
    , putMetricFilter
    -- ** Request lenses
    , pmfrFilterName
    , pmfrFilterPattern
    , pmfrLogGroupName
    , pmfrMetricTransformations

    -- * Response
    , PutMetricFilterResponse
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'PutMetricFilter' request.
putMetricFilter :: Text -- ^ 'pmfrFilterName'
                -> Text -- ^ 'pmfrFilterPattern'
                -> Text -- ^ 'pmfrLogGroupName'
                -> [MetricTransformation] -- ^ 'pmfrMetricTransformations'
                -> PutMetricFilter
putMetricFilter p1 p2 p3 p4 = PutMetricFilter
    { _pmfrFilterName = p1
    , _pmfrFilterPattern = p2
    , _pmfrLogGroupName = p3
    , _pmfrMetricTransformations = p4
    }

data PutMetricFilter = PutMetricFilter
    { _pmfrFilterName :: Text
      -- ^ The name of the metric filter.
    , _pmfrFilterPattern :: Text
    , _pmfrLogGroupName :: Text
    , _pmfrMetricTransformations :: [MetricTransformation]
    } deriving (Show, Generic)

-- | The name of the metric filter.
pmfrFilterName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutMetricFilter
    -> f PutMetricFilter
pmfrFilterName f x =
    (\y -> x { _pmfrFilterName = y })
       <$> f (_pmfrFilterName x)
{-# INLINE pmfrFilterName #-}

pmfrFilterPattern
    :: Functor f
    => (Text
    -> f (Text))
    -> PutMetricFilter
    -> f PutMetricFilter
pmfrFilterPattern f x =
    (\y -> x { _pmfrFilterPattern = y })
       <$> f (_pmfrFilterPattern x)
{-# INLINE pmfrFilterPattern #-}

pmfrLogGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutMetricFilter
    -> f PutMetricFilter
pmfrLogGroupName f x =
    (\y -> x { _pmfrLogGroupName = y })
       <$> f (_pmfrLogGroupName x)
{-# INLINE pmfrLogGroupName #-}

pmfrMetricTransformations
    :: Functor f
    => ([MetricTransformation]
    -> f ([MetricTransformation]))
    -> PutMetricFilter
    -> f PutMetricFilter
pmfrMetricTransformations f x =
    (\y -> x { _pmfrMetricTransformations = y })
       <$> f (_pmfrMetricTransformations x)
{-# INLINE pmfrMetricTransformations #-}

instance ToPath PutMetricFilter

instance ToQuery PutMetricFilter

instance ToHeaders PutMetricFilter

instance ToJSON PutMetricFilter

data PutMetricFilterResponse = PutMetricFilterResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutMetricFilter where
    type Sv PutMetricFilter = CloudWatchLogs
    type Rs PutMetricFilter = PutMetricFilterResponse

    request = get
    response _ = nullaryResponse PutMetricFilterResponse
