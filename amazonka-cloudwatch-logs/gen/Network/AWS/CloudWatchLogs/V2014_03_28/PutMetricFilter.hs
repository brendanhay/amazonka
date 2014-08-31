{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.CloudWatchLogs.V2014_03_28.PutMetricFilter where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data PutMetricFilter = PutMetricFilter
    { _pmfrFilterName :: Text
      -- ^ The name of the metric filter.
    , _pmfrFilterPattern :: Text
    , _pmfrLogGroupName :: Text
    , _pmfrMetricTransformations :: [MetricTransformation]
    } deriving (Show, Generic)

makeLenses ''PutMetricFilter

instance ToPath PutMetricFilter

instance ToQuery PutMetricFilter

instance ToHeaders PutMetricFilter

instance ToJSON PutMetricFilter

data PutMetricFilterResponse = PutMetricFilterResponse
    deriving (Eq, Show, Generic)

makeLenses ''PutMetricFilterResponse

instance AWSRequest PutMetricFilter where
    type Sv PutMetricFilter = CloudWatchLogs
    type Rs PutMetricFilter = PutMetricFilterResponse

    request = get
    response _ = nullaryResponse PutMetricFilterResponse
