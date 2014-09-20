{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.PutMetricFilter
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

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data PutMetricFilter = PutMetricFilter
    { _pmfLogGroupName :: Text
    , _pmfFilterName :: Text
    , _pmfFilterPattern :: Text
    , _pmfMetricTransformations :: List1 MetricTransformation
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutMetricFilter' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LogGroupName ::@ @Text@
--
-- * @FilterName ::@ @Text@
--
-- * @FilterPattern ::@ @Text@
--
-- * @MetricTransformations ::@ @List1 MetricTransformation@
--
putMetricFilter :: Text -- ^ 'pmfLogGroupName'
                -> Text -- ^ 'pmfFilterName'
                -> Text -- ^ 'pmfFilterPattern'
                -> List1 MetricTransformation -- ^ 'pmfMetricTransformations'
                -> PutMetricFilter
putMetricFilter p1 p2 p3 p4 = PutMetricFilter
    { _pmfLogGroupName = p1
    , _pmfFilterName = p2
    , _pmfFilterPattern = p3
    , _pmfMetricTransformations = p4
    }

pmfLogGroupName :: Lens' PutMetricFilter Text
pmfLogGroupName = lens _pmfLogGroupName (\s a -> s { _pmfLogGroupName = a })

-- | The name of the metric filter.
pmfFilterName :: Lens' PutMetricFilter Text
pmfFilterName = lens _pmfFilterName (\s a -> s { _pmfFilterName = a })

pmfFilterPattern :: Lens' PutMetricFilter Text
pmfFilterPattern =
    lens _pmfFilterPattern (\s a -> s { _pmfFilterPattern = a })

pmfMetricTransformations :: Lens' PutMetricFilter (List1 MetricTransformation)
pmfMetricTransformations =
    lens _pmfMetricTransformations
         (\s a -> s { _pmfMetricTransformations = a })

instance ToPath PutMetricFilter

instance ToQuery PutMetricFilter

instance ToHeaders PutMetricFilter

instance ToJSON PutMetricFilter

data PutMetricFilterResponse = PutMetricFilterResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutMetricFilterResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
putMetricFilterResponse :: PutMetricFilterResponse
putMetricFilterResponse = PutMetricFilterResponse

instance AWSRequest PutMetricFilter where
    type Sv PutMetricFilter = CloudWatchLogs
    type Rs PutMetricFilter = PutMetricFilterResponse

    request = get
    response _ = nullaryResponse PutMetricFilterResponse
