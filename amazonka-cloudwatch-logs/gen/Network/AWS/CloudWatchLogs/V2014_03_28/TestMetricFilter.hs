{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.TestMetricFilter
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Tests the filter pattern of a metric filter against a sample of log event
-- messages. You can use this operation to validate the correctness of a
-- metric filter pattern. Test a metric filter pattern on Apache access.log
-- events The following is an example of a TestMetricFilter request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern": "[ip,
-- identity, user_id, timestamp, request, status_code, size]",
-- "logEventMessages": [ "127.0.0.1 - frank [10/Oct/2000:13:25:15 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 200 1534", "127.0.0.1 - frank
-- [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif HTTP/1.0\" 500 5324",
-- "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET /apache_pb.gif
-- HTTP/1.0\" 200 4355" ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 0, "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:25:15
-- -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534", "extractedValues": {
-- "$status_code": "200", "$identity": "-", "$request": "GET /apache_pb.gif
-- HTTP/1.0", "$size": "1534,", "$user_id": "frank", "$ip": "127.0.0.1",
-- "$timestamp": "10/Oct/2000:13:25:15 -0700" } }, { "eventNumber": 1,
-- "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:35:22 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 500 5324", "extractedValues": { "$status_code":
-- "500", "$identity": "-", "$request": "GET /apache_pb.gif HTTP/1.0",
-- "$size": "5324,", "$user_id": "frank", "$ip": "127.0.0.1", "$timestamp":
-- "10/Oct/2000:13:35:22 -0700" } }, { "eventNumber": 2, "eventMessage":
-- "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET /apache_pb.gif
-- HTTP/1.0\" 200 4355", "extractedValues": { "$status_code": "200",
-- "$identity": "-", "$request": "GET /apache_pb.gif HTTP/1.0", "$size":
-- "4355", "$user_id": "frank", "$ip": "127.0.0.1", "$timestamp":
-- "10/Oct/2000:13:50:35 -0700" } } ] } Test a metric filter pattern on Apache
-- access.log events without specifying all the fields The following is an
-- example of a TestMetricFilter request and response. POST / HTTP/1.1 Host:
-- logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern": "[...,
-- size]", "logEventMessages": [ "127.0.0.1 - frank [10/Oct/2000:13:25:15
-- -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534", "127.0.0.1 - frank
-- [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif HTTP/1.0\" 500 5324",
-- "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET /apache_pb.gif
-- HTTP/1.0\" 200 4355" ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 0, "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:25:15
-- -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534", "extractedValues": {
-- "$size": "1534", "$6": "200", "$4": "10/Oct/2000:13:25:15 -0700", "$5":
-- "GET /apache_pb.gif HTTP/1.0", "$2": "-", "$3": "frank", "$1": "127.0.0.1"
-- } }, { "eventNumber": 1, "eventMessage": "127.0.0.1 - frank
-- [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif HTTP/1.0\" 500 5324",
-- "extractedValues": { "$size": "5324", "$6": "500", "$4":
-- "10/Oct/2000:13:35:22 -0700", "$5": "GET /apache_pb.gif HTTP/1.0", "$2":
-- "-", "$3": "frank", "$1": "127.0.0.1" } }, { "eventNumber": 2,
-- "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 200 4355", "extractedValues": { "$size": "4355",
-- "$6": "200", "$4": "10/Oct/2000:13:50:35 -0700", "$5": "GET /apache_pb.gif
-- HTTP/1.0", "$2": "-", "$3": "frank", "$1": "127.0.0.1" } } ] } Test a
-- metric filter pattern on Apache access.log events without specifying any
-- fields The following is an example of a TestMetricFilter request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern": "[]",
-- "logEventMessages": [ "127.0.0.1 - frank [10/Oct/2000:13:25:15 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 200 1534", "127.0.0.1 - frank
-- [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif HTTP/1.0\" 500 5324",
-- "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET /apache_pb.gif
-- HTTP/1.0\" 200 4355" ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 0, "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:25:15
-- -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534", "extractedValues": {
-- "$7": "1534", "$6": "200", "$4": "10/Oct/2000:13:25:15 -0700", "$5": "GET
-- /apache_pb.gif HTTP/1.0", "$2": "-", "$3": "frank", "$1": "127.0.0.1" } },
-- { "eventNumber": 1, "eventMessage": "127.0.0.1 - frank
-- [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif HTTP/1.0\" 500 5324",
-- "extractedValues": { "$7": "5324", "$6": "500", "$4": "10/Oct/2000:13:35:22
-- -0700", "$5": "GET /apache_pb.gif HTTP/1.0", "$2": "-", "$3": "frank",
-- "$1": "127.0.0.1" } }, { "eventNumber": 2, "eventMessage": "127.0.0.1 -
-- frank [10/Oct/2000:13:50:35 -0700] \"GET /apache_pb.gif HTTP/1.0\" 200
-- 4355", "extractedValues": { "$7": "4355", "$6": "200", "$4":
-- "10/Oct/2000:13:50:35 -0700", "$5": "GET /apache_pb.gif HTTP/1.0", "$2":
-- "-", "$3": "frank", "$1": "127.0.0.1" } } ] } Test a metric filter pattern
-- that matches successful requests in Apache access.log events The following
-- is an example of a TestMetricFilter request and response. POST / HTTP/1.1
-- Host: logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern": "[...,
-- status_code=200, size]", "logEventMessages": [ "127.0.0.1 - frank
-- [10/Oct/2000:13:25:15 -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534",
-- "127.0.0.1 - frank [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif
-- HTTP/1.0\" 500 5324", "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 200 4355" ] } HTTP/1.1 200 OK x-amzn-RequestId:
-- Content-Type: application/x-amz-json-1.1 Content-Length: Date: ]]> {
-- "matches": [ { "eventNumber": 0, "eventMessage": "127.0.0.1 - frank
-- [10/Oct/2000:13:25:15 -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534",
-- "extractedValues": { "$status_code": "200", "$size": "1534", "$4":
-- "10/Oct/2000:13:25:15 -0700", "$5": "GET /apache_pb.gif HTTP/1.0", "$2":
-- "-", "$3": "frank", "$1": "127.0.0.1" } }, { "eventNumber": 2,
-- "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 200 4355", "extractedValues": { "$status_code":
-- "200", "$size": "4355", "$4": "10/Oct/2000:13:50:35 -0700", "$5": "GET
-- /apache_pb.gif HTTP/1.0", "$2": "-", "$3": "frank", "$1": "127.0.0.1" } } ]
-- } Test a metric filter pattern that matches 4XX response codes for html
-- pages in Apache access.log events The following is an example of a
-- TestMetricFilter request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern": "[...,
-- request=*.html*, status_code=4*,]", "logEventMessages": [ "127.0.0.1 -
-- frank [10/Oct/2000:13:25:15 -0700] \"GET /index.html HTTP/1.0\" 404 1534",
-- "127.0.0.1 - frank [10/Oct/2000:13:35:22 -0700] \"GET /about-us/index.html
-- HTTP/1.0\" 200 5324", "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 404 4355", "127.0.0.1 - frank
-- [10/Oct/2000:13:25:15 -0700] \"GET /products/index.html HTTP/1.0\" 400
-- 1534", ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 0, "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:25:15
-- -0700] \"GET /index.html HTTP/1.0\" 404 1534", "extractedValues": {
-- "$status_code": "404", "$request": "GET /index.html HTTP/1.0", "$7":
-- "1534", "$4": "10/Oct/2000:13:25:15 -0700", "$2": "-", "$3": "frank", "$1":
-- "127.0.0.1" } }, { "eventNumber": 3, "eventMessage": "127.0.0.1 - frank
-- [10/Oct/2000:13:25:15 -0700] \"GET /products/index.html HTTP/1.0\" 400
-- 1534", "extractedValues": { "$status_code": "400", "$request": "GET
-- /products/index.html HTTP/1.0", "$7": "1534", "$4": "10/Oct/2000:13:25:15
-- -0700", "$2": "-", "$3": "frank", "$1": "127.0.0.1" } } ] } Test a metric
-- filter pattern that matches occurrences of "[ERROR]" in log events The
-- following is an example of a TestMetricFilter request and response. POST /
-- HTTP/1.1 Host: logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern":
-- "\"[ERROR]\"", "logEventMessages": [ "02 May 2014 00:34:12,525 [INFO]
-- Starting the application", "02 May 2014 00:35:14,245 [DEBUG] Database
-- connection established", "02 May 2014 00:34:14,663 [INFO] Executing SQL
-- Query", "02 May 2014 00:34:16,142 [ERROR] Unhanded exception:
-- InvalidQueryException", "02 May 2014 00:34:16,224 [ERROR] Terminating the
-- application" ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 3, "eventMessage": "02 May 2014 00:34:16,142 [ERROR]
-- Unhanded exception: InvalidQueryException", "extractedValues": {} }, {
-- "eventNumber": 4, "eventMessage": "02 May 2014 00:34:16,224 [ERROR]
-- Terminating the application", "extractedValues": {} } ] } Test a metric
-- filter pattern that matches occurrences of "[ERROR]" and "Exception" in log
-- events The following is an example of a TestMetricFilter request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern":
-- "\"[ERROR]\" Exception", "logEventMessages": [ "02 May 2014 00:34:12,525
-- [INFO] Starting the application", "02 May 2014 00:35:14,245 [DEBUG]
-- Database connection established", "02 May 2014 00:34:14,663 [INFO]
-- Executing SQL Query", "02 May 2014 00:34:16,142 [ERROR] Unhanded exception:
-- InvalidQueryException", "02 May 2014 00:34:16,224 [ERROR] Terminating the
-- application" ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 3, "eventMessage": "02 May 2014 00:34:16,142 [ERROR]
-- Unhanded exception: InvalidQueryException", "extractedValues": {} } ] }.
module Network.AWS.CloudWatchLogs.V2014_03_28.TestMetricFilter
    (
    -- * Request
      TestMetricFilter
    -- ** Request constructor
    , testMetricFilter
    -- ** Request lenses
    , tmfrFilterPattern
    , tmfrLogEventMessages

    -- * Response
    , TestMetricFilterResponse
    -- ** Response lenses
    , tmfsMatches
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'TestMetricFilter' request.
testMetricFilter :: Text -- ^ 'tmfrFilterPattern'
                 -> [Text] -- ^ 'tmfrLogEventMessages'
                 -> TestMetricFilter
testMetricFilter p1 p2 = TestMetricFilter
    { _tmfrFilterPattern = p1
    , _tmfrLogEventMessages = p2
    }
{-# INLINE testMetricFilter #-}

data TestMetricFilter = TestMetricFilter
    { _tmfrFilterPattern :: Text
    , _tmfrLogEventMessages :: [Text]
    } deriving (Show, Generic)

tmfrFilterPattern :: Lens' TestMetricFilter (Text)
tmfrFilterPattern f x =
    f (_tmfrFilterPattern x)
        <&> \y -> x { _tmfrFilterPattern = y }
{-# INLINE tmfrFilterPattern #-}

tmfrLogEventMessages :: Lens' TestMetricFilter ([Text])
tmfrLogEventMessages f x =
    f (_tmfrLogEventMessages x)
        <&> \y -> x { _tmfrLogEventMessages = y }
{-# INLINE tmfrLogEventMessages #-}

instance ToPath TestMetricFilter

instance ToQuery TestMetricFilter

instance ToHeaders TestMetricFilter

instance ToJSON TestMetricFilter

data TestMetricFilterResponse = TestMetricFilterResponse
    { _tmfsMatches :: [MetricFilterMatchRecord]
    } deriving (Show, Generic)

tmfsMatches :: Lens' TestMetricFilterResponse ([MetricFilterMatchRecord])
tmfsMatches f x =
    f (_tmfsMatches x)
        <&> \y -> x { _tmfsMatches = y }
{-# INLINE tmfsMatches #-}

instance FromJSON TestMetricFilterResponse

instance AWSRequest TestMetricFilter where
    type Sv TestMetricFilter = CloudWatchLogs
    type Rs TestMetricFilter = TestMetricFilterResponse

    request = get
    response _ = jsonResponse
