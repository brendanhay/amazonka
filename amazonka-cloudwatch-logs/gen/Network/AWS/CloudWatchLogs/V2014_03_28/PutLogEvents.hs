{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.PutLogEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Uploads a batch of log events to the specified log stream. Every
-- PutLogEvents request must include the sequenceToken obtained from the
-- response of the previous request. An upload in a newly created log stream
-- does not require a sequenceToken. The batch of events must satisfy the
-- following constraints: The maximum batch size is 32,768 bytes, and this
-- size is calculated as the sum of all event messages in UTF-8, plus 26 bytes
-- for each log event. None of the log events in the batch can be more than 2
-- hours in the future. None of the log events in the batch can be older than
-- 14 days or the retention period of the log group. The log events in the
-- batch must be in chronological ordered by their timestamp. The maximum
-- number of log events in a batch is 1,000. Upload a batch of log events into
-- a log stream The following is an example of a PutLogEvents request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.PutLogEvents { "logGroupName":
-- "exampleLogGroupName", "logStreamName": "exampleLogStreamName",
-- "logEvents": [ { "timestamp": 1396035378988, "message": "Example Event 1"
-- }, { "timestamp": 1396035378988, "message": "Example Event 2" }, {
-- "timestamp": 1396035378989, "message": "Example Event 3" } ] } HTTP/1.1 200
-- OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]> { "nextSequenceToken":
-- "49536701251539826331025683274032969384950891766572122113" }.
module Network.AWS.CloudWatchLogs.V2014_03_28.PutLogEvents
    (
    -- * Request
      PutLogEvents
    -- ** Request constructor
    , putLogEvents
    -- ** Request lenses
    , plerLogEvents
    , plerLogGroupName
    , plerLogStreamName
    , plerSequenceToken

    -- * Response
    , PutLogEventsResponse
    -- ** Response lenses
    , plesNextSequenceToken
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'PutLogEvents' request.
putLogEvents :: [InputLogEvent] -- ^ 'plerLogEvents'
             -> Text -- ^ 'plerLogGroupName'
             -> Text -- ^ 'plerLogStreamName'
             -> PutLogEvents
putLogEvents p1 p2 p3 = PutLogEvents
    { _plerLogEvents = p1
    , _plerLogGroupName = p2
    , _plerLogStreamName = p3
    , _plerSequenceToken = Nothing
    }
{-# INLINE putLogEvents #-}

data PutLogEvents = PutLogEvents
    { _plerLogEvents :: [InputLogEvent]
      -- ^ A list of events belonging to a log stream.
    , _plerLogGroupName :: Text
    , _plerLogStreamName :: Text
    , _plerSequenceToken :: Maybe Text
      -- ^ A string token that must be obtained from the response of the
      -- previous PutLogEvents request.
    } deriving (Show, Generic)

-- | A list of events belonging to a log stream.
plerLogEvents :: Lens' PutLogEvents ([InputLogEvent])
plerLogEvents f x =
    f (_plerLogEvents x)
        <&> \y -> x { _plerLogEvents = y }
{-# INLINE plerLogEvents #-}

plerLogGroupName :: Lens' PutLogEvents (Text)
plerLogGroupName f x =
    f (_plerLogGroupName x)
        <&> \y -> x { _plerLogGroupName = y }
{-# INLINE plerLogGroupName #-}

plerLogStreamName :: Lens' PutLogEvents (Text)
plerLogStreamName f x =
    f (_plerLogStreamName x)
        <&> \y -> x { _plerLogStreamName = y }
{-# INLINE plerLogStreamName #-}

-- | A string token that must be obtained from the response of the previous
-- PutLogEvents request.
plerSequenceToken :: Lens' PutLogEvents (Maybe Text)
plerSequenceToken f x =
    f (_plerSequenceToken x)
        <&> \y -> x { _plerSequenceToken = y }
{-# INLINE plerSequenceToken #-}

instance ToPath PutLogEvents

instance ToQuery PutLogEvents

instance ToHeaders PutLogEvents

instance ToJSON PutLogEvents

data PutLogEventsResponse = PutLogEventsResponse
    { _plesNextSequenceToken :: Maybe Text
      -- ^ A string token used for making PutLogEvents requests. A
      -- sequenceToken can only be used once, and PutLogEvents requests
      -- must include the sequenceToken obtained from the response of the
      -- previous request.
    } deriving (Show, Generic)

-- | A string token used for making PutLogEvents requests. A sequenceToken can
-- only be used once, and PutLogEvents requests must include the sequenceToken
-- obtained from the response of the previous request.
plesNextSequenceToken :: Lens' PutLogEventsResponse (Maybe Text)
plesNextSequenceToken f x =
    f (_plesNextSequenceToken x)
        <&> \y -> x { _plesNextSequenceToken = y }
{-# INLINE plesNextSequenceToken #-}

instance FromJSON PutLogEventsResponse

instance AWSRequest PutLogEvents where
    type Sv PutLogEvents = CloudWatchLogs
    type Rs PutLogEvents = PutLogEventsResponse

    request = get
    response _ = jsonResponse
