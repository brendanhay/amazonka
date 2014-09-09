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
    , mkPutLogEvents
    -- ** Request lenses
    , pleLogGroupName
    , pleLogStreamName
    , pleLogEvents
    , pleSequenceToken

    -- * Response
    , PutLogEventsResponse
    -- ** Response constructor
    , mkPutLogEventsResponse
    -- ** Response lenses
    , plerNextSequenceToken
    ) where

import Network.AWS.CloudWatchLogs.V2014_03_28.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data PutLogEvents = PutLogEvents
    { _pleLogGroupName :: Text
    , _pleLogStreamName :: Text
    , _pleLogEvents :: List1 InputLogEvent
    , _pleSequenceToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutLogEvents' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LogGroupName ::@ @Text@
--
-- * @LogStreamName ::@ @Text@
--
-- * @LogEvents ::@ @List1 InputLogEvent@
--
-- * @SequenceToken ::@ @Maybe Text@
--
mkPutLogEvents :: Text -- ^ 'pleLogGroupName'
               -> Text -- ^ 'pleLogStreamName'
               -> List1 InputLogEvent -- ^ 'pleLogEvents'
               -> PutLogEvents
mkPutLogEvents p1 p2 p3 = PutLogEvents
    { _pleLogGroupName = p1
    , _pleLogStreamName = p2
    , _pleLogEvents = p3
    , _pleSequenceToken = Nothing
    }

pleLogGroupName :: Lens' PutLogEvents Text
pleLogGroupName = lens _pleLogGroupName (\s a -> s { _pleLogGroupName = a })

pleLogStreamName :: Lens' PutLogEvents Text
pleLogStreamName =
    lens _pleLogStreamName (\s a -> s { _pleLogStreamName = a })

-- | A list of events belonging to a log stream.
pleLogEvents :: Lens' PutLogEvents (List1 InputLogEvent)
pleLogEvents = lens _pleLogEvents (\s a -> s { _pleLogEvents = a })

-- | A string token that must be obtained from the response of the previous
-- PutLogEvents request.
pleSequenceToken :: Lens' PutLogEvents (Maybe Text)
pleSequenceToken =
    lens _pleSequenceToken (\s a -> s { _pleSequenceToken = a })

instance ToPath PutLogEvents

instance ToQuery PutLogEvents

instance ToHeaders PutLogEvents

instance ToJSON PutLogEvents

newtype PutLogEventsResponse = PutLogEventsResponse
    { _plerNextSequenceToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutLogEventsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NextSequenceToken ::@ @Maybe Text@
--
mkPutLogEventsResponse :: PutLogEventsResponse
mkPutLogEventsResponse = PutLogEventsResponse
    { _plerNextSequenceToken = Nothing
    }

-- | A string token used for making PutLogEvents requests. A sequenceToken can
-- only be used once, and PutLogEvents requests must include the sequenceToken
-- obtained from the response of the previous request.
plerNextSequenceToken :: Lens' PutLogEventsResponse (Maybe Text)
plerNextSequenceToken =
    lens _plerNextSequenceToken (\s a -> s { _plerNextSequenceToken = a })

instance FromJSON PutLogEventsResponse

instance AWSRequest PutLogEvents where
    type Sv PutLogEvents = CloudWatchLogs
    type Rs PutLogEvents = PutLogEventsResponse

    request = get
    response _ = jsonResponse
