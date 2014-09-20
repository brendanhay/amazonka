{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.CreateLogStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new log stream in the specified log group. The name of the log
-- stream must be unique within the log group. There is no limit on the number
-- of log streams that can exist in a log group. You must use the following
-- guidelines when naming a log stream: Log stream names can be between 1 and
-- 512 characters long. The ':' colon character is not allowed. Create a new
-- Log Stream The following is an example of a CreateLogStream request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.CreateLogStream { "logGroupName":
-- "exampleLogGroupName", "logStreamName": "exampleLogStreamName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
module Network.AWS.CloudWatchLogs.CreateLogStream
    (
    -- * Request
      CreateLogStream
    -- ** Request constructor
    , createLogStream
    -- ** Request lenses
    , clsLogGroupName
    , clsLogStreamName

    -- * Response
    , CreateLogStreamResponse
    -- ** Response constructor
    , createLogStreamResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CreateLogStream = CreateLogStream
    { _clsLogGroupName :: Text
    , _clsLogStreamName :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLogStream' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LogGroupName ::@ @Text@
--
-- * @LogStreamName ::@ @Text@
--
createLogStream :: Text -- ^ 'clsLogGroupName'
                -> Text -- ^ 'clsLogStreamName'
                -> CreateLogStream
createLogStream p1 p2 = CreateLogStream
    { _clsLogGroupName = p1
    , _clsLogStreamName = p2
    }

clsLogGroupName :: Lens' CreateLogStream Text
clsLogGroupName = lens _clsLogGroupName (\s a -> s { _clsLogGroupName = a })

clsLogStreamName :: Lens' CreateLogStream Text
clsLogStreamName =
    lens _clsLogStreamName (\s a -> s { _clsLogStreamName = a })

instance ToPath CreateLogStream

instance ToQuery CreateLogStream

instance ToHeaders CreateLogStream

instance ToJSON CreateLogStream

data CreateLogStreamResponse = CreateLogStreamResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLogStreamResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
createLogStreamResponse :: CreateLogStreamResponse
createLogStreamResponse = CreateLogStreamResponse

instance AWSRequest CreateLogStream where
    type Sv CreateLogStream = CloudWatchLogs
    type Rs CreateLogStream = CreateLogStreamResponse

    request = get
    response _ = nullaryResponse CreateLogStreamResponse
