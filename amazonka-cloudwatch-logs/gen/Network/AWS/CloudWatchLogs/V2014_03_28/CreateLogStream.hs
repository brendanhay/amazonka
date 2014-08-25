{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.CreateLogStream
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
module Network.AWS.CloudWatchLogs.V2014_03_28.CreateLogStream where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data CreateLogStream = CreateLogStream
    { _clsrLogGroupName :: Text
    , _clsrLogStreamName :: Text
    } deriving (Show, Generic)

makeLenses ''CreateLogStream

instance ToPath CreateLogStream

instance ToQuery CreateLogStream

instance ToHeaders CreateLogStream

instance ToJSON CreateLogStream

data CreateLogStreamResponse = CreateLogStreamResponse
    deriving (Eq, Show, Generic)

makeLenses ''CreateLogStreamResponse

instance AWSRequest CreateLogStream where
    type Sv CreateLogStream = CloudWatchLogs
    type Rs CreateLogStream = CreateLogStreamResponse

    request = get
    response _ = nullaryResponse CreateLogStreamResponse
