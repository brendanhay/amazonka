{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.DeleteLogStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a log stream and permanently deletes all the archived log events
-- associated with it. Delete a Log Stream The following is an example of a
-- DeleteLogStream request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DeleteLogStream { "logGroupName":
-- "exampleLogGroupName", "logStreamName": "exampleLogStreamName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
module Network.AWS.CloudWatchLogs.DeleteLogStream
    (
    -- * Request
      DeleteLogStream
    -- ** Request constructor
    , deleteLogStream
    -- ** Request lenses
    , dlsLogGroupName
    , dlsLogStreamName

    -- * Response
    , DeleteLogStreamResponse
    -- ** Response constructor
    , deleteLogStreamResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DeleteLogStream = DeleteLogStream
    { _dlsLogGroupName :: Text
    , _dlsLogStreamName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLogStream' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LogGroupName ::@ @Text@
--
-- * @LogStreamName ::@ @Text@
--
deleteLogStream :: Text -- ^ 'dlsLogGroupName'
                -> Text -- ^ 'dlsLogStreamName'
                -> DeleteLogStream
deleteLogStream p1 p2 = DeleteLogStream
    { _dlsLogGroupName = p1
    , _dlsLogStreamName = p2
    }

dlsLogGroupName :: Lens' DeleteLogStream Text
dlsLogGroupName = lens _dlsLogGroupName (\s a -> s { _dlsLogGroupName = a })

dlsLogStreamName :: Lens' DeleteLogStream Text
dlsLogStreamName =
    lens _dlsLogStreamName (\s a -> s { _dlsLogStreamName = a })

instance ToPath DeleteLogStream

instance ToQuery DeleteLogStream

instance ToHeaders DeleteLogStream

instance ToJSON DeleteLogStream

data DeleteLogStreamResponse = DeleteLogStreamResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLogStreamResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteLogStreamResponse :: DeleteLogStreamResponse
deleteLogStreamResponse = DeleteLogStreamResponse

instance AWSRequest DeleteLogStream where
    type Sv DeleteLogStream = CloudWatchLogs
    type Rs DeleteLogStream = DeleteLogStreamResponse

    request = get
    response _ = nullaryResponse DeleteLogStreamResponse
