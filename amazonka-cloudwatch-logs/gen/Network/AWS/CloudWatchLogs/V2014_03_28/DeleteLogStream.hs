{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.DeleteLogStream
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
module Network.AWS.CloudWatchLogs.V2014_03_28.DeleteLogStream
    (
    -- * Request
      DeleteLogStream
    -- ** Request constructor
    , mkDeleteLogStreamRequest
    -- ** Request lenses
    , dlsrLogGroupName
    , dlsrLogStreamName

    -- * Response
    , DeleteLogStreamResponse
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLogStream' request.
mkDeleteLogStreamRequest :: Text -- ^ 'dlsrLogGroupName'
                         -> Text -- ^ 'dlsrLogStreamName'
                         -> DeleteLogStream
mkDeleteLogStreamRequest p1 p2 = DeleteLogStream
    { _dlsrLogGroupName = p1
    , _dlsrLogStreamName = p2
    }
{-# INLINE mkDeleteLogStreamRequest #-}

data DeleteLogStream = DeleteLogStream
    { _dlsrLogGroupName :: Text
    , _dlsrLogStreamName :: Text
    } deriving (Show, Generic)

dlsrLogGroupName :: Lens' DeleteLogStream (Text)
dlsrLogGroupName = lens _dlsrLogGroupName (\s a -> s { _dlsrLogGroupName = a })
{-# INLINE dlsrLogGroupName #-}

dlsrLogStreamName :: Lens' DeleteLogStream (Text)
dlsrLogStreamName = lens _dlsrLogStreamName (\s a -> s { _dlsrLogStreamName = a })
{-# INLINE dlsrLogStreamName #-}

instance ToPath DeleteLogStream

instance ToQuery DeleteLogStream

instance ToHeaders DeleteLogStream

instance ToJSON DeleteLogStream

data DeleteLogStreamResponse = DeleteLogStreamResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteLogStream where
    type Sv DeleteLogStream = CloudWatchLogs
    type Rs DeleteLogStream = DeleteLogStreamResponse

    request = get
    response _ = nullaryResponse DeleteLogStreamResponse
