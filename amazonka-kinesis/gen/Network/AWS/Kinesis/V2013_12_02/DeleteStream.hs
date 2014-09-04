{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.V2013_12_02.DeleteStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation deletes a stream and all of its shards and data. You must
-- shut down any applications that are operating on the stream before you
-- delete the stream. If an application attempts to operate on a deleted
-- stream, it will receive the exception ResourceNotFoundException. If the
-- stream is in the ACTIVE state, you can delete it. After a DeleteStream
-- request, the specified stream is in the DELETING state until Amazon Kinesis
-- completes the deletion. Note: Amazon Kinesis might continue to accept data
-- read and write operations, such as PutRecord and GetRecords, on a stream in
-- the DELETING state until the stream deletion is complete. When you delete a
-- stream, any shards in that stream are also deleted. You can use the
-- DescribeStream operation to check the state of the stream, which is
-- returned in StreamStatus. DeleteStream has a limit of 5 transactions per
-- second per account. Delete a Stream The following is an example of an
-- Amazon Kinesis DeleteStream request and response. POST / HTTP/1.1 Host:
-- kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.DeleteStream { "StreamName":"exampleStreamName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
module Network.AWS.Kinesis.V2013_12_02.DeleteStream
    (
    -- * Request
      DeleteStream
    -- ** Request constructor
    , mkDeleteStreamInput
    -- ** Request lenses
    , dsiStreamName

    -- * Response
    , DeleteStreamResponse
    ) where

import           Network.AWS.Kinesis.V2013_12_02.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteStream' request.
mkDeleteStreamInput :: Text -- ^ 'dsiStreamName'
                    -> DeleteStream
mkDeleteStreamInput p1 = DeleteStream
    { _dsiStreamName = p1
    }
{-# INLINE mkDeleteStreamInput #-}

newtype DeleteStream = DeleteStream
    { _dsiStreamName :: Text
      -- ^ The name of the stream to delete.
    } deriving (Show, Generic)

-- | The name of the stream to delete.
dsiStreamName :: Lens' DeleteStream (Text)
dsiStreamName = lens _dsiStreamName (\s a -> s { _dsiStreamName = a })
{-# INLINE dsiStreamName #-}

instance ToPath DeleteStream

instance ToQuery DeleteStream

instance ToHeaders DeleteStream

instance ToJSON DeleteStream

data DeleteStreamResponse = DeleteStreamResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteStream where
    type Sv DeleteStream = Kinesis
    type Rs DeleteStream = DeleteStreamResponse

    request = get
    response _ = nullaryResponse DeleteStreamResponse
