{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.DeleteStream
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
module Network.AWS.Kinesis.DeleteStream
    (
    -- * Request
      DeleteStream
    -- ** Request constructor
    , deleteStream
    -- ** Request lenses
    , dsStreamName

    -- * Response
    , DeleteStreamResponse
    -- ** Response constructor
    , deleteStreamResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a DeleteStream operation.
newtype DeleteStream = DeleteStream
    { _dsStreamName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteStream' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamName ::@ @Text@
--
deleteStream :: Text -- ^ 'dsStreamName'
               -> DeleteStream
deleteStream p1 = DeleteStream
    { _dsStreamName = p1
    }

-- | The name of the stream to delete.
dsStreamName :: Lens' DeleteStream Text
dsStreamName = lens _dsStreamName (\s a -> s { _dsStreamName = a })

instance ToPath DeleteStream

instance ToQuery DeleteStream

instance ToHeaders DeleteStream

instance ToJSON DeleteStream

data DeleteStreamResponse = DeleteStreamResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteStreamResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteStreamResponse :: DeleteStreamResponse
deleteStreamResponse = DeleteStreamResponse

instance AWSRequest DeleteStream where
    type Sv DeleteStream = Kinesis
    type Rs DeleteStream = DeleteStreamResponse

    request = get
    response _ = nullaryResponse DeleteStreamResponse
