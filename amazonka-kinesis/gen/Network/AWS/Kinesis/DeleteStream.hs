{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Kinesis.DeleteStream
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a stream and all its shards and data. You must shut down any
-- applications that are operating on the stream before you delete the
-- stream. If an application attempts to operate on a deleted stream, it
-- will receive the exception @ResourceNotFoundException@.
--
-- If the stream is in the @ACTIVE@ state, you can delete it. After a
-- @DeleteStream@ request, the specified stream is in the @DELETING@ state
-- until Amazon Kinesis completes the deletion.
--
-- __Note:__ Amazon Kinesis might continue to accept data read and write
-- operations, such as PutRecord, PutRecords, and GetRecords, on a stream
-- in the @DELETING@ state until the stream deletion is complete.
--
-- When you delete a stream, any shards in that stream are also deleted,
-- and any tags are dissociated from the stream.
--
-- You can use the DescribeStream operation to check the state of the
-- stream, which is returned in @StreamStatus@.
--
-- DeleteStream has a limit of 5 transactions per second per account.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_DeleteStream.html>
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

import           Network.AWS.Kinesis.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for @DeleteStream@.
--
-- /See:/ 'deleteStream' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsStreamName'
newtype DeleteStream = DeleteStream'
    { _dsStreamName :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteStream' smart constructor.
deleteStream :: Text -> DeleteStream
deleteStream pStreamName =
    DeleteStream'
    { _dsStreamName = pStreamName
    }

-- | The name of the stream to delete.
dsStreamName :: Lens' DeleteStream Text
dsStreamName = lens _dsStreamName (\ s a -> s{_dsStreamName = a});

instance AWSRequest DeleteStream where
        type Sv DeleteStream = Kinesis
        type Rs DeleteStream = DeleteStreamResponse
        request = postJSON
        response = receiveNull DeleteStreamResponse'

instance ToHeaders DeleteStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.DeleteStream" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteStream where
        toJSON DeleteStream'{..}
          = object ["StreamName" .= _dsStreamName]

instance ToPath DeleteStream where
        toPath = const "/"

instance ToQuery DeleteStream where
        toQuery = const mempty

-- | /See:/ 'deleteStreamResponse' smart constructor.
data DeleteStreamResponse =
    DeleteStreamResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteStreamResponse' smart constructor.
deleteStreamResponse :: DeleteStreamResponse
deleteStreamResponse = DeleteStreamResponse'
