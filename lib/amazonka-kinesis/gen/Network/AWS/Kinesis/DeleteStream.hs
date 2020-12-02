{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DeleteStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Kinesis data stream and all its shards and data. You must shut down any applications that are operating on the stream before you delete the stream. If an application attempts to operate on a deleted stream, it receives the exception @ResourceNotFoundException@ .
--
--
-- If the stream is in the @ACTIVE@ state, you can delete it. After a @DeleteStream@ request, the specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.
--
-- __Note:__ Kinesis Data Streams might continue to accept data read and write operations, such as 'PutRecord' , 'PutRecords' , and 'GetRecords' , on a stream in the @DELETING@ state until the stream deletion is complete.
--
-- When you delete a stream, any shards in that stream are also deleted, and any tags are dissociated from the stream.
--
-- You can use the 'DescribeStream' operation to check the state of the stream, which is returned in @StreamStatus@ .
--
-- 'DeleteStream' has a limit of five transactions per second per account.
--
module Network.AWS.Kinesis.DeleteStream
    (
    -- * Creating a Request
      deleteStream
    , DeleteStream
    -- * Request Lenses
    , dsStreamName

    -- * Destructuring the Response
    , deleteStreamResponse
    , DeleteStreamResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for 'DeleteStream' .
--
--
--
-- /See:/ 'deleteStream' smart constructor.
newtype DeleteStream = DeleteStream'
  { _dsStreamName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsStreamName' - The name of the stream to delete.
deleteStream
    :: Text -- ^ 'dsStreamName'
    -> DeleteStream
deleteStream pStreamName_ = DeleteStream' {_dsStreamName = pStreamName_}


-- | The name of the stream to delete.
dsStreamName :: Lens' DeleteStream Text
dsStreamName = lens _dsStreamName (\ s a -> s{_dsStreamName = a})

instance AWSRequest DeleteStream where
        type Rs DeleteStream = DeleteStreamResponse
        request = postJSON kinesis
        response = receiveNull DeleteStreamResponse'

instance Hashable DeleteStream where

instance NFData DeleteStream where

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
          = object
              (catMaybes [Just ("StreamName" .= _dsStreamName)])

instance ToPath DeleteStream where
        toPath = const "/"

instance ToQuery DeleteStream where
        toQuery = const mempty

-- | /See:/ 'deleteStreamResponse' smart constructor.
data DeleteStreamResponse =
  DeleteStreamResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStreamResponse' with the minimum fields required to make a request.
--
deleteStreamResponse
    :: DeleteStreamResponse
deleteStreamResponse = DeleteStreamResponse'


instance NFData DeleteStreamResponse where
