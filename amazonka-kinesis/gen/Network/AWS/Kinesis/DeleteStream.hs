{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Kinesis.DeleteStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a stream and all its shards and data. You must shut down any
-- applications that are operating on the stream before you delete the stream.
-- If an application attempts to operate on a deleted stream, it will receive
-- the exception ResourceNotFoundException. If the stream is in the ACTIVE
-- state, you can delete it. After a DeleteStream request, the specified
-- stream is in the DELETING state until Amazon Kinesis completes the
-- deletion. Note: Amazon Kinesis might continue to accept data read and write
-- operations, such as PutRecord and GetRecords, on a stream in the DELETING
-- state until the stream deletion is complete. When you delete a stream, any
-- shards in that stream are also deleted, and any tags are dissociated from
-- the stream. You can use the DescribeStream operation to check the state of
-- the stream, which is returned in StreamStatus. DeleteStream has a limit of
-- 5 transactions per second per account.
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

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Kinesis.Types

newtype DeleteStream = DeleteStream
    { _dsStreamName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteStream' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsStreamName' @::@ 'Text'
--
deleteStream :: Text -- ^ 'dsStreamName'
             -> DeleteStream
deleteStream p1 = DeleteStream
    { _dsStreamName = p1
    }

-- | The name of the stream to delete.
dsStreamName :: Lens' DeleteStream Text
dsStreamName = lens _dsStreamName (\s a -> s { _dsStreamName = a })

instance ToPath DeleteStream where
    toPath = const "/"

instance ToQuery DeleteStream where
    toQuery = const mempty

instance ToHeaders DeleteStream

instance ToBody DeleteStream where
    toBody = toBody . encode . _dsStreamName

data DeleteStreamResponse = DeleteStreamResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteStreamResponse' constructor.
deleteStreamResponse :: DeleteStreamResponse
deleteStreamResponse = DeleteStreamResponse

instance AWSRequest DeleteStream where
    type Sv DeleteStream = Kinesis
    type Rs DeleteStream = DeleteStreamResponse

    request  = post
    response = nullaryResponse DeleteStreamResponse
