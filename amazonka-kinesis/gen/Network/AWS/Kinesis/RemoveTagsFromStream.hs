{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.RemoveTagsFromStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes tags from the specified Amazon Kinesis stream. If you specify a tag
-- that does not exist, it is ignored. To remove tags from a stream The
-- following example removes the specified tag from the specified stream. POST
-- / HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.RemoveTagsFromStream { "StreamName": "exampleStreamName",
-- "TagKeys": ["Project", "Environment"] } HTTP/1.1 200 OK x-amzn-RequestId:
-- Content-Type: application/x-amz-json-1.1 Content-Length: Date: ]]>.
module Network.AWS.Kinesis.RemoveTagsFromStream
    (
    -- * Request
      RemoveTagsFromStream
    -- ** Request constructor
    , removeTagsFromStream
    -- ** Request lenses
    , rtfsStreamName
    , rtfsTagKeys

    -- * Response
    , RemoveTagsFromStreamResponse
    -- ** Response constructor
    , removeTagsFromStreamResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input for RemoveTagsFromStream.
data RemoveTagsFromStream = RemoveTagsFromStream
    { _rtfsStreamName :: Text
    , _rtfsTagKeys :: List1 Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveTagsFromStream' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamName ::@ @Text@
--
-- * @TagKeys ::@ @List1 Text@
--
removeTagsFromStream :: Text -- ^ 'rtfsStreamName'
                     -> List1 Text -- ^ 'rtfsTagKeys'
                     -> RemoveTagsFromStream
removeTagsFromStream p1 p2 = RemoveTagsFromStream
    { _rtfsStreamName = p1
    , _rtfsTagKeys = p2
    }

-- | The name of the stream.
rtfsStreamName :: Lens' RemoveTagsFromStream Text
rtfsStreamName = lens _rtfsStreamName (\s a -> s { _rtfsStreamName = a })

-- | A list of tag keys. Each corresponding tag is deleted from the stream.
rtfsTagKeys :: Lens' RemoveTagsFromStream (List1 Text)
rtfsTagKeys = lens _rtfsTagKeys (\s a -> s { _rtfsTagKeys = a })

instance ToPath RemoveTagsFromStream

instance ToQuery RemoveTagsFromStream

instance ToHeaders RemoveTagsFromStream

instance ToJSON RemoveTagsFromStream

data RemoveTagsFromStreamResponse = RemoveTagsFromStreamResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveTagsFromStreamResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
removeTagsFromStreamResponse :: RemoveTagsFromStreamResponse
removeTagsFromStreamResponse = RemoveTagsFromStreamResponse

instance AWSRequest RemoveTagsFromStream where
    type Sv RemoveTagsFromStream = Kinesis
    type Rs RemoveTagsFromStream = RemoveTagsFromStreamResponse

    request = get
    response _ = nullaryResponse RemoveTagsFromStreamResponse
