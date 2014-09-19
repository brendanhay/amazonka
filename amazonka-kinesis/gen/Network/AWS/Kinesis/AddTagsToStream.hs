{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.AddTagsToStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds or updates tags for the specified Amazon Kinesis stream. Each stream
-- can have up to 10 tags. If tags have already been assigned to the stream,
-- AddTagsToStream overwrites any existing tags that correspond to the
-- specified tag keys. To add tags to a stream The following example adds two
-- tags to the specified stream. POST / HTTP/1.1 Host: kinesis.. x-amz-Date:
-- Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.AddTagsToStream { "StreamName": "exampleStreamName",
-- "Tags": { "Project" : "myProject", "Environment" : "Production" } }
-- HTTP/1.1 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
module Network.AWS.Kinesis.AddTagsToStream
    (
    -- * Request
      AddTagsToStream
    -- ** Request constructor
    , addTagsToStream
    -- ** Request lenses
    , attsStreamName
    , attsTags

    -- * Response
    , AddTagsToStreamResponse
    -- ** Response constructor
    , addTagsToStreamResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input for AddTagsToStream.
data AddTagsToStream = AddTagsToStream
    { _attsStreamName :: Text
    , _attsTags :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddTagsToStream' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamName ::@ @Text@
--
-- * @Tags ::@ @Map Text Text@
--
addTagsToStream :: Text -- ^ 'attsStreamName'
                -> Map Text Text -- ^ 'attsTags'
                -> AddTagsToStream
addTagsToStream p1 p2 = AddTagsToStream
    { _attsStreamName = p1
    , _attsTags = p2
    }

-- | The name of the stream.
attsStreamName :: Lens' AddTagsToStream Text
attsStreamName = lens _attsStreamName (\s a -> s { _attsStreamName = a })

-- | The set of key-value pairs to use to create the tags.
attsTags :: Lens' AddTagsToStream (Map Text Text)
attsTags = lens _attsTags (\s a -> s { _attsTags = a })

instance ToPath AddTagsToStream

instance ToQuery AddTagsToStream

instance ToHeaders AddTagsToStream

instance ToJSON AddTagsToStream

data AddTagsToStreamResponse = AddTagsToStreamResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddTagsToStreamResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
addTagsToStreamResponse :: AddTagsToStreamResponse
addTagsToStreamResponse = AddTagsToStreamResponse

instance AWSRequest AddTagsToStream where
    type Sv AddTagsToStream = Kinesis
    type Rs AddTagsToStream = AddTagsToStreamResponse

    request = get
    response _ = nullaryResponse AddTagsToStreamResponse
