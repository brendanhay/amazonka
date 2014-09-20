{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.ListTagsForStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the tags for the specified Amazon Kinesis stream. To list the tags
-- for a stream The following example lists the tags for the specified stream.
-- POST / HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.ListTagsForStream { "StreamName": "exampleStreamName" }
-- HTTP/1.1 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]> { "HasMoreTags": "false", "Tags" : [ { "Key":
-- "Project", "Value": "myProject" }, { "Key": "Environment", "Value":
-- "Production" } ] }.
module Network.AWS.Kinesis.ListTagsForStream
    (
    -- * Request
      ListTagsForStream
    -- ** Request constructor
    , listTagsForStream
    -- ** Request lenses
    , ltfsStreamName
    , ltfsExclusiveStartTagKey
    , ltfsLimit

    -- * Response
    , ListTagsForStreamResponse
    -- ** Response constructor
    , listTagsForStreamResponse
    -- ** Response lenses
    , ltfsrTags
    , ltfsrHasMoreTags
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input for ListTagsForStream.
data ListTagsForStream = ListTagsForStream
    { _ltfsStreamName :: Text
    , _ltfsExclusiveStartTagKey :: Maybe Text
    , _ltfsLimit :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTagsForStream' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamName ::@ @Text@
--
-- * @ExclusiveStartTagKey ::@ @Maybe Text@
--
-- * @Limit ::@ @Maybe Integer@
--
listTagsForStream :: Text -- ^ 'ltfsStreamName'
                  -> ListTagsForStream
listTagsForStream p1 = ListTagsForStream
    { _ltfsStreamName = p1
    , _ltfsExclusiveStartTagKey = Nothing
    , _ltfsLimit = Nothing
    }

-- | The name of the stream.
ltfsStreamName :: Lens' ListTagsForStream Text
ltfsStreamName = lens _ltfsStreamName (\s a -> s { _ltfsStreamName = a })

-- | The key to use as the starting point for the list of tags. If this
-- parameter is set, ListTagsForStream gets all tags that occur after
-- ExclusiveStartTagKey.
ltfsExclusiveStartTagKey :: Lens' ListTagsForStream (Maybe Text)
ltfsExclusiveStartTagKey =
    lens _ltfsExclusiveStartTagKey
         (\s a -> s { _ltfsExclusiveStartTagKey = a })

-- | The number of tags to return. If this number is less than the total number
-- of tags associated with the stream, HasMoreTags is set to true. To list
-- additional tags, set ExclusiveStartTagKey to the last key in the response.
ltfsLimit :: Lens' ListTagsForStream (Maybe Integer)
ltfsLimit = lens _ltfsLimit (\s a -> s { _ltfsLimit = a })

instance ToPath ListTagsForStream

instance ToQuery ListTagsForStream

instance ToHeaders ListTagsForStream

instance ToJSON ListTagsForStream

-- | Represents the output for ListTagsForStream.
data ListTagsForStreamResponse = ListTagsForStreamResponse
    { _ltfsrTags :: [Tag]
    , _ltfsrHasMoreTags :: !Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTagsForStreamResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Tags ::@ @[Tag]@
--
-- * @HasMoreTags ::@ @Bool@
--
listTagsForStreamResponse :: [Tag] -- ^ 'ltfsrTags'
                          -> Bool -- ^ 'ltfsrHasMoreTags'
                          -> ListTagsForStreamResponse
listTagsForStreamResponse p1 p2 = ListTagsForStreamResponse
    { _ltfsrTags = p1
    , _ltfsrHasMoreTags = p2
    }

-- | A list of tags associated with StreamName, starting with the first tag
-- after ExclusiveStartTagKey and up to the specified Limit.
ltfsrTags :: Lens' ListTagsForStreamResponse [Tag]
ltfsrTags = lens _ltfsrTags (\s a -> s { _ltfsrTags = a })

-- | If set to true, more tags are available. To request additional tags, set
-- ExclusiveStartTagKey to the key of the last tag returned.
ltfsrHasMoreTags :: Lens' ListTagsForStreamResponse Bool
ltfsrHasMoreTags =
    lens _ltfsrHasMoreTags (\s a -> s { _ltfsrHasMoreTags = a })

instance FromJSON ListTagsForStreamResponse

instance AWSRequest ListTagsForStream where
    type Sv ListTagsForStream = Kinesis
    type Rs ListTagsForStream = ListTagsForStreamResponse

    request = get
    response _ = jsonResponse
