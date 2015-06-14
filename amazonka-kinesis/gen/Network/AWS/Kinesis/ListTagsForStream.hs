{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Kinesis.ListTagsForStream
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

-- | Lists the tags for the specified Amazon Kinesis stream.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_ListTagsForStream.html>
module Network.AWS.Kinesis.ListTagsForStream
    (
    -- * Request
      ListTagsForStream
    -- ** Request constructor
    , listTagsForStream
    -- ** Request lenses
    , ltfsLimit
    , ltfsExclusiveStartTagKey
    , ltfsStreamName

    -- * Response
    , ListTagsForStreamResponse
    -- ** Response constructor
    , listTagsForStreamResponse
    -- ** Response lenses
    , ltfsrTags
    , ltfsrHasMoreTags
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Kinesis.Types

-- | /See:/ 'listTagsForStream' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfsLimit'
--
-- * 'ltfsExclusiveStartTagKey'
--
-- * 'ltfsStreamName'
data ListTagsForStream = ListTagsForStream'{_ltfsLimit :: Maybe Nat, _ltfsExclusiveStartTagKey :: Maybe Text, _ltfsStreamName :: Text} deriving (Eq, Read, Show)

-- | 'ListTagsForStream' smart constructor.
listTagsForStream :: Text -> ListTagsForStream
listTagsForStream pStreamName = ListTagsForStream'{_ltfsLimit = Nothing, _ltfsExclusiveStartTagKey = Nothing, _ltfsStreamName = pStreamName};

-- | The number of tags to return. If this number is less than the total
-- number of tags associated with the stream, @HasMoreTags@ is set to
-- @true@. To list additional tags, set @ExclusiveStartTagKey@ to the last
-- key in the response.
ltfsLimit :: Lens' ListTagsForStream (Maybe Natural)
ltfsLimit = lens _ltfsLimit (\ s a -> s{_ltfsLimit = a}) . mapping _Nat;

-- | The key to use as the starting point for the list of tags. If this
-- parameter is set, @ListTagsForStream@ gets all tags that occur after
-- @ExclusiveStartTagKey@.
ltfsExclusiveStartTagKey :: Lens' ListTagsForStream (Maybe Text)
ltfsExclusiveStartTagKey = lens _ltfsExclusiveStartTagKey (\ s a -> s{_ltfsExclusiveStartTagKey = a});

-- | The name of the stream.
ltfsStreamName :: Lens' ListTagsForStream Text
ltfsStreamName = lens _ltfsStreamName (\ s a -> s{_ltfsStreamName = a});

instance AWSRequest ListTagsForStream where
        type Sv ListTagsForStream = Kinesis
        type Rs ListTagsForStream = ListTagsForStreamResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForStreamResponse' <$>
                   x .?> "Tags" .!@ mempty <*> x .:> "HasMoreTags")

instance ToHeaders ListTagsForStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.ListTagsForStream" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagsForStream where
        toJSON ListTagsForStream'{..}
          = object
              ["Limit" .= _ltfsLimit,
               "ExclusiveStartTagKey" .= _ltfsExclusiveStartTagKey,
               "StreamName" .= _ltfsStreamName]

instance ToPath ListTagsForStream where
        toPath = const "/"

instance ToQuery ListTagsForStream where
        toQuery = const mempty

-- | /See:/ 'listTagsForStreamResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfsrTags'
--
-- * 'ltfsrHasMoreTags'
data ListTagsForStreamResponse = ListTagsForStreamResponse'{_ltfsrTags :: [Tag], _ltfsrHasMoreTags :: Bool} deriving (Eq, Read, Show)

-- | 'ListTagsForStreamResponse' smart constructor.
listTagsForStreamResponse :: Bool -> ListTagsForStreamResponse
listTagsForStreamResponse pHasMoreTags = ListTagsForStreamResponse'{_ltfsrTags = mempty, _ltfsrHasMoreTags = pHasMoreTags};

-- | A list of tags associated with @StreamName@, starting with the first tag
-- after @ExclusiveStartTagKey@ and up to the specified @Limit@.
ltfsrTags :: Lens' ListTagsForStreamResponse [Tag]
ltfsrTags = lens _ltfsrTags (\ s a -> s{_ltfsrTags = a});

-- | If set to @true@, more tags are available. To request additional tags,
-- set @ExclusiveStartTagKey@ to the key of the last tag returned.
ltfsrHasMoreTags :: Lens' ListTagsForStreamResponse Bool
ltfsrHasMoreTags = lens _ltfsrHasMoreTags (\ s a -> s{_ltfsrHasMoreTags = a});
