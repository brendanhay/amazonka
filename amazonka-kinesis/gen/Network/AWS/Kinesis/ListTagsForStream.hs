{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.ListTagsForStream
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified Amazon Kinesis stream.
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
    , ltfsrsStatus
    , ltfsrsTags
    , ltfsrsHasMoreTags
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for @ListTagsForStream@.
--
-- /See:/ 'listTagsForStream' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfsLimit'
--
-- * 'ltfsExclusiveStartTagKey'
--
-- * 'ltfsStreamName'
data ListTagsForStream = ListTagsForStream'
    { _ltfsLimit                :: !(Maybe Nat)
    , _ltfsExclusiveStartTagKey :: !(Maybe Text)
    , _ltfsStreamName           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForStream' smart constructor.
listTagsForStream :: Text -> ListTagsForStream
listTagsForStream pStreamName_ =
    ListTagsForStream'
    { _ltfsLimit = Nothing
    , _ltfsExclusiveStartTagKey = Nothing
    , _ltfsStreamName = pStreamName_
    }

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
                   (pure (fromEnum s)) <*> (x .?> "Tags" .!@ mempty) <*>
                     (x .:> "HasMoreTags"))

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

-- | Represents the output for @ListTagsForStream@.
--
-- /See:/ 'listTagsForStreamResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfsrsStatus'
--
-- * 'ltfsrsTags'
--
-- * 'ltfsrsHasMoreTags'
data ListTagsForStreamResponse = ListTagsForStreamResponse'
    { _ltfsrsStatus      :: !Int
    , _ltfsrsTags        :: ![Tag]
    , _ltfsrsHasMoreTags :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForStreamResponse' smart constructor.
listTagsForStreamResponse :: Int -> Bool -> ListTagsForStreamResponse
listTagsForStreamResponse pStatus_ pHasMoreTags_ =
    ListTagsForStreamResponse'
    { _ltfsrsStatus = pStatus_
    , _ltfsrsTags = mempty
    , _ltfsrsHasMoreTags = pHasMoreTags_
    }

-- | FIXME: Undocumented member.
ltfsrsStatus :: Lens' ListTagsForStreamResponse Int
ltfsrsStatus = lens _ltfsrsStatus (\ s a -> s{_ltfsrsStatus = a});

-- | A list of tags associated with @StreamName@, starting with the first tag
-- after @ExclusiveStartTagKey@ and up to the specified @Limit@.
ltfsrsTags :: Lens' ListTagsForStreamResponse [Tag]
ltfsrsTags = lens _ltfsrsTags (\ s a -> s{_ltfsrsTags = a}) . _Coerce;

-- | If set to @true@, more tags are available. To request additional tags,
-- set @ExclusiveStartTagKey@ to the key of the last tag returned.
ltfsrsHasMoreTags :: Lens' ListTagsForStreamResponse Bool
ltfsrsHasMoreTags = lens _ltfsrsHasMoreTags (\ s a -> s{_ltfsrsHasMoreTags = a});
