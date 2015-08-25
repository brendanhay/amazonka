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
-- Module      : Network.AWS.Kinesis.ListTagsForStream
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified Amazon Kinesis stream.
--
-- /See:/ <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_ListTagsForStream.html AWS API Reference> for ListTagsForStream.
module Network.AWS.Kinesis.ListTagsForStream
    (
    -- * Creating a Request
      listTagsForStream
    , ListTagsForStream
    -- * Request Lenses
    , ltfsLimit
    , ltfsExclusiveStartTagKey
    , ltfsStreamName

    -- * Destructuring the Response
    , listTagsForStreamResponse
    , ListTagsForStreamResponse
    -- * Response Lenses
    , ltfsrsStatus
    , ltfsrsTags
    , ltfsrsHasMoreTags
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Kinesis.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for 'ListTagsForStream'.
--
-- /See:/ 'listTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
    { _ltfsLimit                :: !(Maybe Nat)
    , _ltfsExclusiveStartTagKey :: !(Maybe Text)
    , _ltfsStreamName           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTagsForStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfsLimit'
--
-- * 'ltfsExclusiveStartTagKey'
--
-- * 'ltfsStreamName'
listTagsForStream
    :: Text -- ^ 'ltfsStreamName'
    -> ListTagsForStream
listTagsForStream pStreamName_ =
    ListTagsForStream'
    { _ltfsLimit = Nothing
    , _ltfsExclusiveStartTagKey = Nothing
    , _ltfsStreamName = pStreamName_
    }

-- | The number of tags to return. If this number is less than the total
-- number of tags associated with the stream, 'HasMoreTags' is set to
-- 'true'. To list additional tags, set 'ExclusiveStartTagKey' to the last
-- key in the response.
ltfsLimit :: Lens' ListTagsForStream (Maybe Natural)
ltfsLimit = lens _ltfsLimit (\ s a -> s{_ltfsLimit = a}) . mapping _Nat;

-- | The key to use as the starting point for the list of tags. If this
-- parameter is set, 'ListTagsForStream' gets all tags that occur after
-- 'ExclusiveStartTagKey'.
ltfsExclusiveStartTagKey :: Lens' ListTagsForStream (Maybe Text)
ltfsExclusiveStartTagKey = lens _ltfsExclusiveStartTagKey (\ s a -> s{_ltfsExclusiveStartTagKey = a});

-- | The name of the stream.
ltfsStreamName :: Lens' ListTagsForStream Text
ltfsStreamName = lens _ltfsStreamName (\ s a -> s{_ltfsStreamName = a});

instance AWSRequest ListTagsForStream where
        type Rs ListTagsForStream = ListTagsForStreamResponse
        request = postJSON kinesis
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
              (catMaybes
                 [("Limit" .=) <$> _ltfsLimit,
                  ("ExclusiveStartTagKey" .=) <$>
                    _ltfsExclusiveStartTagKey,
                  Just ("StreamName" .= _ltfsStreamName)])

instance ToPath ListTagsForStream where
        toPath = const "/"

instance ToQuery ListTagsForStream where
        toQuery = const mempty

-- | Represents the output for 'ListTagsForStream'.
--
-- /See:/ 'listTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
    { _ltfsrsStatus      :: !Int
    , _ltfsrsTags        :: ![Tag]
    , _ltfsrsHasMoreTags :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTagsForStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfsrsStatus'
--
-- * 'ltfsrsTags'
--
-- * 'ltfsrsHasMoreTags'
listTagsForStreamResponse
    :: Int -- ^ 'ltfsrsStatus'
    -> Bool -- ^ 'ltfsrsHasMoreTags'
    -> ListTagsForStreamResponse
listTagsForStreamResponse pStatus_ pHasMoreTags_ =
    ListTagsForStreamResponse'
    { _ltfsrsStatus = pStatus_
    , _ltfsrsTags = mempty
    , _ltfsrsHasMoreTags = pHasMoreTags_
    }

-- | The response status code.
ltfsrsStatus :: Lens' ListTagsForStreamResponse Int
ltfsrsStatus = lens _ltfsrsStatus (\ s a -> s{_ltfsrsStatus = a});

-- | A list of tags associated with 'StreamName', starting with the first tag
-- after 'ExclusiveStartTagKey' and up to the specified 'Limit'.
ltfsrsTags :: Lens' ListTagsForStreamResponse [Tag]
ltfsrsTags = lens _ltfsrsTags (\ s a -> s{_ltfsrsTags = a}) . _Coerce;

-- | If set to 'true', more tags are available. To request additional tags,
-- set 'ExclusiveStartTagKey' to the key of the last tag returned.
ltfsrsHasMoreTags :: Lens' ListTagsForStreamResponse Bool
ltfsrsHasMoreTags = lens _ltfsrsHasMoreTags (\ s a -> s{_ltfsrsHasMoreTags = a});
