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
-- Module      : Network.AWS.Kinesis.ListShards
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the shards in a stream and provides information about each shard.
--
--
-- /Important:/ This API is a new operation that is used by the Amazon Kinesis Client Library (KCL). If you have a fine-grained IAM policy that only allows specific operations, you must update your policy to allow calls to this API. For more information, see <https://docs.aws.amazon.com/streams/latest/dev/controlling-access.html Controlling Access to Amazon Kinesis Data Streams Resources Using IAM> .
--
module Network.AWS.Kinesis.ListShards
    (
    -- * Creating a Request
      listShards
    , ListShards
    -- * Request Lenses
    , lsNextToken
    , lsExclusiveStartShardId
    , lsStreamCreationTimestamp
    , lsStreamName
    , lsMaxResults

    -- * Destructuring the Response
    , listShardsResponse
    , ListShardsResponse
    -- * Response Lenses
    , lrsNextToken
    , lrsShards
    , lrsResponseStatus
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listShards' smart constructor.
data ListShards = ListShards'
  { _lsNextToken               :: !(Maybe Text)
  , _lsExclusiveStartShardId   :: !(Maybe Text)
  , _lsStreamCreationTimestamp :: !(Maybe POSIX)
  , _lsStreamName              :: !(Maybe Text)
  , _lsMaxResults              :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListShards' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsNextToken' - When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards. Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream. You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of shards that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListShards@ operation. /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
--
-- * 'lsExclusiveStartShardId' - The ID of the shard to start the list with.  If you don't specify this parameter, the default behavior is for @ListShards@ to list the shards starting with the first one in the stream. You cannot specify this parameter if you specify @NextToken@ .
--
-- * 'lsStreamCreationTimestamp' - Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the shards for. You cannot specify this parameter if you specify the @NextToken@ parameter.
--
-- * 'lsStreamName' - The name of the data stream whose shards you want to list.  You cannot specify this parameter if you specify the @NextToken@ parameter.
--
-- * 'lsMaxResults' - The maximum number of shards to return in a single call to @ListShards@ . The minimum value you can specify for this parameter is 1, and the maximum is 1,000, which is also the default. When the number of shards to be listed is greater than the value of @MaxResults@ , the response contains a @NextToken@ value that you can use in a subsequent call to @ListShards@ to list the next set of shards.
listShards
    :: ListShards
listShards =
  ListShards'
    { _lsNextToken = Nothing
    , _lsExclusiveStartShardId = Nothing
    , _lsStreamCreationTimestamp = Nothing
    , _lsStreamName = Nothing
    , _lsMaxResults = Nothing
    }


-- | When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards. Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream. You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of shards that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListShards@ operation. /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
lsNextToken :: Lens' ListShards (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a})

-- | The ID of the shard to start the list with.  If you don't specify this parameter, the default behavior is for @ListShards@ to list the shards starting with the first one in the stream. You cannot specify this parameter if you specify @NextToken@ .
lsExclusiveStartShardId :: Lens' ListShards (Maybe Text)
lsExclusiveStartShardId = lens _lsExclusiveStartShardId (\ s a -> s{_lsExclusiveStartShardId = a})

-- | Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the shards for. You cannot specify this parameter if you specify the @NextToken@ parameter.
lsStreamCreationTimestamp :: Lens' ListShards (Maybe UTCTime)
lsStreamCreationTimestamp = lens _lsStreamCreationTimestamp (\ s a -> s{_lsStreamCreationTimestamp = a}) . mapping _Time

-- | The name of the data stream whose shards you want to list.  You cannot specify this parameter if you specify the @NextToken@ parameter.
lsStreamName :: Lens' ListShards (Maybe Text)
lsStreamName = lens _lsStreamName (\ s a -> s{_lsStreamName = a})

-- | The maximum number of shards to return in a single call to @ListShards@ . The minimum value you can specify for this parameter is 1, and the maximum is 1,000, which is also the default. When the number of shards to be listed is greater than the value of @MaxResults@ , the response contains a @NextToken@ value that you can use in a subsequent call to @ListShards@ to list the next set of shards.
lsMaxResults :: Lens' ListShards (Maybe Natural)
lsMaxResults = lens _lsMaxResults (\ s a -> s{_lsMaxResults = a}) . mapping _Nat

instance AWSRequest ListShards where
        type Rs ListShards = ListShardsResponse
        request = postJSON kinesis
        response
          = receiveJSON
              (\ s h x ->
                 ListShardsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Shards" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListShards where

instance NFData ListShards where

instance ToHeaders ListShards where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.ListShards" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListShards where
        toJSON ListShards'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lsNextToken,
                  ("ExclusiveStartShardId" .=) <$>
                    _lsExclusiveStartShardId,
                  ("StreamCreationTimestamp" .=) <$>
                    _lsStreamCreationTimestamp,
                  ("StreamName" .=) <$> _lsStreamName,
                  ("MaxResults" .=) <$> _lsMaxResults])

instance ToPath ListShards where
        toPath = const "/"

instance ToQuery ListShards where
        toQuery = const mempty

-- | /See:/ 'listShardsResponse' smart constructor.
data ListShardsResponse = ListShardsResponse'
  { _lrsNextToken      :: !(Maybe Text)
  , _lrsShards         :: !(Maybe [Shard])
  , _lrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListShardsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards. For more information about the use of this pagination token when calling the @ListShards@ operation, see 'ListShardsInput$NextToken' . /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
--
-- * 'lrsShards' - An array of JSON objects. Each object represents one shard and specifies the IDs of the shard, the shard's parent, and the shard that's adjacent to the shard's parent. Each object also contains the starting and ending hash keys and the starting and ending sequence numbers for the shard.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listShardsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListShardsResponse
listShardsResponse pResponseStatus_ =
  ListShardsResponse'
    { _lrsNextToken = Nothing
    , _lrsShards = Nothing
    , _lrsResponseStatus = pResponseStatus_
    }


-- | When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards. For more information about the use of this pagination token when calling the @ListShards@ operation, see 'ListShardsInput$NextToken' . /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
lrsNextToken :: Lens' ListShardsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a})

-- | An array of JSON objects. Each object represents one shard and specifies the IDs of the shard, the shard's parent, and the shard that's adjacent to the shard's parent. Each object also contains the starting and ending hash keys and the starting and ending sequence numbers for the shard.
lrsShards :: Lens' ListShardsResponse [Shard]
lrsShards = lens _lrsShards (\ s a -> s{_lrsShards = a}) . _Default . _Coerce

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListShardsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData ListShardsResponse where
