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
-- Module      : Network.AWS.DynamoDBStreams.ListStreams
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of stream ARNs associated with the current account and endpoint. If the @TableName@ parameter is present, then @ListStreams@ will return only the streams ARNs for that table.
--
--
module Network.AWS.DynamoDBStreams.ListStreams
    (
    -- * Creating a Request
      listStreams
    , ListStreams
    -- * Request Lenses
    , lsExclusiveStartStreamARN
    , lsLimit
    , lsTableName

    -- * Destructuring the Response
    , listStreamsResponse
    , ListStreamsResponse
    -- * Response Lenses
    , lsrsLastEvaluatedStreamARN
    , lsrsStreams
    , lsrsResponseStatus
    ) where

import Network.AWS.DynamoDBStreams.Types
import Network.AWS.DynamoDBStreams.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @ListStreams@ operation.
--
--
--
-- /See:/ 'listStreams' smart constructor.
data ListStreams = ListStreams'
  { _lsExclusiveStartStreamARN :: !(Maybe Text)
  , _lsLimit                   :: !(Maybe Nat)
  , _lsTableName               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsExclusiveStartStreamARN' - The ARN (Amazon Resource Name) of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedStreamArn@ in the previous operation.
--
-- * 'lsLimit' - The maximum number of streams to return. The upper limit is 100.
--
-- * 'lsTableName' - If this parameter is provided, then only the streams associated with this table name are returned.
listStreams
    :: ListStreams
listStreams =
  ListStreams'
    { _lsExclusiveStartStreamARN = Nothing
    , _lsLimit = Nothing
    , _lsTableName = Nothing
    }


-- | The ARN (Amazon Resource Name) of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedStreamArn@ in the previous operation.
lsExclusiveStartStreamARN :: Lens' ListStreams (Maybe Text)
lsExclusiveStartStreamARN = lens _lsExclusiveStartStreamARN (\ s a -> s{_lsExclusiveStartStreamARN = a})

-- | The maximum number of streams to return. The upper limit is 100.
lsLimit :: Lens' ListStreams (Maybe Natural)
lsLimit = lens _lsLimit (\ s a -> s{_lsLimit = a}) . mapping _Nat

-- | If this parameter is provided, then only the streams associated with this table name are returned.
lsTableName :: Lens' ListStreams (Maybe Text)
lsTableName = lens _lsTableName (\ s a -> s{_lsTableName = a})

instance AWSRequest ListStreams where
        type Rs ListStreams = ListStreamsResponse
        request = postJSON dynamoDBStreams
        response
          = receiveJSON
              (\ s h x ->
                 ListStreamsResponse' <$>
                   (x .?> "LastEvaluatedStreamArn") <*>
                     (x .?> "Streams" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListStreams where

instance NFData ListStreams where

instance ToHeaders ListStreams where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDBStreams_20120810.ListStreams" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListStreams where
        toJSON ListStreams'{..}
          = object
              (catMaybes
                 [("ExclusiveStartStreamArn" .=) <$>
                    _lsExclusiveStartStreamARN,
                  ("Limit" .=) <$> _lsLimit,
                  ("TableName" .=) <$> _lsTableName])

instance ToPath ListStreams where
        toPath = const "/"

instance ToQuery ListStreams where
        toQuery = const mempty

-- | Represents the output of a @ListStreams@ operation.
--
--
--
-- /See:/ 'listStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { _lsrsLastEvaluatedStreamARN :: !(Maybe Text)
  , _lsrsStreams                :: !(Maybe [Stream])
  , _lsrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsLastEvaluatedStreamARN' - The stream ARN of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request. If @LastEvaluatedStreamArn@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved. If @LastEvaluatedStreamArn@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedStreamArn@ is empty.
--
-- * 'lsrsStreams' - A list of stream descriptors associated with the current account and endpoint.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listStreamsResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> ListStreamsResponse
listStreamsResponse pResponseStatus_ =
  ListStreamsResponse'
    { _lsrsLastEvaluatedStreamARN = Nothing
    , _lsrsStreams = Nothing
    , _lsrsResponseStatus = pResponseStatus_
    }


-- | The stream ARN of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request. If @LastEvaluatedStreamArn@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved. If @LastEvaluatedStreamArn@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedStreamArn@ is empty.
lsrsLastEvaluatedStreamARN :: Lens' ListStreamsResponse (Maybe Text)
lsrsLastEvaluatedStreamARN = lens _lsrsLastEvaluatedStreamARN (\ s a -> s{_lsrsLastEvaluatedStreamARN = a})

-- | A list of stream descriptors associated with the current account and endpoint.
lsrsStreams :: Lens' ListStreamsResponse [Stream]
lsrsStreams = lens _lsrsStreams (\ s a -> s{_lsrsStreams = a}) . _Default . _Coerce

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListStreamsResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

instance NFData ListStreamsResponse where
