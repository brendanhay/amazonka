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
-- Module      : Network.AWS.Kinesis.ListStreams
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Kinesis data streams.
--
--
-- The number of streams may be too large to return from a single call to @ListStreams@ . You can limit the number of returned streams using the @Limit@ parameter. If you do not specify a value for the @Limit@ parameter, Kinesis Data Streams uses the default limit, which is currently 10.
--
-- You can detect if there are more streams available to list by using the @HasMoreStreams@ flag from the returned output. If there are more streams available, you can request more streams by using the name of the last stream returned by the @ListStreams@ request in the @ExclusiveStartStreamName@ parameter in a subsequent request to @ListStreams@ . The group of stream names returned by the subsequent request is then added to the list. You can continue this process until all the stream names have been collected in the list.
--
-- 'ListStreams' has a limit of five transactions per second per account.
--
--
-- This operation returns paginated results.
module Network.AWS.Kinesis.ListStreams
    (
    -- * Creating a Request
      listStreams
    , ListStreams
    -- * Request Lenses
    , lsLimit
    , lsExclusiveStartStreamName

    -- * Destructuring the Response
    , listStreamsResponse
    , ListStreamsResponse
    -- * Response Lenses
    , lsrsResponseStatus
    , lsrsStreamNames
    , lsrsHasMoreStreams
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for @ListStreams@ .
--
--
--
-- /See:/ 'listStreams' smart constructor.
data ListStreams = ListStreams'
  { _lsLimit                    :: !(Maybe Nat)
  , _lsExclusiveStartStreamName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsLimit' - The maximum number of streams to list.
--
-- * 'lsExclusiveStartStreamName' - The name of the stream to start the list with.
listStreams
    :: ListStreams
listStreams =
  ListStreams' {_lsLimit = Nothing, _lsExclusiveStartStreamName = Nothing}


-- | The maximum number of streams to list.
lsLimit :: Lens' ListStreams (Maybe Natural)
lsLimit = lens _lsLimit (\ s a -> s{_lsLimit = a}) . mapping _Nat

-- | The name of the stream to start the list with.
lsExclusiveStartStreamName :: Lens' ListStreams (Maybe Text)
lsExclusiveStartStreamName = lens _lsExclusiveStartStreamName (\ s a -> s{_lsExclusiveStartStreamName = a})

instance AWSPager ListStreams where
        page rq rs
          | stop (rs ^. lsrsHasMoreStreams) = Nothing
          | isNothing (rs ^? lsrsStreamNames . _last) = Nothing
          | otherwise =
            Just $ rq &
              lsExclusiveStartStreamName .~
                rs ^? lsrsStreamNames . _last

instance AWSRequest ListStreams where
        type Rs ListStreams = ListStreamsResponse
        request = postJSON kinesis
        response
          = receiveJSON
              (\ s h x ->
                 ListStreamsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "StreamNames" .!@ mempty)
                     <*> (x .:> "HasMoreStreams"))

instance Hashable ListStreams where

instance NFData ListStreams where

instance ToHeaders ListStreams where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.ListStreams" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListStreams where
        toJSON ListStreams'{..}
          = object
              (catMaybes
                 [("Limit" .=) <$> _lsLimit,
                  ("ExclusiveStartStreamName" .=) <$>
                    _lsExclusiveStartStreamName])

instance ToPath ListStreams where
        toPath = const "/"

instance ToQuery ListStreams where
        toQuery = const mempty

-- | Represents the output for @ListStreams@ .
--
--
--
-- /See:/ 'listStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { _lsrsResponseStatus :: !Int
  , _lsrsStreamNames    :: ![Text]
  , _lsrsHasMoreStreams :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsResponseStatus' - -- | The response status code.
--
-- * 'lsrsStreamNames' - The names of the streams that are associated with the AWS account making the @ListStreams@ request.
--
-- * 'lsrsHasMoreStreams' - If set to @true@ , there are more streams available to list.
listStreamsResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> Bool -- ^ 'lsrsHasMoreStreams'
    -> ListStreamsResponse
listStreamsResponse pResponseStatus_ pHasMoreStreams_ =
  ListStreamsResponse'
    { _lsrsResponseStatus = pResponseStatus_
    , _lsrsStreamNames = mempty
    , _lsrsHasMoreStreams = pHasMoreStreams_
    }


-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListStreamsResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

-- | The names of the streams that are associated with the AWS account making the @ListStreams@ request.
lsrsStreamNames :: Lens' ListStreamsResponse [Text]
lsrsStreamNames = lens _lsrsStreamNames (\ s a -> s{_lsrsStreamNames = a}) . _Coerce

-- | If set to @true@ , there are more streams available to list.
lsrsHasMoreStreams :: Lens' ListStreamsResponse Bool
lsrsHasMoreStreams = lens _lsrsHasMoreStreams (\ s a -> s{_lsrsHasMoreStreams = a})

instance NFData ListStreamsResponse where
