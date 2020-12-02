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
-- Module      : Network.AWS.KinesisVideo.ListStreams
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @StreamInfo@ objects. Each object describes a stream. To retrieve only streams that satisfy a specific condition, you can specify a @StreamNameCondition@ .
--
--
module Network.AWS.KinesisVideo.ListStreams
    (
    -- * Creating a Request
      listStreams
    , ListStreams
    -- * Request Lenses
    , lsNextToken
    , lsStreamNameCondition
    , lsMaxResults

    -- * Destructuring the Response
    , listStreamsResponse
    , ListStreamsResponse
    -- * Response Lenses
    , lsrsStreamInfoList
    , lsrsNextToken
    , lsrsResponseStatus
    ) where

import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listStreams' smart constructor.
data ListStreams = ListStreams'
  { _lsNextToken           :: !(Maybe Text)
  , _lsStreamNameCondition :: !(Maybe StreamNameCondition)
  , _lsMaxResults          :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsNextToken' - If you specify this parameter, when the result of a @ListStreams@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of streams, provide this token in your next request.
--
-- * 'lsStreamNameCondition' - Optional: Returns only streams that satisfy a specific condition. Currently, you can specify only the prefix of a stream name as a condition.
--
-- * 'lsMaxResults' - The maximum number of streams to return in the response. The default is 10,000.
listStreams
    :: ListStreams
listStreams =
  ListStreams'
    { _lsNextToken = Nothing
    , _lsStreamNameCondition = Nothing
    , _lsMaxResults = Nothing
    }


-- | If you specify this parameter, when the result of a @ListStreams@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of streams, provide this token in your next request.
lsNextToken :: Lens' ListStreams (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a})

-- | Optional: Returns only streams that satisfy a specific condition. Currently, you can specify only the prefix of a stream name as a condition.
lsStreamNameCondition :: Lens' ListStreams (Maybe StreamNameCondition)
lsStreamNameCondition = lens _lsStreamNameCondition (\ s a -> s{_lsStreamNameCondition = a})

-- | The maximum number of streams to return in the response. The default is 10,000.
lsMaxResults :: Lens' ListStreams (Maybe Natural)
lsMaxResults = lens _lsMaxResults (\ s a -> s{_lsMaxResults = a}) . mapping _Nat

instance AWSRequest ListStreams where
        type Rs ListStreams = ListStreamsResponse
        request = postJSON kinesisVideo
        response
          = receiveJSON
              (\ s h x ->
                 ListStreamsResponse' <$>
                   (x .?> "StreamInfoList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListStreams where

instance NFData ListStreams where

instance ToHeaders ListStreams where
        toHeaders = const mempty

instance ToJSON ListStreams where
        toJSON ListStreams'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lsNextToken,
                  ("StreamNameCondition" .=) <$>
                    _lsStreamNameCondition,
                  ("MaxResults" .=) <$> _lsMaxResults])

instance ToPath ListStreams where
        toPath = const "/listStreams"

instance ToQuery ListStreams where
        toQuery = const mempty

-- | /See:/ 'listStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { _lsrsStreamInfoList :: !(Maybe [StreamInfo])
  , _lsrsNextToken      :: !(Maybe Text)
  , _lsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsStreamInfoList' - An array of @StreamInfo@ objects.
--
-- * 'lsrsNextToken' - If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listStreamsResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> ListStreamsResponse
listStreamsResponse pResponseStatus_ =
  ListStreamsResponse'
    { _lsrsStreamInfoList = Nothing
    , _lsrsNextToken = Nothing
    , _lsrsResponseStatus = pResponseStatus_
    }


-- | An array of @StreamInfo@ objects.
lsrsStreamInfoList :: Lens' ListStreamsResponse [StreamInfo]
lsrsStreamInfoList = lens _lsrsStreamInfoList (\ s a -> s{_lsrsStreamInfoList = a}) . _Default . _Coerce

-- | If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
lsrsNextToken :: Lens' ListStreamsResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a})

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListStreamsResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

instance NFData ListStreamsResponse where
