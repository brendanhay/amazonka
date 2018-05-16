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
-- Module      : Network.AWS.SNS.ListSubscriptionsByTopic
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the subscriptions to a specific topic. Each call returns a limited list of subscriptions, up to 100. If there are more subscriptions, a @NextToken@ is also returned. Use the @NextToken@ parameter in a new @ListSubscriptionsByTopic@ call to get further results.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListSubscriptionsByTopic
    (
    -- * Creating a Request
      listSubscriptionsByTopic
    , ListSubscriptionsByTopic
    -- * Request Lenses
    , lsbtNextToken
    , lsbtTopicARN

    -- * Destructuring the Response
    , listSubscriptionsByTopicResponse
    , ListSubscriptionsByTopicResponse
    -- * Response Lenses
    , lsbtrsNextToken
    , lsbtrsSubscriptions
    , lsbtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for ListSubscriptionsByTopic action.
--
--
--
-- /See:/ 'listSubscriptionsByTopic' smart constructor.
data ListSubscriptionsByTopic = ListSubscriptionsByTopic'
  { _lsbtNextToken :: !(Maybe Text)
  , _lsbtTopicARN  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSubscriptionsByTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsbtNextToken' - Token returned by the previous @ListSubscriptionsByTopic@ request.
--
-- * 'lsbtTopicARN' - The ARN of the topic for which you wish to find subscriptions.
listSubscriptionsByTopic
    :: Text -- ^ 'lsbtTopicARN'
    -> ListSubscriptionsByTopic
listSubscriptionsByTopic pTopicARN_ =
  ListSubscriptionsByTopic'
    {_lsbtNextToken = Nothing, _lsbtTopicARN = pTopicARN_}


-- | Token returned by the previous @ListSubscriptionsByTopic@ request.
lsbtNextToken :: Lens' ListSubscriptionsByTopic (Maybe Text)
lsbtNextToken = lens _lsbtNextToken (\ s a -> s{_lsbtNextToken = a})

-- | The ARN of the topic for which you wish to find subscriptions.
lsbtTopicARN :: Lens' ListSubscriptionsByTopic Text
lsbtTopicARN = lens _lsbtTopicARN (\ s a -> s{_lsbtTopicARN = a})

instance AWSPager ListSubscriptionsByTopic where
        page rq rs
          | stop (rs ^. lsbtrsNextToken) = Nothing
          | stop (rs ^. lsbtrsSubscriptions) = Nothing
          | otherwise =
            Just $ rq & lsbtNextToken .~ rs ^. lsbtrsNextToken

instance AWSRequest ListSubscriptionsByTopic where
        type Rs ListSubscriptionsByTopic =
             ListSubscriptionsByTopicResponse
        request = postQuery sns
        response
          = receiveXMLWrapper "ListSubscriptionsByTopicResult"
              (\ s h x ->
                 ListSubscriptionsByTopicResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Subscriptions" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListSubscriptionsByTopic where

instance NFData ListSubscriptionsByTopic where

instance ToHeaders ListSubscriptionsByTopic where
        toHeaders = const mempty

instance ToPath ListSubscriptionsByTopic where
        toPath = const "/"

instance ToQuery ListSubscriptionsByTopic where
        toQuery ListSubscriptionsByTopic'{..}
          = mconcat
              ["Action" =:
                 ("ListSubscriptionsByTopic" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "NextToken" =: _lsbtNextToken,
               "TopicArn" =: _lsbtTopicARN]

-- | Response for ListSubscriptionsByTopic action.
--
--
--
-- /See:/ 'listSubscriptionsByTopicResponse' smart constructor.
data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse'
  { _lsbtrsNextToken      :: !(Maybe Text)
  , _lsbtrsSubscriptions  :: !(Maybe [Subscription])
  , _lsbtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSubscriptionsByTopicResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsbtrsNextToken' - Token to pass along to the next @ListSubscriptionsByTopic@ request. This element is returned if there are more subscriptions to retrieve.
--
-- * 'lsbtrsSubscriptions' - A list of subscriptions.
--
-- * 'lsbtrsResponseStatus' - -- | The response status code.
listSubscriptionsByTopicResponse
    :: Int -- ^ 'lsbtrsResponseStatus'
    -> ListSubscriptionsByTopicResponse
listSubscriptionsByTopicResponse pResponseStatus_ =
  ListSubscriptionsByTopicResponse'
    { _lsbtrsNextToken = Nothing
    , _lsbtrsSubscriptions = Nothing
    , _lsbtrsResponseStatus = pResponseStatus_
    }


-- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This element is returned if there are more subscriptions to retrieve.
lsbtrsNextToken :: Lens' ListSubscriptionsByTopicResponse (Maybe Text)
lsbtrsNextToken = lens _lsbtrsNextToken (\ s a -> s{_lsbtrsNextToken = a})

-- | A list of subscriptions.
lsbtrsSubscriptions :: Lens' ListSubscriptionsByTopicResponse [Subscription]
lsbtrsSubscriptions = lens _lsbtrsSubscriptions (\ s a -> s{_lsbtrsSubscriptions = a}) . _Default . _Coerce

-- | -- | The response status code.
lsbtrsResponseStatus :: Lens' ListSubscriptionsByTopicResponse Int
lsbtrsResponseStatus = lens _lsbtrsResponseStatus (\ s a -> s{_lsbtrsResponseStatus = a})

instance NFData ListSubscriptionsByTopicResponse
         where
