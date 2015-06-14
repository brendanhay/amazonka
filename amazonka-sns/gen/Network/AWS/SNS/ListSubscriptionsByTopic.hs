{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SNS.ListSubscriptionsByTopic
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

-- | Returns a list of the subscriptions to a specific topic. Each call
-- returns a limited list of subscriptions, up to 100. If there are more
-- subscriptions, a @NextToken@ is also returned. Use the @NextToken@
-- parameter in a new @ListSubscriptionsByTopic@ call to get further
-- results.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_ListSubscriptionsByTopic.html>
module Network.AWS.SNS.ListSubscriptionsByTopic
    (
    -- * Request
      ListSubscriptionsByTopic
    -- ** Request constructor
    , listSubscriptionsByTopic
    -- ** Request lenses
    , lsbtNextToken
    , lsbtTopicARN

    -- * Response
    , ListSubscriptionsByTopicResponse
    -- ** Response constructor
    , listSubscriptionsByTopicResponse
    -- ** Response lenses
    , lsbtrNextToken
    , lsbtrSubscriptions
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.SNS.Types

-- | /See:/ 'listSubscriptionsByTopic' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsbtNextToken'
--
-- * 'lsbtTopicARN'
data ListSubscriptionsByTopic = ListSubscriptionsByTopic'{_lsbtNextToken :: Maybe Text, _lsbtTopicARN :: Text} deriving (Eq, Read, Show)

-- | 'ListSubscriptionsByTopic' smart constructor.
listSubscriptionsByTopic :: Text -> ListSubscriptionsByTopic
listSubscriptionsByTopic pTopicARN = ListSubscriptionsByTopic'{_lsbtNextToken = Nothing, _lsbtTopicARN = pTopicARN};

-- | Token returned by the previous @ListSubscriptionsByTopic@ request.
lsbtNextToken :: Lens' ListSubscriptionsByTopic (Maybe Text)
lsbtNextToken = lens _lsbtNextToken (\ s a -> s{_lsbtNextToken = a});

-- | The ARN of the topic for which you wish to find subscriptions.
lsbtTopicARN :: Lens' ListSubscriptionsByTopic Text
lsbtTopicARN = lens _lsbtTopicARN (\ s a -> s{_lsbtTopicARN = a});

instance AWSRequest ListSubscriptionsByTopic where
        type Sv ListSubscriptionsByTopic = SNS
        type Rs ListSubscriptionsByTopic =
             ListSubscriptionsByTopicResponse
        request = post
        response
          = receiveXMLWrapper "ListSubscriptionsByTopicResult"
              (\ s h x ->
                 ListSubscriptionsByTopicResponse' <$>
                   x .@? "NextToken" <*>
                     (x .@? "Subscriptions" .!@ mempty >>=
                        parseXMLList "member"))

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

-- | /See:/ 'listSubscriptionsByTopicResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsbtrNextToken'
--
-- * 'lsbtrSubscriptions'
data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse'{_lsbtrNextToken :: Maybe Text, _lsbtrSubscriptions :: Maybe [Subscription]} deriving (Eq, Read, Show)

-- | 'ListSubscriptionsByTopicResponse' smart constructor.
listSubscriptionsByTopicResponse :: ListSubscriptionsByTopicResponse
listSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse'{_lsbtrNextToken = Nothing, _lsbtrSubscriptions = Nothing};

-- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This
-- element is returned if there are more subscriptions to retrieve.
lsbtrNextToken :: Lens' ListSubscriptionsByTopicResponse (Maybe Text)
lsbtrNextToken = lens _lsbtrNextToken (\ s a -> s{_lsbtrNextToken = a});

-- | A list of subscriptions.
lsbtrSubscriptions :: Lens' ListSubscriptionsByTopicResponse (Maybe [Subscription])
lsbtrSubscriptions = lens _lsbtrSubscriptions (\ s a -> s{_lsbtrSubscriptions = a});
