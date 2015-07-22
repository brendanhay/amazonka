{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListSubscriptionsByTopic
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the subscriptions to a specific topic. Each call
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
    , lsbtrqNextToken
    , lsbtrqTopicARN

    -- * Response
    , ListSubscriptionsByTopicResponse
    -- ** Response constructor
    , listSubscriptionsByTopicResponse
    -- ** Response lenses
    , lsbtrsNextToken
    , lsbtrsSubscriptions
    , lsbtrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for ListSubscriptionsByTopic action.
--
-- /See:/ 'listSubscriptionsByTopic' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsbtrqNextToken'
--
-- * 'lsbtrqTopicARN'
data ListSubscriptionsByTopic = ListSubscriptionsByTopic'
    { _lsbtrqNextToken :: !(Maybe Text)
    , _lsbtrqTopicARN  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSubscriptionsByTopic' smart constructor.
listSubscriptionsByTopic :: Text -> ListSubscriptionsByTopic
listSubscriptionsByTopic pTopicARN =
    ListSubscriptionsByTopic'
    { _lsbtrqNextToken = Nothing
    , _lsbtrqTopicARN = pTopicARN
    }

-- | Token returned by the previous @ListSubscriptionsByTopic@ request.
lsbtrqNextToken :: Lens' ListSubscriptionsByTopic (Maybe Text)
lsbtrqNextToken = lens _lsbtrqNextToken (\ s a -> s{_lsbtrqNextToken = a});

-- | The ARN of the topic for which you wish to find subscriptions.
lsbtrqTopicARN :: Lens' ListSubscriptionsByTopic Text
lsbtrqTopicARN = lens _lsbtrqTopicARN (\ s a -> s{_lsbtrqTopicARN = a});

instance AWSPager ListSubscriptionsByTopic where
        page rq rs
          | stop (rs ^. lsbtrsNextToken) = Nothing
          | stop (rs ^. lsbtrsSubscriptions) = Nothing
          | otherwise =
            Just $ rq & lsbtrqNextToken .~ rs ^. lsbtrsNextToken

instance AWSRequest ListSubscriptionsByTopic where
        type Sv ListSubscriptionsByTopic = SNS
        type Rs ListSubscriptionsByTopic =
             ListSubscriptionsByTopicResponse
        request = post
        response
          = receiveXMLWrapper "ListSubscriptionsByTopicResult"
              (\ s h x ->
                 ListSubscriptionsByTopicResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Subscriptions" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
               "NextToken" =: _lsbtrqNextToken,
               "TopicArn" =: _lsbtrqTopicARN]

-- | Response for ListSubscriptionsByTopic action.
--
-- /See:/ 'listSubscriptionsByTopicResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsbtrsNextToken'
--
-- * 'lsbtrsSubscriptions'
--
-- * 'lsbtrsStatus'
data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse'
    { _lsbtrsNextToken     :: !(Maybe Text)
    , _lsbtrsSubscriptions :: !(Maybe [Subscription])
    , _lsbtrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSubscriptionsByTopicResponse' smart constructor.
listSubscriptionsByTopicResponse :: Int -> ListSubscriptionsByTopicResponse
listSubscriptionsByTopicResponse pStatus =
    ListSubscriptionsByTopicResponse'
    { _lsbtrsNextToken = Nothing
    , _lsbtrsSubscriptions = Nothing
    , _lsbtrsStatus = pStatus
    }

-- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This
-- element is returned if there are more subscriptions to retrieve.
lsbtrsNextToken :: Lens' ListSubscriptionsByTopicResponse (Maybe Text)
lsbtrsNextToken = lens _lsbtrsNextToken (\ s a -> s{_lsbtrsNextToken = a});

-- | A list of subscriptions.
lsbtrsSubscriptions :: Lens' ListSubscriptionsByTopicResponse [Subscription]
lsbtrsSubscriptions = lens _lsbtrsSubscriptions (\ s a -> s{_lsbtrsSubscriptions = a}) . _Default;

-- | FIXME: Undocumented member.
lsbtrsStatus :: Lens' ListSubscriptionsByTopicResponse Int
lsbtrsStatus = lens _lsbtrsStatus (\ s a -> s{_lsbtrsStatus = a});
