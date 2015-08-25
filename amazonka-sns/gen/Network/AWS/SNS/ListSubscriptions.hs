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
-- Module      : Network.AWS.SNS.ListSubscriptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the requester\'s subscriptions. Each call returns a
-- limited list of subscriptions, up to 100. If there are more
-- subscriptions, a 'NextToken' is also returned. Use the 'NextToken'
-- parameter in a new 'ListSubscriptions' call to get further results.
--
-- /See:/ <http://docs.aws.amazon.com/sns/latest/api/API_ListSubscriptions.html AWS API Reference> for ListSubscriptions.
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListSubscriptions
    (
    -- * Creating a Request
      listSubscriptions
    , ListSubscriptions
    -- * Request Lenses
    , lsNextToken

    -- * Destructuring the Response
    , listSubscriptionsResponse
    , ListSubscriptionsResponse
    -- * Response Lenses
    , lsrsNextToken
    , lsrsSubscriptions
    , lsrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types
import           Network.AWS.SNS.Types.Product

-- | Input for ListSubscriptions action.
--
-- /See:/ 'listSubscriptions' smart constructor.
newtype ListSubscriptions = ListSubscriptions'
    { _lsNextToken :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListSubscriptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsNextToken'
listSubscriptions
    :: ListSubscriptions
listSubscriptions =
    ListSubscriptions'
    { _lsNextToken = Nothing
    }

-- | Token returned by the previous 'ListSubscriptions' request.
lsNextToken :: Lens' ListSubscriptions (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a});

instance AWSPager ListSubscriptions where
        page rq rs
          | stop (rs ^. lsrsNextToken) = Nothing
          | stop (rs ^. lsrsSubscriptions) = Nothing
          | otherwise =
            Just $ rq & lsNextToken .~ rs ^. lsrsNextToken

instance AWSRequest ListSubscriptions where
        type Rs ListSubscriptions = ListSubscriptionsResponse
        request = postQuery sNS
        response
          = receiveXMLWrapper "ListSubscriptionsResult"
              (\ s h x ->
                 ListSubscriptionsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Subscriptions" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders ListSubscriptions where
        toHeaders = const mempty

instance ToPath ListSubscriptions where
        toPath = const "/"

instance ToQuery ListSubscriptions where
        toQuery ListSubscriptions'{..}
          = mconcat
              ["Action" =: ("ListSubscriptions" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "NextToken" =: _lsNextToken]

-- | Response for ListSubscriptions action
--
-- /See:/ 'listSubscriptionsResponse' smart constructor.
data ListSubscriptionsResponse = ListSubscriptionsResponse'
    { _lsrsNextToken     :: !(Maybe Text)
    , _lsrsSubscriptions :: !(Maybe [Subscription])
    , _lsrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListSubscriptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsNextToken'
--
-- * 'lsrsSubscriptions'
--
-- * 'lsrsStatus'
listSubscriptionsResponse
    :: Int -- ^ 'lsrsStatus'
    -> ListSubscriptionsResponse
listSubscriptionsResponse pStatus_ =
    ListSubscriptionsResponse'
    { _lsrsNextToken = Nothing
    , _lsrsSubscriptions = Nothing
    , _lsrsStatus = pStatus_
    }

-- | Token to pass along to the next 'ListSubscriptions' request. This
-- element is returned if there are more subscriptions to retrieve.
lsrsNextToken :: Lens' ListSubscriptionsResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a});

-- | A list of subscriptions.
lsrsSubscriptions :: Lens' ListSubscriptionsResponse [Subscription]
lsrsSubscriptions = lens _lsrsSubscriptions (\ s a -> s{_lsrsSubscriptions = a}) . _Default . _Coerce;

-- | The response status code.
lsrsStatus :: Lens' ListSubscriptionsResponse Int
lsrsStatus = lens _lsrsStatus (\ s a -> s{_lsrsStatus = a});
