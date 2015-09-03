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
-- Module      : Network.AWS.SNS.ListTopics
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the requester\'s topics. Each call returns a limited
-- list of topics, up to 100. If there are more topics, a 'NextToken' is
-- also returned. Use the 'NextToken' parameter in a new 'ListTopics' call
-- to get further results.
--
-- /See:/ <http://docs.aws.amazon.com/sns/latest/api/API_ListTopics.html AWS API Reference> for ListTopics.
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListTopics
    (
    -- * Creating a Request
      listTopics
    , ListTopics
    -- * Request Lenses
    , ltNextToken

    -- * Destructuring the Response
    , listTopicsResponse
    , ListTopicsResponse
    -- * Response Lenses
    , ltrsTopics
    , ltrsNextToken
    , ltrsResponseStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types
import           Network.AWS.SNS.Types.Product

-- | /See:/ 'listTopics' smart constructor.
newtype ListTopics = ListTopics'
    { _ltNextToken :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTopics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken'
listTopics
    :: ListTopics
listTopics =
    ListTopics'
    { _ltNextToken = Nothing
    }

-- | Token returned by the previous 'ListTopics' request.
ltNextToken :: Lens' ListTopics (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a});

instance AWSPager ListTopics where
        page rq rs
          | stop (rs ^. ltrsNextToken) = Nothing
          | stop (rs ^. ltrsTopics) = Nothing
          | otherwise =
            Just $ rq & ltNextToken .~ rs ^. ltrsNextToken

instance AWSRequest ListTopics where
        type Rs ListTopics = ListTopicsResponse
        request = postQuery sNS
        response
          = receiveXMLWrapper "ListTopicsResult"
              (\ s h x ->
                 ListTopicsResponse' <$>
                   (x .@? "Topics" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListTopics where
        toHeaders = const mempty

instance ToPath ListTopics where
        toPath = const "/"

instance ToQuery ListTopics where
        toQuery ListTopics'{..}
          = mconcat
              ["Action" =: ("ListTopics" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "NextToken" =: _ltNextToken]

-- | Response for ListTopics action.
--
-- /See:/ 'listTopicsResponse' smart constructor.
data ListTopicsResponse = ListTopicsResponse'
    { _ltrsTopics         :: !(Maybe [Topic])
    , _ltrsNextToken      :: !(Maybe Text)
    , _ltrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTopicsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsTopics'
--
-- * 'ltrsNextToken'
--
-- * 'ltrsResponseStatus'
listTopicsResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTopicsResponse
listTopicsResponse pResponseStatus_ =
    ListTopicsResponse'
    { _ltrsTopics = Nothing
    , _ltrsNextToken = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }

-- | A list of topic ARNs.
ltrsTopics :: Lens' ListTopicsResponse [Topic]
ltrsTopics = lens _ltrsTopics (\ s a -> s{_ltrsTopics = a}) . _Default . _Coerce;

-- | Token to pass along to the next 'ListTopics' request. This element is
-- returned if there are additional topics to retrieve.
ltrsNextToken :: Lens' ListTopicsResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a});

-- | The response status code.
ltrsResponseStatus :: Lens' ListTopicsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a});
