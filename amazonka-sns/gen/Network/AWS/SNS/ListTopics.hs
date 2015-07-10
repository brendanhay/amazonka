{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListTopics
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the requester\'s topics. Each call returns a limited
-- list of topics, up to 100. If there are more topics, a @NextToken@ is
-- also returned. Use the @NextToken@ parameter in a new @ListTopics@ call
-- to get further results.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_ListTopics.html>
module Network.AWS.SNS.ListTopics
    (
    -- * Request
      ListTopics
    -- ** Request constructor
    , listTopics
    -- ** Request lenses
    , ltNextToken

    -- * Response
    , ListTopicsResponse
    -- ** Response constructor
    , listTopicsResponse
    -- ** Response lenses
    , ltrTopics
    , ltrNextToken
    , ltrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | /See:/ 'listTopics' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltNextToken'
newtype ListTopics = ListTopics'
    { _ltNextToken :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTopics' smart constructor.
listTopics :: ListTopics
listTopics =
    ListTopics'
    { _ltNextToken = Nothing
    }

-- | Token returned by the previous @ListTopics@ request.
ltNextToken :: Lens' ListTopics (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a});

instance AWSPager ListTopics where
        page rq rs
          | stop (rs ^. ltrNextToken) = Nothing
          | stop (rs ^. ltrTopics) = Nothing
          | otherwise =
            Just $ rq & ltNextToken .~ rs ^. ltrNextToken

instance AWSRequest ListTopics where
        type Sv ListTopics = SNS
        type Rs ListTopics = ListTopicsResponse
        request = post
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltrTopics'
--
-- * 'ltrNextToken'
--
-- * 'ltrStatus'
data ListTopicsResponse = ListTopicsResponse'
    { _ltrTopics    :: !(Maybe [Topic])
    , _ltrNextToken :: !(Maybe Text)
    , _ltrStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTopicsResponse' smart constructor.
listTopicsResponse :: Int -> ListTopicsResponse
listTopicsResponse pStatus =
    ListTopicsResponse'
    { _ltrTopics = Nothing
    , _ltrNextToken = Nothing
    , _ltrStatus = pStatus
    }

-- | A list of topic ARNs.
ltrTopics :: Lens' ListTopicsResponse [Topic]
ltrTopics = lens _ltrTopics (\ s a -> s{_ltrTopics = a}) . _Default;

-- | Token to pass along to the next @ListTopics@ request. This element is
-- returned if there are additional topics to retrieve.
ltrNextToken :: Lens' ListTopicsResponse (Maybe Text)
ltrNextToken = lens _ltrNextToken (\ s a -> s{_ltrNextToken = a});

-- | FIXME: Undocumented member.
ltrStatus :: Lens' ListTopicsResponse Int
ltrStatus = lens _ltrStatus (\ s a -> s{_ltrStatus = a});
