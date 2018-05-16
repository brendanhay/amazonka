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
-- Module      : Network.AWS.CodeCommit.DescribePullRequestEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more pull request events.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.DescribePullRequestEvents
    (
    -- * Creating a Request
      describePullRequestEvents
    , DescribePullRequestEvents
    -- * Request Lenses
    , dprePullRequestEventType
    , dpreActorARN
    , dpreNextToken
    , dpreMaxResults
    , dprePullRequestId

    -- * Destructuring the Response
    , describePullRequestEventsResponse
    , DescribePullRequestEventsResponse
    -- * Response Lenses
    , dprersNextToken
    , dprersResponseStatus
    , dprersPullRequestEvents
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePullRequestEvents' smart constructor.
data DescribePullRequestEvents = DescribePullRequestEvents'
  { _dprePullRequestEventType :: !(Maybe PullRequestEventType)
  , _dpreActorARN             :: !(Maybe Text)
  , _dpreNextToken            :: !(Maybe Text)
  , _dpreMaxResults           :: !(Maybe Int)
  , _dprePullRequestId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePullRequestEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprePullRequestEventType' - Optional. The pull request event type about which you want to return information.
--
-- * 'dpreActorARN' - The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with additional commits or changing the status of a pull request.
--
-- * 'dpreNextToken' - An enumeration token that when provided in a request, returns the next batch of the results.
--
-- * 'dpreMaxResults' - A non-negative integer used to limit the number of returned results. The default is 100 events, which is also the maximum number of events that can be returned in a result.
--
-- * 'dprePullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
describePullRequestEvents
    :: Text -- ^ 'dprePullRequestId'
    -> DescribePullRequestEvents
describePullRequestEvents pPullRequestId_ =
  DescribePullRequestEvents'
    { _dprePullRequestEventType = Nothing
    , _dpreActorARN = Nothing
    , _dpreNextToken = Nothing
    , _dpreMaxResults = Nothing
    , _dprePullRequestId = pPullRequestId_
    }


-- | Optional. The pull request event type about which you want to return information.
dprePullRequestEventType :: Lens' DescribePullRequestEvents (Maybe PullRequestEventType)
dprePullRequestEventType = lens _dprePullRequestEventType (\ s a -> s{_dprePullRequestEventType = a})

-- | The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with additional commits or changing the status of a pull request.
dpreActorARN :: Lens' DescribePullRequestEvents (Maybe Text)
dpreActorARN = lens _dpreActorARN (\ s a -> s{_dpreActorARN = a})

-- | An enumeration token that when provided in a request, returns the next batch of the results.
dpreNextToken :: Lens' DescribePullRequestEvents (Maybe Text)
dpreNextToken = lens _dpreNextToken (\ s a -> s{_dpreNextToken = a})

-- | A non-negative integer used to limit the number of returned results. The default is 100 events, which is also the maximum number of events that can be returned in a result.
dpreMaxResults :: Lens' DescribePullRequestEvents (Maybe Int)
dpreMaxResults = lens _dpreMaxResults (\ s a -> s{_dpreMaxResults = a})

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
dprePullRequestId :: Lens' DescribePullRequestEvents Text
dprePullRequestId = lens _dprePullRequestId (\ s a -> s{_dprePullRequestId = a})

instance AWSPager DescribePullRequestEvents where
        page rq rs
          | stop (rs ^. dprersNextToken) = Nothing
          | stop (rs ^. dprersPullRequestEvents) = Nothing
          | otherwise =
            Just $ rq & dpreNextToken .~ rs ^. dprersNextToken

instance AWSRequest DescribePullRequestEvents where
        type Rs DescribePullRequestEvents =
             DescribePullRequestEventsResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 DescribePullRequestEventsResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "pullRequestEvents" .!@ mempty))

instance Hashable DescribePullRequestEvents where

instance NFData DescribePullRequestEvents where

instance ToHeaders DescribePullRequestEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.DescribePullRequestEvents" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePullRequestEvents where
        toJSON DescribePullRequestEvents'{..}
          = object
              (catMaybes
                 [("pullRequestEventType" .=) <$>
                    _dprePullRequestEventType,
                  ("actorArn" .=) <$> _dpreActorARN,
                  ("nextToken" .=) <$> _dpreNextToken,
                  ("maxResults" .=) <$> _dpreMaxResults,
                  Just ("pullRequestId" .= _dprePullRequestId)])

instance ToPath DescribePullRequestEvents where
        toPath = const "/"

instance ToQuery DescribePullRequestEvents where
        toQuery = const mempty

-- | /See:/ 'describePullRequestEventsResponse' smart constructor.
data DescribePullRequestEventsResponse = DescribePullRequestEventsResponse'
  { _dprersNextToken         :: !(Maybe Text)
  , _dprersResponseStatus    :: !Int
  , _dprersPullRequestEvents :: ![PullRequestEvent]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePullRequestEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprersNextToken' - An enumeration token that can be used in a request to return the next batch of the results.
--
-- * 'dprersResponseStatus' - -- | The response status code.
--
-- * 'dprersPullRequestEvents' - Information about the pull request events.
describePullRequestEventsResponse
    :: Int -- ^ 'dprersResponseStatus'
    -> DescribePullRequestEventsResponse
describePullRequestEventsResponse pResponseStatus_ =
  DescribePullRequestEventsResponse'
    { _dprersNextToken = Nothing
    , _dprersResponseStatus = pResponseStatus_
    , _dprersPullRequestEvents = mempty
    }


-- | An enumeration token that can be used in a request to return the next batch of the results.
dprersNextToken :: Lens' DescribePullRequestEventsResponse (Maybe Text)
dprersNextToken = lens _dprersNextToken (\ s a -> s{_dprersNextToken = a})

-- | -- | The response status code.
dprersResponseStatus :: Lens' DescribePullRequestEventsResponse Int
dprersResponseStatus = lens _dprersResponseStatus (\ s a -> s{_dprersResponseStatus = a})

-- | Information about the pull request events.
dprersPullRequestEvents :: Lens' DescribePullRequestEventsResponse [PullRequestEvent]
dprersPullRequestEvents = lens _dprersPullRequestEvents (\ s a -> s{_dprersPullRequestEvents = a}) . _Coerce

instance NFData DescribePullRequestEventsResponse
         where
