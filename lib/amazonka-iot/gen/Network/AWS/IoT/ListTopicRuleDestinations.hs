{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListTopicRuleDestinations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the topic rule destinations in your AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTopicRuleDestinations
  ( -- * Creating a Request
    listTopicRuleDestinations,
    ListTopicRuleDestinations,

    -- * Request Lenses
    ltrdNextToken,
    ltrdMaxResults,

    -- * Destructuring the Response
    listTopicRuleDestinationsResponse,
    ListTopicRuleDestinationsResponse,

    -- * Response Lenses
    ltrdrsDestinationSummaries,
    ltrdrsNextToken,
    ltrdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTopicRuleDestinations' smart constructor.
data ListTopicRuleDestinations = ListTopicRuleDestinations'
  { _ltrdNextToken ::
      !(Maybe Text),
    _ltrdMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTopicRuleDestinations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrdNextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- * 'ltrdMaxResults' - The maximum number of results to return at one time.
listTopicRuleDestinations ::
  ListTopicRuleDestinations
listTopicRuleDestinations =
  ListTopicRuleDestinations'
    { _ltrdNextToken = Nothing,
      _ltrdMaxResults = Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
ltrdNextToken :: Lens' ListTopicRuleDestinations (Maybe Text)
ltrdNextToken = lens _ltrdNextToken (\s a -> s {_ltrdNextToken = a})

-- | The maximum number of results to return at one time.
ltrdMaxResults :: Lens' ListTopicRuleDestinations (Maybe Natural)
ltrdMaxResults = lens _ltrdMaxResults (\s a -> s {_ltrdMaxResults = a}) . mapping _Nat

instance AWSPager ListTopicRuleDestinations where
  page rq rs
    | stop (rs ^. ltrdrsNextToken) = Nothing
    | stop (rs ^. ltrdrsDestinationSummaries) = Nothing
    | otherwise = Just $ rq & ltrdNextToken .~ rs ^. ltrdrsNextToken

instance AWSRequest ListTopicRuleDestinations where
  type
    Rs ListTopicRuleDestinations =
      ListTopicRuleDestinationsResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListTopicRuleDestinationsResponse'
            <$> (x .?> "destinationSummaries" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListTopicRuleDestinations

instance NFData ListTopicRuleDestinations

instance ToHeaders ListTopicRuleDestinations where
  toHeaders = const mempty

instance ToPath ListTopicRuleDestinations where
  toPath = const "/destinations"

instance ToQuery ListTopicRuleDestinations where
  toQuery ListTopicRuleDestinations' {..} =
    mconcat
      ["nextToken" =: _ltrdNextToken, "maxResults" =: _ltrdMaxResults]

-- | /See:/ 'listTopicRuleDestinationsResponse' smart constructor.
data ListTopicRuleDestinationsResponse = ListTopicRuleDestinationsResponse'
  { _ltrdrsDestinationSummaries ::
      !( Maybe
           [TopicRuleDestinationSummary]
       ),
    _ltrdrsNextToken ::
      !(Maybe Text),
    _ltrdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTopicRuleDestinationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrdrsDestinationSummaries' - Information about a topic rule destination.
--
-- * 'ltrdrsNextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- * 'ltrdrsResponseStatus' - -- | The response status code.
listTopicRuleDestinationsResponse ::
  -- | 'ltrdrsResponseStatus'
  Int ->
  ListTopicRuleDestinationsResponse
listTopicRuleDestinationsResponse pResponseStatus_ =
  ListTopicRuleDestinationsResponse'
    { _ltrdrsDestinationSummaries =
        Nothing,
      _ltrdrsNextToken = Nothing,
      _ltrdrsResponseStatus = pResponseStatus_
    }

-- | Information about a topic rule destination.
ltrdrsDestinationSummaries :: Lens' ListTopicRuleDestinationsResponse [TopicRuleDestinationSummary]
ltrdrsDestinationSummaries = lens _ltrdrsDestinationSummaries (\s a -> s {_ltrdrsDestinationSummaries = a}) . _Default . _Coerce

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
ltrdrsNextToken :: Lens' ListTopicRuleDestinationsResponse (Maybe Text)
ltrdrsNextToken = lens _ltrdrsNextToken (\s a -> s {_ltrdrsNextToken = a})

-- | -- | The response status code.
ltrdrsResponseStatus :: Lens' ListTopicRuleDestinationsResponse Int
ltrdrsResponseStatus = lens _ltrdrsResponseStatus (\s a -> s {_ltrdrsResponseStatus = a})

instance NFData ListTopicRuleDestinationsResponse
