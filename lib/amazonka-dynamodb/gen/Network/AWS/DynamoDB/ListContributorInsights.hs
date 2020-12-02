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
-- Module      : Network.AWS.DynamoDB.ListContributorInsights
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ContributorInsightsSummary for a table and all its global secondary indexes.
module Network.AWS.DynamoDB.ListContributorInsights
  ( -- * Creating a Request
    listContributorInsights,
    ListContributorInsights,

    -- * Request Lenses
    lciNextToken,
    lciMaxResults,
    lciTableName,

    -- * Destructuring the Response
    listContributorInsightsResponse,
    ListContributorInsightsResponse,

    -- * Response Lenses
    lcirsContributorInsightsSummaries,
    lcirsNextToken,
    lcirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listContributorInsights' smart constructor.
data ListContributorInsights = ListContributorInsights'
  { _lciNextToken ::
      !(Maybe Text),
    _lciMaxResults :: !(Maybe Int),
    _lciTableName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListContributorInsights' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lciNextToken' - A token to for the desired page, if there is one.
--
-- * 'lciMaxResults' - Maximum number of results to return per page.
--
-- * 'lciTableName' - The name of the table.
listContributorInsights ::
  ListContributorInsights
listContributorInsights =
  ListContributorInsights'
    { _lciNextToken = Nothing,
      _lciMaxResults = Nothing,
      _lciTableName = Nothing
    }

-- | A token to for the desired page, if there is one.
lciNextToken :: Lens' ListContributorInsights (Maybe Text)
lciNextToken = lens _lciNextToken (\s a -> s {_lciNextToken = a})

-- | Maximum number of results to return per page.
lciMaxResults :: Lens' ListContributorInsights (Maybe Int)
lciMaxResults = lens _lciMaxResults (\s a -> s {_lciMaxResults = a})

-- | The name of the table.
lciTableName :: Lens' ListContributorInsights (Maybe Text)
lciTableName = lens _lciTableName (\s a -> s {_lciTableName = a})

instance AWSRequest ListContributorInsights where
  type Rs ListContributorInsights = ListContributorInsightsResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          ListContributorInsightsResponse'
            <$> (x .?> "ContributorInsightsSummaries" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListContributorInsights

instance NFData ListContributorInsights

instance ToHeaders ListContributorInsights where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.ListContributorInsights" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON ListContributorInsights where
  toJSON ListContributorInsights' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lciNextToken,
            ("MaxResults" .=) <$> _lciMaxResults,
            ("TableName" .=) <$> _lciTableName
          ]
      )

instance ToPath ListContributorInsights where
  toPath = const "/"

instance ToQuery ListContributorInsights where
  toQuery = const mempty

-- | /See:/ 'listContributorInsightsResponse' smart constructor.
data ListContributorInsightsResponse = ListContributorInsightsResponse'
  { _lcirsContributorInsightsSummaries ::
      !( Maybe
           [ContributorInsightsSummary]
       ),
    _lcirsNextToken ::
      !(Maybe Text),
    _lcirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListContributorInsightsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcirsContributorInsightsSummaries' - A list of ContributorInsightsSummary.
--
-- * 'lcirsNextToken' - A token to go to the next page if there is one.
--
-- * 'lcirsResponseStatus' - -- | The response status code.
listContributorInsightsResponse ::
  -- | 'lcirsResponseStatus'
  Int ->
  ListContributorInsightsResponse
listContributorInsightsResponse pResponseStatus_ =
  ListContributorInsightsResponse'
    { _lcirsContributorInsightsSummaries =
        Nothing,
      _lcirsNextToken = Nothing,
      _lcirsResponseStatus = pResponseStatus_
    }

-- | A list of ContributorInsightsSummary.
lcirsContributorInsightsSummaries :: Lens' ListContributorInsightsResponse [ContributorInsightsSummary]
lcirsContributorInsightsSummaries = lens _lcirsContributorInsightsSummaries (\s a -> s {_lcirsContributorInsightsSummaries = a}) . _Default . _Coerce

-- | A token to go to the next page if there is one.
lcirsNextToken :: Lens' ListContributorInsightsResponse (Maybe Text)
lcirsNextToken = lens _lcirsNextToken (\s a -> s {_lcirsNextToken = a})

-- | -- | The response status code.
lcirsResponseStatus :: Lens' ListContributorInsightsResponse Int
lcirsResponseStatus = lens _lcirsResponseStatus (\s a -> s {_lcirsResponseStatus = a})

instance NFData ListContributorInsightsResponse
