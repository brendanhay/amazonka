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
-- Module      : Network.AWS.SageMaker.ListExperiments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the experiments in your account. The list can be filtered to show only experiments that were created in a specific time range. The list can be sorted by experiment name or creation time.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListExperiments
  ( -- * Creating a Request
    listExperiments,
    ListExperiments,

    -- * Request Lenses
    leCreatedAfter,
    leNextToken,
    leSortOrder,
    leMaxResults,
    leCreatedBefore,
    leSortBy,

    -- * Destructuring the Response
    listExperimentsResponse,
    ListExperimentsResponse,

    -- * Response Lenses
    lersExperimentSummaries,
    lersNextToken,
    lersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listExperiments' smart constructor.
data ListExperiments = ListExperiments'
  { _leCreatedAfter ::
      !(Maybe POSIX),
    _leNextToken :: !(Maybe Text),
    _leSortOrder :: !(Maybe SortOrder),
    _leMaxResults :: !(Maybe Nat),
    _leCreatedBefore :: !(Maybe POSIX),
    _leSortBy :: !(Maybe SortExperimentsBy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListExperiments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leCreatedAfter' - A filter that returns only experiments created after the specified time.
--
-- * 'leNextToken' - If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
--
-- * 'leSortOrder' - The sort order. The default value is @Descending@ .
--
-- * 'leMaxResults' - The maximum number of experiments to return in the response. The default value is 10.
--
-- * 'leCreatedBefore' - A filter that returns only experiments created before the specified time.
--
-- * 'leSortBy' - The property used to sort results. The default value is @CreationTime@ .
listExperiments ::
  ListExperiments
listExperiments =
  ListExperiments'
    { _leCreatedAfter = Nothing,
      _leNextToken = Nothing,
      _leSortOrder = Nothing,
      _leMaxResults = Nothing,
      _leCreatedBefore = Nothing,
      _leSortBy = Nothing
    }

-- | A filter that returns only experiments created after the specified time.
leCreatedAfter :: Lens' ListExperiments (Maybe UTCTime)
leCreatedAfter = lens _leCreatedAfter (\s a -> s {_leCreatedAfter = a}) . mapping _Time

-- | If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
leNextToken :: Lens' ListExperiments (Maybe Text)
leNextToken = lens _leNextToken (\s a -> s {_leNextToken = a})

-- | The sort order. The default value is @Descending@ .
leSortOrder :: Lens' ListExperiments (Maybe SortOrder)
leSortOrder = lens _leSortOrder (\s a -> s {_leSortOrder = a})

-- | The maximum number of experiments to return in the response. The default value is 10.
leMaxResults :: Lens' ListExperiments (Maybe Natural)
leMaxResults = lens _leMaxResults (\s a -> s {_leMaxResults = a}) . mapping _Nat

-- | A filter that returns only experiments created before the specified time.
leCreatedBefore :: Lens' ListExperiments (Maybe UTCTime)
leCreatedBefore = lens _leCreatedBefore (\s a -> s {_leCreatedBefore = a}) . mapping _Time

-- | The property used to sort results. The default value is @CreationTime@ .
leSortBy :: Lens' ListExperiments (Maybe SortExperimentsBy)
leSortBy = lens _leSortBy (\s a -> s {_leSortBy = a})

instance AWSPager ListExperiments where
  page rq rs
    | stop (rs ^. lersNextToken) = Nothing
    | stop (rs ^. lersExperimentSummaries) = Nothing
    | otherwise = Just $ rq & leNextToken .~ rs ^. lersNextToken

instance AWSRequest ListExperiments where
  type Rs ListExperiments = ListExperimentsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListExperimentsResponse'
            <$> (x .?> "ExperimentSummaries" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListExperiments

instance NFData ListExperiments

instance ToHeaders ListExperiments where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListExperiments" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListExperiments where
  toJSON ListExperiments' {..} =
    object
      ( catMaybes
          [ ("CreatedAfter" .=) <$> _leCreatedAfter,
            ("NextToken" .=) <$> _leNextToken,
            ("SortOrder" .=) <$> _leSortOrder,
            ("MaxResults" .=) <$> _leMaxResults,
            ("CreatedBefore" .=) <$> _leCreatedBefore,
            ("SortBy" .=) <$> _leSortBy
          ]
      )

instance ToPath ListExperiments where
  toPath = const "/"

instance ToQuery ListExperiments where
  toQuery = const mempty

-- | /See:/ 'listExperimentsResponse' smart constructor.
data ListExperimentsResponse = ListExperimentsResponse'
  { _lersExperimentSummaries ::
      !(Maybe [ExperimentSummary]),
    _lersNextToken :: !(Maybe Text),
    _lersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListExperimentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lersExperimentSummaries' - A list of the summaries of your experiments.
--
-- * 'lersNextToken' - A token for getting the next set of experiments, if there are any.
--
-- * 'lersResponseStatus' - -- | The response status code.
listExperimentsResponse ::
  -- | 'lersResponseStatus'
  Int ->
  ListExperimentsResponse
listExperimentsResponse pResponseStatus_ =
  ListExperimentsResponse'
    { _lersExperimentSummaries = Nothing,
      _lersNextToken = Nothing,
      _lersResponseStatus = pResponseStatus_
    }

-- | A list of the summaries of your experiments.
lersExperimentSummaries :: Lens' ListExperimentsResponse [ExperimentSummary]
lersExperimentSummaries = lens _lersExperimentSummaries (\s a -> s {_lersExperimentSummaries = a}) . _Default . _Coerce

-- | A token for getting the next set of experiments, if there are any.
lersNextToken :: Lens' ListExperimentsResponse (Maybe Text)
lersNextToken = lens _lersNextToken (\s a -> s {_lersNextToken = a})

-- | -- | The response status code.
lersResponseStatus :: Lens' ListExperimentsResponse Int
lersResponseStatus = lens _lersResponseStatus (\s a -> s {_lersResponseStatus = a})

instance NFData ListExperimentsResponse
