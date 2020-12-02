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
-- Module      : Network.AWS.SageMaker.ListTrials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the trials in your account. Specify an experiment name to limit the list to the trials that are part of that experiment. Specify a trial component name to limit the list to the trials that associated with that trial component. The list can be filtered to show only trials that were created in a specific time range. The list can be sorted by trial name or creation time.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrials
  ( -- * Creating a Request
    listTrials,
    ListTrials,

    -- * Request Lenses
    ltsCreatedAfter,
    ltsExperimentName,
    ltsNextToken,
    ltsSortOrder,
    ltsTrialComponentName,
    ltsMaxResults,
    ltsCreatedBefore,
    ltsSortBy,

    -- * Destructuring the Response
    listTrialsResponse,
    ListTrialsResponse,

    -- * Response Lenses
    ltsrsNextToken,
    ltsrsTrialSummaries,
    ltsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listTrials' smart constructor.
data ListTrials = ListTrials'
  { _ltsCreatedAfter :: !(Maybe POSIX),
    _ltsExperimentName :: !(Maybe Text),
    _ltsNextToken :: !(Maybe Text),
    _ltsSortOrder :: !(Maybe SortOrder),
    _ltsTrialComponentName :: !(Maybe Text),
    _ltsMaxResults :: !(Maybe Nat),
    _ltsCreatedBefore :: !(Maybe POSIX),
    _ltsSortBy :: !(Maybe SortTrialsBy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTrials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsCreatedAfter' - A filter that returns only trials created after the specified time.
--
-- * 'ltsExperimentName' - A filter that returns only trials that are part of the specified experiment.
--
-- * 'ltsNextToken' - If the previous call to @ListTrials@ didn't return the full set of trials, the call returns a token for getting the next set of trials.
--
-- * 'ltsSortOrder' - The sort order. The default value is @Descending@ .
--
-- * 'ltsTrialComponentName' - A filter that returns only trials that are associated with the specified trial component.
--
-- * 'ltsMaxResults' - The maximum number of trials to return in the response. The default value is 10.
--
-- * 'ltsCreatedBefore' - A filter that returns only trials created before the specified time.
--
-- * 'ltsSortBy' - The property used to sort results. The default value is @CreationTime@ .
listTrials ::
  ListTrials
listTrials =
  ListTrials'
    { _ltsCreatedAfter = Nothing,
      _ltsExperimentName = Nothing,
      _ltsNextToken = Nothing,
      _ltsSortOrder = Nothing,
      _ltsTrialComponentName = Nothing,
      _ltsMaxResults = Nothing,
      _ltsCreatedBefore = Nothing,
      _ltsSortBy = Nothing
    }

-- | A filter that returns only trials created after the specified time.
ltsCreatedAfter :: Lens' ListTrials (Maybe UTCTime)
ltsCreatedAfter = lens _ltsCreatedAfter (\s a -> s {_ltsCreatedAfter = a}) . mapping _Time

-- | A filter that returns only trials that are part of the specified experiment.
ltsExperimentName :: Lens' ListTrials (Maybe Text)
ltsExperimentName = lens _ltsExperimentName (\s a -> s {_ltsExperimentName = a})

-- | If the previous call to @ListTrials@ didn't return the full set of trials, the call returns a token for getting the next set of trials.
ltsNextToken :: Lens' ListTrials (Maybe Text)
ltsNextToken = lens _ltsNextToken (\s a -> s {_ltsNextToken = a})

-- | The sort order. The default value is @Descending@ .
ltsSortOrder :: Lens' ListTrials (Maybe SortOrder)
ltsSortOrder = lens _ltsSortOrder (\s a -> s {_ltsSortOrder = a})

-- | A filter that returns only trials that are associated with the specified trial component.
ltsTrialComponentName :: Lens' ListTrials (Maybe Text)
ltsTrialComponentName = lens _ltsTrialComponentName (\s a -> s {_ltsTrialComponentName = a})

-- | The maximum number of trials to return in the response. The default value is 10.
ltsMaxResults :: Lens' ListTrials (Maybe Natural)
ltsMaxResults = lens _ltsMaxResults (\s a -> s {_ltsMaxResults = a}) . mapping _Nat

-- | A filter that returns only trials created before the specified time.
ltsCreatedBefore :: Lens' ListTrials (Maybe UTCTime)
ltsCreatedBefore = lens _ltsCreatedBefore (\s a -> s {_ltsCreatedBefore = a}) . mapping _Time

-- | The property used to sort results. The default value is @CreationTime@ .
ltsSortBy :: Lens' ListTrials (Maybe SortTrialsBy)
ltsSortBy = lens _ltsSortBy (\s a -> s {_ltsSortBy = a})

instance AWSPager ListTrials where
  page rq rs
    | stop (rs ^. ltsrsNextToken) = Nothing
    | stop (rs ^. ltsrsTrialSummaries) = Nothing
    | otherwise = Just $ rq & ltsNextToken .~ rs ^. ltsrsNextToken

instance AWSRequest ListTrials where
  type Rs ListTrials = ListTrialsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListTrialsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "TrialSummaries" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListTrials

instance NFData ListTrials

instance ToHeaders ListTrials where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListTrials" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListTrials where
  toJSON ListTrials' {..} =
    object
      ( catMaybes
          [ ("CreatedAfter" .=) <$> _ltsCreatedAfter,
            ("ExperimentName" .=) <$> _ltsExperimentName,
            ("NextToken" .=) <$> _ltsNextToken,
            ("SortOrder" .=) <$> _ltsSortOrder,
            ("TrialComponentName" .=) <$> _ltsTrialComponentName,
            ("MaxResults" .=) <$> _ltsMaxResults,
            ("CreatedBefore" .=) <$> _ltsCreatedBefore,
            ("SortBy" .=) <$> _ltsSortBy
          ]
      )

instance ToPath ListTrials where
  toPath = const "/"

instance ToQuery ListTrials where
  toQuery = const mempty

-- | /See:/ 'listTrialsResponse' smart constructor.
data ListTrialsResponse = ListTrialsResponse'
  { _ltsrsNextToken ::
      !(Maybe Text),
    _ltsrsTrialSummaries :: !(Maybe [TrialSummary]),
    _ltsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTrialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsrsNextToken' - A token for getting the next set of trials, if there are any.
--
-- * 'ltsrsTrialSummaries' - A list of the summaries of your trials.
--
-- * 'ltsrsResponseStatus' - -- | The response status code.
listTrialsResponse ::
  -- | 'ltsrsResponseStatus'
  Int ->
  ListTrialsResponse
listTrialsResponse pResponseStatus_ =
  ListTrialsResponse'
    { _ltsrsNextToken = Nothing,
      _ltsrsTrialSummaries = Nothing,
      _ltsrsResponseStatus = pResponseStatus_
    }

-- | A token for getting the next set of trials, if there are any.
ltsrsNextToken :: Lens' ListTrialsResponse (Maybe Text)
ltsrsNextToken = lens _ltsrsNextToken (\s a -> s {_ltsrsNextToken = a})

-- | A list of the summaries of your trials.
ltsrsTrialSummaries :: Lens' ListTrialsResponse [TrialSummary]
ltsrsTrialSummaries = lens _ltsrsTrialSummaries (\s a -> s {_ltsrsTrialSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
ltsrsResponseStatus :: Lens' ListTrialsResponse Int
ltsrsResponseStatus = lens _ltsrsResponseStatus (\s a -> s {_ltsrsResponseStatus = a})

instance NFData ListTrialsResponse
