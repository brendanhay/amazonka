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
-- Module      : Network.AWS.SageMaker.ListTrialComponents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the trial components in your account. You can sort the list by trial component name or creation time. You can filter the list to show only components that were created in a specific time range. You can also filter on one of the following:
--
--
--     * @ExperimentName@
--
--     * @SourceArn@
--
--     * @TrialName@
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrialComponents
  ( -- * Creating a Request
    listTrialComponents,
    ListTrialComponents,

    -- * Request Lenses
    ltcCreatedAfter,
    ltcSourceARN,
    ltcExperimentName,
    ltcNextToken,
    ltcSortOrder,
    ltcTrialName,
    ltcMaxResults,
    ltcCreatedBefore,
    ltcSortBy,

    -- * Destructuring the Response
    listTrialComponentsResponse,
    ListTrialComponentsResponse,

    -- * Response Lenses
    ltcrsTrialComponentSummaries,
    ltcrsNextToken,
    ltcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listTrialComponents' smart constructor.
data ListTrialComponents = ListTrialComponents'
  { _ltcCreatedAfter ::
      !(Maybe POSIX),
    _ltcSourceARN :: !(Maybe Text),
    _ltcExperimentName :: !(Maybe Text),
    _ltcNextToken :: !(Maybe Text),
    _ltcSortOrder :: !(Maybe SortOrder),
    _ltcTrialName :: !(Maybe Text),
    _ltcMaxResults :: !(Maybe Nat),
    _ltcCreatedBefore :: !(Maybe POSIX),
    _ltcSortBy :: !(Maybe SortTrialComponentsBy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTrialComponents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltcCreatedAfter' - A filter that returns only components created after the specified time.
--
-- * 'ltcSourceARN' - A filter that returns only components that have the specified source Amazon Resource Name (ARN). If you specify @SourceArn@ , you can't filter by @ExperimentName@ or @TrialName@ .
--
-- * 'ltcExperimentName' - A filter that returns only components that are part of the specified experiment. If you specify @ExperimentName@ , you can't filter by @SourceArn@ or @TrialName@ .
--
-- * 'ltcNextToken' - If the previous call to @ListTrialComponents@ didn't return the full set of components, the call returns a token for getting the next set of components.
--
-- * 'ltcSortOrder' - The sort order. The default value is @Descending@ .
--
-- * 'ltcTrialName' - A filter that returns only components that are part of the specified trial. If you specify @TrialName@ , you can't filter by @ExperimentName@ or @SourceArn@ .
--
-- * 'ltcMaxResults' - The maximum number of components to return in the response. The default value is 10.
--
-- * 'ltcCreatedBefore' - A filter that returns only components created before the specified time.
--
-- * 'ltcSortBy' - The property used to sort results. The default value is @CreationTime@ .
listTrialComponents ::
  ListTrialComponents
listTrialComponents =
  ListTrialComponents'
    { _ltcCreatedAfter = Nothing,
      _ltcSourceARN = Nothing,
      _ltcExperimentName = Nothing,
      _ltcNextToken = Nothing,
      _ltcSortOrder = Nothing,
      _ltcTrialName = Nothing,
      _ltcMaxResults = Nothing,
      _ltcCreatedBefore = Nothing,
      _ltcSortBy = Nothing
    }

-- | A filter that returns only components created after the specified time.
ltcCreatedAfter :: Lens' ListTrialComponents (Maybe UTCTime)
ltcCreatedAfter = lens _ltcCreatedAfter (\s a -> s {_ltcCreatedAfter = a}) . mapping _Time

-- | A filter that returns only components that have the specified source Amazon Resource Name (ARN). If you specify @SourceArn@ , you can't filter by @ExperimentName@ or @TrialName@ .
ltcSourceARN :: Lens' ListTrialComponents (Maybe Text)
ltcSourceARN = lens _ltcSourceARN (\s a -> s {_ltcSourceARN = a})

-- | A filter that returns only components that are part of the specified experiment. If you specify @ExperimentName@ , you can't filter by @SourceArn@ or @TrialName@ .
ltcExperimentName :: Lens' ListTrialComponents (Maybe Text)
ltcExperimentName = lens _ltcExperimentName (\s a -> s {_ltcExperimentName = a})

-- | If the previous call to @ListTrialComponents@ didn't return the full set of components, the call returns a token for getting the next set of components.
ltcNextToken :: Lens' ListTrialComponents (Maybe Text)
ltcNextToken = lens _ltcNextToken (\s a -> s {_ltcNextToken = a})

-- | The sort order. The default value is @Descending@ .
ltcSortOrder :: Lens' ListTrialComponents (Maybe SortOrder)
ltcSortOrder = lens _ltcSortOrder (\s a -> s {_ltcSortOrder = a})

-- | A filter that returns only components that are part of the specified trial. If you specify @TrialName@ , you can't filter by @ExperimentName@ or @SourceArn@ .
ltcTrialName :: Lens' ListTrialComponents (Maybe Text)
ltcTrialName = lens _ltcTrialName (\s a -> s {_ltcTrialName = a})

-- | The maximum number of components to return in the response. The default value is 10.
ltcMaxResults :: Lens' ListTrialComponents (Maybe Natural)
ltcMaxResults = lens _ltcMaxResults (\s a -> s {_ltcMaxResults = a}) . mapping _Nat

-- | A filter that returns only components created before the specified time.
ltcCreatedBefore :: Lens' ListTrialComponents (Maybe UTCTime)
ltcCreatedBefore = lens _ltcCreatedBefore (\s a -> s {_ltcCreatedBefore = a}) . mapping _Time

-- | The property used to sort results. The default value is @CreationTime@ .
ltcSortBy :: Lens' ListTrialComponents (Maybe SortTrialComponentsBy)
ltcSortBy = lens _ltcSortBy (\s a -> s {_ltcSortBy = a})

instance AWSPager ListTrialComponents where
  page rq rs
    | stop (rs ^. ltcrsNextToken) = Nothing
    | stop (rs ^. ltcrsTrialComponentSummaries) = Nothing
    | otherwise = Just $ rq & ltcNextToken .~ rs ^. ltcrsNextToken

instance AWSRequest ListTrialComponents where
  type Rs ListTrialComponents = ListTrialComponentsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListTrialComponentsResponse'
            <$> (x .?> "TrialComponentSummaries" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListTrialComponents

instance NFData ListTrialComponents

instance ToHeaders ListTrialComponents where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListTrialComponents" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListTrialComponents where
  toJSON ListTrialComponents' {..} =
    object
      ( catMaybes
          [ ("CreatedAfter" .=) <$> _ltcCreatedAfter,
            ("SourceArn" .=) <$> _ltcSourceARN,
            ("ExperimentName" .=) <$> _ltcExperimentName,
            ("NextToken" .=) <$> _ltcNextToken,
            ("SortOrder" .=) <$> _ltcSortOrder,
            ("TrialName" .=) <$> _ltcTrialName,
            ("MaxResults" .=) <$> _ltcMaxResults,
            ("CreatedBefore" .=) <$> _ltcCreatedBefore,
            ("SortBy" .=) <$> _ltcSortBy
          ]
      )

instance ToPath ListTrialComponents where
  toPath = const "/"

instance ToQuery ListTrialComponents where
  toQuery = const mempty

-- | /See:/ 'listTrialComponentsResponse' smart constructor.
data ListTrialComponentsResponse = ListTrialComponentsResponse'
  { _ltcrsTrialComponentSummaries ::
      !(Maybe [TrialComponentSummary]),
    _ltcrsNextToken :: !(Maybe Text),
    _ltcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTrialComponentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltcrsTrialComponentSummaries' - A list of the summaries of your trial components.
--
-- * 'ltcrsNextToken' - A token for getting the next set of components, if there are any.
--
-- * 'ltcrsResponseStatus' - -- | The response status code.
listTrialComponentsResponse ::
  -- | 'ltcrsResponseStatus'
  Int ->
  ListTrialComponentsResponse
listTrialComponentsResponse pResponseStatus_ =
  ListTrialComponentsResponse'
    { _ltcrsTrialComponentSummaries =
        Nothing,
      _ltcrsNextToken = Nothing,
      _ltcrsResponseStatus = pResponseStatus_
    }

-- | A list of the summaries of your trial components.
ltcrsTrialComponentSummaries :: Lens' ListTrialComponentsResponse [TrialComponentSummary]
ltcrsTrialComponentSummaries = lens _ltcrsTrialComponentSummaries (\s a -> s {_ltcrsTrialComponentSummaries = a}) . _Default . _Coerce

-- | A token for getting the next set of components, if there are any.
ltcrsNextToken :: Lens' ListTrialComponentsResponse (Maybe Text)
ltcrsNextToken = lens _ltcrsNextToken (\s a -> s {_ltcrsNextToken = a})

-- | -- | The response status code.
ltcrsResponseStatus :: Lens' ListTrialComponentsResponse Int
ltcrsResponseStatus = lens _ltcrsResponseStatus (\s a -> s {_ltcrsResponseStatus = a})

instance NFData ListTrialComponentsResponse
