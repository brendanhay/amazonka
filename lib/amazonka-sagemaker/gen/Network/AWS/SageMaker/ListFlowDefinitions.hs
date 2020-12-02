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
-- Module      : Network.AWS.SageMaker.ListFlowDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the flow definitions in your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListFlowDefinitions
  ( -- * Creating a Request
    listFlowDefinitions,
    ListFlowDefinitions,

    -- * Request Lenses
    lfdCreationTimeAfter,
    lfdNextToken,
    lfdSortOrder,
    lfdCreationTimeBefore,
    lfdMaxResults,

    -- * Destructuring the Response
    listFlowDefinitionsResponse,
    ListFlowDefinitionsResponse,

    -- * Response Lenses
    lfdrsNextToken,
    lfdrsResponseStatus,
    lfdrsFlowDefinitionSummaries,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listFlowDefinitions' smart constructor.
data ListFlowDefinitions = ListFlowDefinitions'
  { _lfdCreationTimeAfter ::
      !(Maybe POSIX),
    _lfdNextToken :: !(Maybe Text),
    _lfdSortOrder :: !(Maybe SortOrder),
    _lfdCreationTimeBefore :: !(Maybe POSIX),
    _lfdMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFlowDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfdCreationTimeAfter' - A filter that returns only flow definitions with a creation time greater than or equal to the specified timestamp.
--
-- * 'lfdNextToken' - A token to resume pagination.
--
-- * 'lfdSortOrder' - An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
--
-- * 'lfdCreationTimeBefore' - A filter that returns only flow definitions that were created before the specified timestamp.
--
-- * 'lfdMaxResults' - The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
listFlowDefinitions ::
  ListFlowDefinitions
listFlowDefinitions =
  ListFlowDefinitions'
    { _lfdCreationTimeAfter = Nothing,
      _lfdNextToken = Nothing,
      _lfdSortOrder = Nothing,
      _lfdCreationTimeBefore = Nothing,
      _lfdMaxResults = Nothing
    }

-- | A filter that returns only flow definitions with a creation time greater than or equal to the specified timestamp.
lfdCreationTimeAfter :: Lens' ListFlowDefinitions (Maybe UTCTime)
lfdCreationTimeAfter = lens _lfdCreationTimeAfter (\s a -> s {_lfdCreationTimeAfter = a}) . mapping _Time

-- | A token to resume pagination.
lfdNextToken :: Lens' ListFlowDefinitions (Maybe Text)
lfdNextToken = lens _lfdNextToken (\s a -> s {_lfdNextToken = a})

-- | An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
lfdSortOrder :: Lens' ListFlowDefinitions (Maybe SortOrder)
lfdSortOrder = lens _lfdSortOrder (\s a -> s {_lfdSortOrder = a})

-- | A filter that returns only flow definitions that were created before the specified timestamp.
lfdCreationTimeBefore :: Lens' ListFlowDefinitions (Maybe UTCTime)
lfdCreationTimeBefore = lens _lfdCreationTimeBefore (\s a -> s {_lfdCreationTimeBefore = a}) . mapping _Time

-- | The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
lfdMaxResults :: Lens' ListFlowDefinitions (Maybe Natural)
lfdMaxResults = lens _lfdMaxResults (\s a -> s {_lfdMaxResults = a}) . mapping _Nat

instance AWSPager ListFlowDefinitions where
  page rq rs
    | stop (rs ^. lfdrsNextToken) = Nothing
    | stop (rs ^. lfdrsFlowDefinitionSummaries) = Nothing
    | otherwise = Just $ rq & lfdNextToken .~ rs ^. lfdrsNextToken

instance AWSRequest ListFlowDefinitions where
  type Rs ListFlowDefinitions = ListFlowDefinitionsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListFlowDefinitionsResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "FlowDefinitionSummaries" .!@ mempty)
      )

instance Hashable ListFlowDefinitions

instance NFData ListFlowDefinitions

instance ToHeaders ListFlowDefinitions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListFlowDefinitions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListFlowDefinitions where
  toJSON ListFlowDefinitions' {..} =
    object
      ( catMaybes
          [ ("CreationTimeAfter" .=) <$> _lfdCreationTimeAfter,
            ("NextToken" .=) <$> _lfdNextToken,
            ("SortOrder" .=) <$> _lfdSortOrder,
            ("CreationTimeBefore" .=) <$> _lfdCreationTimeBefore,
            ("MaxResults" .=) <$> _lfdMaxResults
          ]
      )

instance ToPath ListFlowDefinitions where
  toPath = const "/"

instance ToQuery ListFlowDefinitions where
  toQuery = const mempty

-- | /See:/ 'listFlowDefinitionsResponse' smart constructor.
data ListFlowDefinitionsResponse = ListFlowDefinitionsResponse'
  { _lfdrsNextToken ::
      !(Maybe Text),
    _lfdrsResponseStatus :: !Int,
    _lfdrsFlowDefinitionSummaries ::
      ![FlowDefinitionSummary]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFlowDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfdrsNextToken' - A token to resume pagination.
--
-- * 'lfdrsResponseStatus' - -- | The response status code.
--
-- * 'lfdrsFlowDefinitionSummaries' - An array of objects describing the flow definitions.
listFlowDefinitionsResponse ::
  -- | 'lfdrsResponseStatus'
  Int ->
  ListFlowDefinitionsResponse
listFlowDefinitionsResponse pResponseStatus_ =
  ListFlowDefinitionsResponse'
    { _lfdrsNextToken = Nothing,
      _lfdrsResponseStatus = pResponseStatus_,
      _lfdrsFlowDefinitionSummaries = mempty
    }

-- | A token to resume pagination.
lfdrsNextToken :: Lens' ListFlowDefinitionsResponse (Maybe Text)
lfdrsNextToken = lens _lfdrsNextToken (\s a -> s {_lfdrsNextToken = a})

-- | -- | The response status code.
lfdrsResponseStatus :: Lens' ListFlowDefinitionsResponse Int
lfdrsResponseStatus = lens _lfdrsResponseStatus (\s a -> s {_lfdrsResponseStatus = a})

-- | An array of objects describing the flow definitions.
lfdrsFlowDefinitionSummaries :: Lens' ListFlowDefinitionsResponse [FlowDefinitionSummary]
lfdrsFlowDefinitionSummaries = lens _lfdrsFlowDefinitionSummaries (\s a -> s {_lfdrsFlowDefinitionSummaries = a}) . _Coerce

instance NFData ListFlowDefinitionsResponse
