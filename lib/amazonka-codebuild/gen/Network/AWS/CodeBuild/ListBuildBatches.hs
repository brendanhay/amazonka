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
-- Module      : Network.AWS.CodeBuild.ListBuildBatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the identifiers of your build batches in the current region.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildBatches
  ( -- * Creating a Request
    listBuildBatches,
    ListBuildBatches,

    -- * Request Lenses
    lbbSortOrder,
    lbbNextToken,
    lbbFilter,
    lbbMaxResults,

    -- * Destructuring the Response
    listBuildBatchesResponse,
    ListBuildBatchesResponse,

    -- * Response Lenses
    lbbrsIds,
    lbbrsNextToken,
    lbbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBuildBatches' smart constructor.
data ListBuildBatches = ListBuildBatches'
  { _lbbSortOrder ::
      !(Maybe SortOrderType),
    _lbbNextToken :: !(Maybe Text),
    _lbbFilter :: !(Maybe BuildBatchFilter),
    _lbbMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBuildBatches' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbbSortOrder' - Specifies the sort order of the returned items. Valid values include:     * @ASCENDING@ : List the batch build identifiers in ascending order by identifier.     * @DESCENDING@ : List the batch build identifiers in descending order by identifier.
--
-- * 'lbbNextToken' - The @nextToken@ value returned from a previous call to @ListBuildBatches@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
--
-- * 'lbbFilter' - A @BuildBatchFilter@ object that specifies the filters for the search.
--
-- * 'lbbMaxResults' - The maximum number of results to return.
listBuildBatches ::
  ListBuildBatches
listBuildBatches =
  ListBuildBatches'
    { _lbbSortOrder = Nothing,
      _lbbNextToken = Nothing,
      _lbbFilter = Nothing,
      _lbbMaxResults = Nothing
    }

-- | Specifies the sort order of the returned items. Valid values include:     * @ASCENDING@ : List the batch build identifiers in ascending order by identifier.     * @DESCENDING@ : List the batch build identifiers in descending order by identifier.
lbbSortOrder :: Lens' ListBuildBatches (Maybe SortOrderType)
lbbSortOrder = lens _lbbSortOrder (\s a -> s {_lbbSortOrder = a})

-- | The @nextToken@ value returned from a previous call to @ListBuildBatches@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
lbbNextToken :: Lens' ListBuildBatches (Maybe Text)
lbbNextToken = lens _lbbNextToken (\s a -> s {_lbbNextToken = a})

-- | A @BuildBatchFilter@ object that specifies the filters for the search.
lbbFilter :: Lens' ListBuildBatches (Maybe BuildBatchFilter)
lbbFilter = lens _lbbFilter (\s a -> s {_lbbFilter = a})

-- | The maximum number of results to return.
lbbMaxResults :: Lens' ListBuildBatches (Maybe Natural)
lbbMaxResults = lens _lbbMaxResults (\s a -> s {_lbbMaxResults = a}) . mapping _Nat

instance AWSPager ListBuildBatches where
  page rq rs
    | stop (rs ^. lbbrsNextToken) = Nothing
    | stop (rs ^. lbbrsIds) = Nothing
    | otherwise = Just $ rq & lbbNextToken .~ rs ^. lbbrsNextToken

instance AWSRequest ListBuildBatches where
  type Rs ListBuildBatches = ListBuildBatchesResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          ListBuildBatchesResponse'
            <$> (x .?> "ids" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListBuildBatches

instance NFData ListBuildBatches

instance ToHeaders ListBuildBatches where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.ListBuildBatches" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListBuildBatches where
  toJSON ListBuildBatches' {..} =
    object
      ( catMaybes
          [ ("sortOrder" .=) <$> _lbbSortOrder,
            ("nextToken" .=) <$> _lbbNextToken,
            ("filter" .=) <$> _lbbFilter,
            ("maxResults" .=) <$> _lbbMaxResults
          ]
      )

instance ToPath ListBuildBatches where
  toPath = const "/"

instance ToQuery ListBuildBatches where
  toQuery = const mempty

-- | /See:/ 'listBuildBatchesResponse' smart constructor.
data ListBuildBatchesResponse = ListBuildBatchesResponse'
  { _lbbrsIds ::
      !(Maybe [Text]),
    _lbbrsNextToken :: !(Maybe Text),
    _lbbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBuildBatchesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbbrsIds' - An array of strings that contains the batch build identifiers.
--
-- * 'lbbrsNextToken' - If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatches@ to retrieve the next set of items.
--
-- * 'lbbrsResponseStatus' - -- | The response status code.
listBuildBatchesResponse ::
  -- | 'lbbrsResponseStatus'
  Int ->
  ListBuildBatchesResponse
listBuildBatchesResponse pResponseStatus_ =
  ListBuildBatchesResponse'
    { _lbbrsIds = Nothing,
      _lbbrsNextToken = Nothing,
      _lbbrsResponseStatus = pResponseStatus_
    }

-- | An array of strings that contains the batch build identifiers.
lbbrsIds :: Lens' ListBuildBatchesResponse [Text]
lbbrsIds = lens _lbbrsIds (\s a -> s {_lbbrsIds = a}) . _Default . _Coerce

-- | If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatches@ to retrieve the next set of items.
lbbrsNextToken :: Lens' ListBuildBatchesResponse (Maybe Text)
lbbrsNextToken = lens _lbbrsNextToken (\s a -> s {_lbbrsNextToken = a})

-- | -- | The response status code.
lbbrsResponseStatus :: Lens' ListBuildBatchesResponse Int
lbbrsResponseStatus = lens _lbbrsResponseStatus (\s a -> s {_lbbrsResponseStatus = a})

instance NFData ListBuildBatchesResponse
