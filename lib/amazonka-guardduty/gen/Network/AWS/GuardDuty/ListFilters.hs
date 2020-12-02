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
-- Module      : Network.AWS.GuardDuty.ListFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of the current filters.
--
--
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListFilters
  ( -- * Creating a Request
    listFilters,
    ListFilters,

    -- * Request Lenses
    lNextToken,
    lMaxResults,
    lDetectorId,

    -- * Destructuring the Response
    listFiltersResponse,
    ListFiltersResponse,

    -- * Response Lenses
    lrsNextToken,
    lrsResponseStatus,
    lrsFilterNames,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFilters' smart constructor.
data ListFilters = ListFilters'
  { _lNextToken :: !(Maybe Text),
    _lMaxResults :: !(Maybe Nat),
    _lDetectorId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- * 'lMaxResults' - You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
--
-- * 'lDetectorId' - The unique ID of the detector that the filter is associated with.
listFilters ::
  -- | 'lDetectorId'
  Text ->
  ListFilters
listFilters pDetectorId_ =
  ListFilters'
    { _lNextToken = Nothing,
      _lMaxResults = Nothing,
      _lDetectorId = pDetectorId_
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
lNextToken :: Lens' ListFilters (Maybe Text)
lNextToken = lens _lNextToken (\s a -> s {_lNextToken = a})

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
lMaxResults :: Lens' ListFilters (Maybe Natural)
lMaxResults = lens _lMaxResults (\s a -> s {_lMaxResults = a}) . mapping _Nat

-- | The unique ID of the detector that the filter is associated with.
lDetectorId :: Lens' ListFilters Text
lDetectorId = lens _lDetectorId (\s a -> s {_lDetectorId = a})

instance AWSPager ListFilters where
  page rq rs
    | stop (rs ^. lrsNextToken) = Nothing
    | stop (rs ^. lrsFilterNames) = Nothing
    | otherwise = Just $ rq & lNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListFilters where
  type Rs ListFilters = ListFiltersResponse
  request = get guardDuty
  response =
    receiveJSON
      ( \s h x ->
          ListFiltersResponse'
            <$> (x .?> "nextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "filterNames" .!@ mempty)
      )

instance Hashable ListFilters

instance NFData ListFilters

instance ToHeaders ListFilters where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListFilters where
  toPath ListFilters' {..} =
    mconcat ["/detector/", toBS _lDetectorId, "/filter"]

instance ToQuery ListFilters where
  toQuery ListFilters' {..} =
    mconcat
      ["nextToken" =: _lNextToken, "maxResults" =: _lMaxResults]

-- | /See:/ 'listFiltersResponse' smart constructor.
data ListFiltersResponse = ListFiltersResponse'
  { _lrsNextToken ::
      !(Maybe Text),
    _lrsResponseStatus :: !Int,
    _lrsFilterNames :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFiltersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
--
-- * 'lrsResponseStatus' - -- | The response status code.
--
-- * 'lrsFilterNames' - A list of filter names.
listFiltersResponse ::
  -- | 'lrsResponseStatus'
  Int ->
  ListFiltersResponse
listFiltersResponse pResponseStatus_ =
  ListFiltersResponse'
    { _lrsNextToken = Nothing,
      _lrsResponseStatus = pResponseStatus_,
      _lrsFilterNames = mempty
    }

-- | The pagination parameter to be used on the next list operation to retrieve more items.
lrsNextToken :: Lens' ListFiltersResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\s a -> s {_lrsNextToken = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListFiltersResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\s a -> s {_lrsResponseStatus = a})

-- | A list of filter names.
lrsFilterNames :: Lens' ListFiltersResponse [Text]
lrsFilterNames = lens _lrsFilterNames (\s a -> s {_lrsFilterNames = a}) . _Coerce

instance NFData ListFiltersResponse
