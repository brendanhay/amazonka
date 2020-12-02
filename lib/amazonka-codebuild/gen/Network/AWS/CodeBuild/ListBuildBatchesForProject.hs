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
-- Module      : Network.AWS.CodeBuild.ListBuildBatchesForProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the identifiers of the build batches for a specific project.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildBatchesForProject
  ( -- * Creating a Request
    listBuildBatchesForProject,
    ListBuildBatchesForProject,

    -- * Request Lenses
    lbbfpSortOrder,
    lbbfpNextToken,
    lbbfpProjectName,
    lbbfpFilter,
    lbbfpMaxResults,

    -- * Destructuring the Response
    listBuildBatchesForProjectResponse,
    ListBuildBatchesForProjectResponse,

    -- * Response Lenses
    lbbfprsIds,
    lbbfprsNextToken,
    lbbfprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBuildBatchesForProject' smart constructor.
data ListBuildBatchesForProject = ListBuildBatchesForProject'
  { _lbbfpSortOrder ::
      !(Maybe SortOrderType),
    _lbbfpNextToken :: !(Maybe Text),
    _lbbfpProjectName :: !(Maybe Text),
    _lbbfpFilter ::
      !(Maybe BuildBatchFilter),
    _lbbfpMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBuildBatchesForProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbbfpSortOrder' - Specifies the sort order of the returned items. Valid values include:     * @ASCENDING@ : List the batch build identifiers in ascending order by identifier.     * @DESCENDING@ : List the batch build identifiers in descending order by identifier.
--
-- * 'lbbfpNextToken' - The @nextToken@ value returned from a previous call to @ListBuildBatchesForProject@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
--
-- * 'lbbfpProjectName' - The name of the project.
--
-- * 'lbbfpFilter' - A @BuildBatchFilter@ object that specifies the filters for the search.
--
-- * 'lbbfpMaxResults' - The maximum number of results to return.
listBuildBatchesForProject ::
  ListBuildBatchesForProject
listBuildBatchesForProject =
  ListBuildBatchesForProject'
    { _lbbfpSortOrder = Nothing,
      _lbbfpNextToken = Nothing,
      _lbbfpProjectName = Nothing,
      _lbbfpFilter = Nothing,
      _lbbfpMaxResults = Nothing
    }

-- | Specifies the sort order of the returned items. Valid values include:     * @ASCENDING@ : List the batch build identifiers in ascending order by identifier.     * @DESCENDING@ : List the batch build identifiers in descending order by identifier.
lbbfpSortOrder :: Lens' ListBuildBatchesForProject (Maybe SortOrderType)
lbbfpSortOrder = lens _lbbfpSortOrder (\s a -> s {_lbbfpSortOrder = a})

-- | The @nextToken@ value returned from a previous call to @ListBuildBatchesForProject@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
lbbfpNextToken :: Lens' ListBuildBatchesForProject (Maybe Text)
lbbfpNextToken = lens _lbbfpNextToken (\s a -> s {_lbbfpNextToken = a})

-- | The name of the project.
lbbfpProjectName :: Lens' ListBuildBatchesForProject (Maybe Text)
lbbfpProjectName = lens _lbbfpProjectName (\s a -> s {_lbbfpProjectName = a})

-- | A @BuildBatchFilter@ object that specifies the filters for the search.
lbbfpFilter :: Lens' ListBuildBatchesForProject (Maybe BuildBatchFilter)
lbbfpFilter = lens _lbbfpFilter (\s a -> s {_lbbfpFilter = a})

-- | The maximum number of results to return.
lbbfpMaxResults :: Lens' ListBuildBatchesForProject (Maybe Natural)
lbbfpMaxResults = lens _lbbfpMaxResults (\s a -> s {_lbbfpMaxResults = a}) . mapping _Nat

instance AWSPager ListBuildBatchesForProject where
  page rq rs
    | stop (rs ^. lbbfprsNextToken) = Nothing
    | stop (rs ^. lbbfprsIds) = Nothing
    | otherwise = Just $ rq & lbbfpNextToken .~ rs ^. lbbfprsNextToken

instance AWSRequest ListBuildBatchesForProject where
  type
    Rs ListBuildBatchesForProject =
      ListBuildBatchesForProjectResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          ListBuildBatchesForProjectResponse'
            <$> (x .?> "ids" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListBuildBatchesForProject

instance NFData ListBuildBatchesForProject

instance ToHeaders ListBuildBatchesForProject where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.ListBuildBatchesForProject" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListBuildBatchesForProject where
  toJSON ListBuildBatchesForProject' {..} =
    object
      ( catMaybes
          [ ("sortOrder" .=) <$> _lbbfpSortOrder,
            ("nextToken" .=) <$> _lbbfpNextToken,
            ("projectName" .=) <$> _lbbfpProjectName,
            ("filter" .=) <$> _lbbfpFilter,
            ("maxResults" .=) <$> _lbbfpMaxResults
          ]
      )

instance ToPath ListBuildBatchesForProject where
  toPath = const "/"

instance ToQuery ListBuildBatchesForProject where
  toQuery = const mempty

-- | /See:/ 'listBuildBatchesForProjectResponse' smart constructor.
data ListBuildBatchesForProjectResponse = ListBuildBatchesForProjectResponse'
  { _lbbfprsIds ::
      !(Maybe [Text]),
    _lbbfprsNextToken ::
      !(Maybe Text),
    _lbbfprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBuildBatchesForProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbbfprsIds' - An array of strings that contains the batch build identifiers.
--
-- * 'lbbfprsNextToken' - If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatchesForProject@ to retrieve the next set of items.
--
-- * 'lbbfprsResponseStatus' - -- | The response status code.
listBuildBatchesForProjectResponse ::
  -- | 'lbbfprsResponseStatus'
  Int ->
  ListBuildBatchesForProjectResponse
listBuildBatchesForProjectResponse pResponseStatus_ =
  ListBuildBatchesForProjectResponse'
    { _lbbfprsIds = Nothing,
      _lbbfprsNextToken = Nothing,
      _lbbfprsResponseStatus = pResponseStatus_
    }

-- | An array of strings that contains the batch build identifiers.
lbbfprsIds :: Lens' ListBuildBatchesForProjectResponse [Text]
lbbfprsIds = lens _lbbfprsIds (\s a -> s {_lbbfprsIds = a}) . _Default . _Coerce

-- | If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatchesForProject@ to retrieve the next set of items.
lbbfprsNextToken :: Lens' ListBuildBatchesForProjectResponse (Maybe Text)
lbbfprsNextToken = lens _lbbfprsNextToken (\s a -> s {_lbbfprsNextToken = a})

-- | -- | The response status code.
lbbfprsResponseStatus :: Lens' ListBuildBatchesForProjectResponse Int
lbbfprsResponseStatus = lens _lbbfprsResponseStatus (\s a -> s {_lbbfprsResponseStatus = a})

instance NFData ListBuildBatchesForProjectResponse
