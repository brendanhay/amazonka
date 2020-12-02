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
-- Module      : Network.AWS.CodeBuild.ListSharedProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of projects that are shared with other AWS accounts or users.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListSharedProjects
  ( -- * Creating a Request
    listSharedProjects,
    ListSharedProjects,

    -- * Request Lenses
    lspSortOrder,
    lspNextToken,
    lspMaxResults,
    lspSortBy,

    -- * Destructuring the Response
    listSharedProjectsResponse,
    ListSharedProjectsResponse,

    -- * Response Lenses
    lsprsNextToken,
    lsprsProjects,
    lsprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSharedProjects' smart constructor.
data ListSharedProjects = ListSharedProjects'
  { _lspSortOrder ::
      !(Maybe SortOrderType),
    _lspNextToken :: !(Maybe Text),
    _lspMaxResults :: !(Maybe Nat),
    _lspSortBy :: !(Maybe SharedResourceSortByType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSharedProjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lspSortOrder' - The order in which to list shared build projects. Valid values include:     * @ASCENDING@ : List in ascending order.     * @DESCENDING@ : List in descending order.
--
-- * 'lspNextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'lspMaxResults' - The maximum number of paginated shared build projects returned per response. Use @nextToken@ to iterate pages in the list of returned @Project@ objects. The default value is 100.
--
-- * 'lspSortBy' - The criterion to be used to list build projects shared with the current AWS account or user. Valid values include:      * @ARN@ : List based on the ARN.      * @MODIFIED_TIME@ : List based on when information about the shared project was last changed.
listSharedProjects ::
  ListSharedProjects
listSharedProjects =
  ListSharedProjects'
    { _lspSortOrder = Nothing,
      _lspNextToken = Nothing,
      _lspMaxResults = Nothing,
      _lspSortBy = Nothing
    }

-- | The order in which to list shared build projects. Valid values include:     * @ASCENDING@ : List in ascending order.     * @DESCENDING@ : List in descending order.
lspSortOrder :: Lens' ListSharedProjects (Maybe SortOrderType)
lspSortOrder = lens _lspSortOrder (\s a -> s {_lspSortOrder = a})

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
lspNextToken :: Lens' ListSharedProjects (Maybe Text)
lspNextToken = lens _lspNextToken (\s a -> s {_lspNextToken = a})

-- | The maximum number of paginated shared build projects returned per response. Use @nextToken@ to iterate pages in the list of returned @Project@ objects. The default value is 100.
lspMaxResults :: Lens' ListSharedProjects (Maybe Natural)
lspMaxResults = lens _lspMaxResults (\s a -> s {_lspMaxResults = a}) . mapping _Nat

-- | The criterion to be used to list build projects shared with the current AWS account or user. Valid values include:      * @ARN@ : List based on the ARN.      * @MODIFIED_TIME@ : List based on when information about the shared project was last changed.
lspSortBy :: Lens' ListSharedProjects (Maybe SharedResourceSortByType)
lspSortBy = lens _lspSortBy (\s a -> s {_lspSortBy = a})

instance AWSPager ListSharedProjects where
  page rq rs
    | stop (rs ^. lsprsNextToken) = Nothing
    | stop (rs ^. lsprsProjects) = Nothing
    | otherwise = Just $ rq & lspNextToken .~ rs ^. lsprsNextToken

instance AWSRequest ListSharedProjects where
  type Rs ListSharedProjects = ListSharedProjectsResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          ListSharedProjectsResponse'
            <$> (x .?> "nextToken") <*> (x .?> "projects") <*> (pure (fromEnum s))
      )

instance Hashable ListSharedProjects

instance NFData ListSharedProjects

instance ToHeaders ListSharedProjects where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.ListSharedProjects" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListSharedProjects where
  toJSON ListSharedProjects' {..} =
    object
      ( catMaybes
          [ ("sortOrder" .=) <$> _lspSortOrder,
            ("nextToken" .=) <$> _lspNextToken,
            ("maxResults" .=) <$> _lspMaxResults,
            ("sortBy" .=) <$> _lspSortBy
          ]
      )

instance ToPath ListSharedProjects where
  toPath = const "/"

instance ToQuery ListSharedProjects where
  toQuery = const mempty

-- | /See:/ 'listSharedProjectsResponse' smart constructor.
data ListSharedProjectsResponse = ListSharedProjectsResponse'
  { _lsprsNextToken ::
      !(Maybe Text),
    _lsprsProjects ::
      !(Maybe (List1 Text)),
    _lsprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSharedProjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsprsNextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'lsprsProjects' - The list of ARNs for the build projects shared with the current AWS account or user.
--
-- * 'lsprsResponseStatus' - -- | The response status code.
listSharedProjectsResponse ::
  -- | 'lsprsResponseStatus'
  Int ->
  ListSharedProjectsResponse
listSharedProjectsResponse pResponseStatus_ =
  ListSharedProjectsResponse'
    { _lsprsNextToken = Nothing,
      _lsprsProjects = Nothing,
      _lsprsResponseStatus = pResponseStatus_
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
lsprsNextToken :: Lens' ListSharedProjectsResponse (Maybe Text)
lsprsNextToken = lens _lsprsNextToken (\s a -> s {_lsprsNextToken = a})

-- | The list of ARNs for the build projects shared with the current AWS account or user.
lsprsProjects :: Lens' ListSharedProjectsResponse (Maybe (NonEmpty Text))
lsprsProjects = lens _lsprsProjects (\s a -> s {_lsprsProjects = a}) . mapping _List1

-- | -- | The response status code.
lsprsResponseStatus :: Lens' ListSharedProjectsResponse Int
lsprsResponseStatus = lens _lsprsResponseStatus (\s a -> s {_lsprsResponseStatus = a})

instance NFData ListSharedProjectsResponse
