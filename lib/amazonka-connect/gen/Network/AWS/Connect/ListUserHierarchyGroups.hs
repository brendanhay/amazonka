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
-- Module      : Network.AWS.Connect.ListUserHierarchyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the hierarchy groups for the specified Amazon Connect instance.
--
--
-- For more information about agent hierarchies, see <https://docs.aws.amazon.com/connect/latest/adminguide/agent-hierarchy.html Set Up Agent Hierarchies> in the /Amazon Connect Administrator Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUserHierarchyGroups
  ( -- * Creating a Request
    listUserHierarchyGroups,
    ListUserHierarchyGroups,

    -- * Request Lenses
    luhgNextToken,
    luhgMaxResults,
    luhgInstanceId,

    -- * Destructuring the Response
    listUserHierarchyGroupsResponse,
    ListUserHierarchyGroupsResponse,

    -- * Response Lenses
    luhgrsNextToken,
    luhgrsUserHierarchyGroupSummaryList,
    luhgrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listUserHierarchyGroups' smart constructor.
data ListUserHierarchyGroups = ListUserHierarchyGroups'
  { _luhgNextToken ::
      !(Maybe Text),
    _luhgMaxResults :: !(Maybe Nat),
    _luhgInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListUserHierarchyGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luhgNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'luhgMaxResults' - The maximimum number of results to return per page.
--
-- * 'luhgInstanceId' - The identifier of the Amazon Connect instance.
listUserHierarchyGroups ::
  -- | 'luhgInstanceId'
  Text ->
  ListUserHierarchyGroups
listUserHierarchyGroups pInstanceId_ =
  ListUserHierarchyGroups'
    { _luhgNextToken = Nothing,
      _luhgMaxResults = Nothing,
      _luhgInstanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
luhgNextToken :: Lens' ListUserHierarchyGroups (Maybe Text)
luhgNextToken = lens _luhgNextToken (\s a -> s {_luhgNextToken = a})

-- | The maximimum number of results to return per page.
luhgMaxResults :: Lens' ListUserHierarchyGroups (Maybe Natural)
luhgMaxResults = lens _luhgMaxResults (\s a -> s {_luhgMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
luhgInstanceId :: Lens' ListUserHierarchyGroups Text
luhgInstanceId = lens _luhgInstanceId (\s a -> s {_luhgInstanceId = a})

instance AWSPager ListUserHierarchyGroups where
  page rq rs
    | stop (rs ^. luhgrsNextToken) = Nothing
    | stop (rs ^. luhgrsUserHierarchyGroupSummaryList) = Nothing
    | otherwise = Just $ rq & luhgNextToken .~ rs ^. luhgrsNextToken

instance AWSRequest ListUserHierarchyGroups where
  type Rs ListUserHierarchyGroups = ListUserHierarchyGroupsResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListUserHierarchyGroupsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "UserHierarchyGroupSummaryList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListUserHierarchyGroups

instance NFData ListUserHierarchyGroups

instance ToHeaders ListUserHierarchyGroups where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListUserHierarchyGroups where
  toPath ListUserHierarchyGroups' {..} =
    mconcat ["/user-hierarchy-groups-summary/", toBS _luhgInstanceId]

instance ToQuery ListUserHierarchyGroups where
  toQuery ListUserHierarchyGroups' {..} =
    mconcat
      ["nextToken" =: _luhgNextToken, "maxResults" =: _luhgMaxResults]

-- | /See:/ 'listUserHierarchyGroupsResponse' smart constructor.
data ListUserHierarchyGroupsResponse = ListUserHierarchyGroupsResponse'
  { _luhgrsNextToken ::
      !(Maybe Text),
    _luhgrsUserHierarchyGroupSummaryList ::
      !( Maybe
           [HierarchyGroupSummary]
       ),
    _luhgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListUserHierarchyGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luhgrsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'luhgrsUserHierarchyGroupSummaryList' - Information about the hierarchy groups.
--
-- * 'luhgrsResponseStatus' - -- | The response status code.
listUserHierarchyGroupsResponse ::
  -- | 'luhgrsResponseStatus'
  Int ->
  ListUserHierarchyGroupsResponse
listUserHierarchyGroupsResponse pResponseStatus_ =
  ListUserHierarchyGroupsResponse'
    { _luhgrsNextToken = Nothing,
      _luhgrsUserHierarchyGroupSummaryList = Nothing,
      _luhgrsResponseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
luhgrsNextToken :: Lens' ListUserHierarchyGroupsResponse (Maybe Text)
luhgrsNextToken = lens _luhgrsNextToken (\s a -> s {_luhgrsNextToken = a})

-- | Information about the hierarchy groups.
luhgrsUserHierarchyGroupSummaryList :: Lens' ListUserHierarchyGroupsResponse [HierarchyGroupSummary]
luhgrsUserHierarchyGroupSummaryList = lens _luhgrsUserHierarchyGroupSummaryList (\s a -> s {_luhgrsUserHierarchyGroupSummaryList = a}) . _Default . _Coerce

-- | -- | The response status code.
luhgrsResponseStatus :: Lens' ListUserHierarchyGroupsResponse Int
luhgrsResponseStatus = lens _luhgrsResponseStatus (\s a -> s {_luhgrsResponseStatus = a})

instance NFData ListUserHierarchyGroupsResponse
