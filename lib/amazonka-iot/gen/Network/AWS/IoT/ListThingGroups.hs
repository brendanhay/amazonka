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
-- Module      : Network.AWS.IoT.ListThingGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the thing groups in your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingGroups
  ( -- * Creating a Request
    listThingGroups,
    ListThingGroups,

    -- * Request Lenses
    ltgNamePrefixFilter,
    ltgParentGroup,
    ltgNextToken,
    ltgRecursive,
    ltgMaxResults,

    -- * Destructuring the Response
    listThingGroupsResponse,
    ListThingGroupsResponse,

    -- * Response Lenses
    ltgrsThingGroups,
    ltgrsNextToken,
    ltgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listThingGroups' smart constructor.
data ListThingGroups = ListThingGroups'
  { _ltgNamePrefixFilter ::
      !(Maybe Text),
    _ltgParentGroup :: !(Maybe Text),
    _ltgNextToken :: !(Maybe Text),
    _ltgRecursive :: !(Maybe Bool),
    _ltgMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListThingGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltgNamePrefixFilter' - A filter that limits the results to those with the specified name prefix.
--
-- * 'ltgParentGroup' - A filter that limits the results to those with the specified parent group.
--
-- * 'ltgNextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- * 'ltgRecursive' - If true, return child groups as well.
--
-- * 'ltgMaxResults' - The maximum number of results to return at one time.
listThingGroups ::
  ListThingGroups
listThingGroups =
  ListThingGroups'
    { _ltgNamePrefixFilter = Nothing,
      _ltgParentGroup = Nothing,
      _ltgNextToken = Nothing,
      _ltgRecursive = Nothing,
      _ltgMaxResults = Nothing
    }

-- | A filter that limits the results to those with the specified name prefix.
ltgNamePrefixFilter :: Lens' ListThingGroups (Maybe Text)
ltgNamePrefixFilter = lens _ltgNamePrefixFilter (\s a -> s {_ltgNamePrefixFilter = a})

-- | A filter that limits the results to those with the specified parent group.
ltgParentGroup :: Lens' ListThingGroups (Maybe Text)
ltgParentGroup = lens _ltgParentGroup (\s a -> s {_ltgParentGroup = a})

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
ltgNextToken :: Lens' ListThingGroups (Maybe Text)
ltgNextToken = lens _ltgNextToken (\s a -> s {_ltgNextToken = a})

-- | If true, return child groups as well.
ltgRecursive :: Lens' ListThingGroups (Maybe Bool)
ltgRecursive = lens _ltgRecursive (\s a -> s {_ltgRecursive = a})

-- | The maximum number of results to return at one time.
ltgMaxResults :: Lens' ListThingGroups (Maybe Natural)
ltgMaxResults = lens _ltgMaxResults (\s a -> s {_ltgMaxResults = a}) . mapping _Nat

instance AWSPager ListThingGroups where
  page rq rs
    | stop (rs ^. ltgrsNextToken) = Nothing
    | stop (rs ^. ltgrsThingGroups) = Nothing
    | otherwise = Just $ rq & ltgNextToken .~ rs ^. ltgrsNextToken

instance AWSRequest ListThingGroups where
  type Rs ListThingGroups = ListThingGroupsResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListThingGroupsResponse'
            <$> (x .?> "thingGroups" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListThingGroups

instance NFData ListThingGroups

instance ToHeaders ListThingGroups where
  toHeaders = const mempty

instance ToPath ListThingGroups where
  toPath = const "/thing-groups"

instance ToQuery ListThingGroups where
  toQuery ListThingGroups' {..} =
    mconcat
      [ "namePrefixFilter" =: _ltgNamePrefixFilter,
        "parentGroup" =: _ltgParentGroup,
        "nextToken" =: _ltgNextToken,
        "recursive" =: _ltgRecursive,
        "maxResults" =: _ltgMaxResults
      ]

-- | /See:/ 'listThingGroupsResponse' smart constructor.
data ListThingGroupsResponse = ListThingGroupsResponse'
  { _ltgrsThingGroups ::
      !(Maybe [GroupNameAndARN]),
    _ltgrsNextToken :: !(Maybe Text),
    _ltgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListThingGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltgrsThingGroups' - The thing groups.
--
-- * 'ltgrsNextToken' - The token to use to get the next set of results. Will not be returned if operation has returned all results.
--
-- * 'ltgrsResponseStatus' - -- | The response status code.
listThingGroupsResponse ::
  -- | 'ltgrsResponseStatus'
  Int ->
  ListThingGroupsResponse
listThingGroupsResponse pResponseStatus_ =
  ListThingGroupsResponse'
    { _ltgrsThingGroups = Nothing,
      _ltgrsNextToken = Nothing,
      _ltgrsResponseStatus = pResponseStatus_
    }

-- | The thing groups.
ltgrsThingGroups :: Lens' ListThingGroupsResponse [GroupNameAndARN]
ltgrsThingGroups = lens _ltgrsThingGroups (\s a -> s {_ltgrsThingGroups = a}) . _Default . _Coerce

-- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
ltgrsNextToken :: Lens' ListThingGroupsResponse (Maybe Text)
ltgrsNextToken = lens _ltgrsNextToken (\s a -> s {_ltgrsNextToken = a})

-- | -- | The response status code.
ltgrsResponseStatus :: Lens' ListThingGroupsResponse Int
ltgrsResponseStatus = lens _ltgrsResponseStatus (\s a -> s {_ltgrsResponseStatus = a})

instance NFData ListThingGroupsResponse
