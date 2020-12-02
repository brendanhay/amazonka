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
-- Module      : Network.AWS.IoT.ListThingsInBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things you have added to the given billing group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingsInBillingGroup
  ( -- * Creating a Request
    listThingsInBillingGroup,
    ListThingsInBillingGroup,

    -- * Request Lenses
    ltibgNextToken,
    ltibgMaxResults,
    ltibgBillingGroupName,

    -- * Destructuring the Response
    listThingsInBillingGroupResponse,
    ListThingsInBillingGroupResponse,

    -- * Response Lenses
    ltibgrsNextToken,
    ltibgrsThings,
    ltibgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listThingsInBillingGroup' smart constructor.
data ListThingsInBillingGroup = ListThingsInBillingGroup'
  { _ltibgNextToken ::
      !(Maybe Text),
    _ltibgMaxResults :: !(Maybe Nat),
    _ltibgBillingGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListThingsInBillingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltibgNextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- * 'ltibgMaxResults' - The maximum number of results to return per request.
--
-- * 'ltibgBillingGroupName' - The name of the billing group.
listThingsInBillingGroup ::
  -- | 'ltibgBillingGroupName'
  Text ->
  ListThingsInBillingGroup
listThingsInBillingGroup pBillingGroupName_ =
  ListThingsInBillingGroup'
    { _ltibgNextToken = Nothing,
      _ltibgMaxResults = Nothing,
      _ltibgBillingGroupName = pBillingGroupName_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
ltibgNextToken :: Lens' ListThingsInBillingGroup (Maybe Text)
ltibgNextToken = lens _ltibgNextToken (\s a -> s {_ltibgNextToken = a})

-- | The maximum number of results to return per request.
ltibgMaxResults :: Lens' ListThingsInBillingGroup (Maybe Natural)
ltibgMaxResults = lens _ltibgMaxResults (\s a -> s {_ltibgMaxResults = a}) . mapping _Nat

-- | The name of the billing group.
ltibgBillingGroupName :: Lens' ListThingsInBillingGroup Text
ltibgBillingGroupName = lens _ltibgBillingGroupName (\s a -> s {_ltibgBillingGroupName = a})

instance AWSPager ListThingsInBillingGroup where
  page rq rs
    | stop (rs ^. ltibgrsNextToken) = Nothing
    | stop (rs ^. ltibgrsThings) = Nothing
    | otherwise = Just $ rq & ltibgNextToken .~ rs ^. ltibgrsNextToken

instance AWSRequest ListThingsInBillingGroup where
  type Rs ListThingsInBillingGroup = ListThingsInBillingGroupResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListThingsInBillingGroupResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "things" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListThingsInBillingGroup

instance NFData ListThingsInBillingGroup

instance ToHeaders ListThingsInBillingGroup where
  toHeaders = const mempty

instance ToPath ListThingsInBillingGroup where
  toPath ListThingsInBillingGroup' {..} =
    mconcat
      ["/billing-groups/", toBS _ltibgBillingGroupName, "/things"]

instance ToQuery ListThingsInBillingGroup where
  toQuery ListThingsInBillingGroup' {..} =
    mconcat
      ["nextToken" =: _ltibgNextToken, "maxResults" =: _ltibgMaxResults]

-- | /See:/ 'listThingsInBillingGroupResponse' smart constructor.
data ListThingsInBillingGroupResponse = ListThingsInBillingGroupResponse'
  { _ltibgrsNextToken ::
      !(Maybe Text),
    _ltibgrsThings ::
      !(Maybe [Text]),
    _ltibgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListThingsInBillingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltibgrsNextToken' - The token to use to get the next set of results. Will not be returned if operation has returned all results.
--
-- * 'ltibgrsThings' - A list of things in the billing group.
--
-- * 'ltibgrsResponseStatus' - -- | The response status code.
listThingsInBillingGroupResponse ::
  -- | 'ltibgrsResponseStatus'
  Int ->
  ListThingsInBillingGroupResponse
listThingsInBillingGroupResponse pResponseStatus_ =
  ListThingsInBillingGroupResponse'
    { _ltibgrsNextToken = Nothing,
      _ltibgrsThings = Nothing,
      _ltibgrsResponseStatus = pResponseStatus_
    }

-- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
ltibgrsNextToken :: Lens' ListThingsInBillingGroupResponse (Maybe Text)
ltibgrsNextToken = lens _ltibgrsNextToken (\s a -> s {_ltibgrsNextToken = a})

-- | A list of things in the billing group.
ltibgrsThings :: Lens' ListThingsInBillingGroupResponse [Text]
ltibgrsThings = lens _ltibgrsThings (\s a -> s {_ltibgrsThings = a}) . _Default . _Coerce

-- | -- | The response status code.
ltibgrsResponseStatus :: Lens' ListThingsInBillingGroupResponse Int
ltibgrsResponseStatus = lens _ltibgrsResponseStatus (\s a -> s {_ltibgrsResponseStatus = a})

instance NFData ListThingsInBillingGroupResponse
