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
-- Module      : Network.AWS.IoT.ListBillingGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the billing groups you have created.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListBillingGroups
  ( -- * Creating a Request
    listBillingGroups,
    ListBillingGroups,

    -- * Request Lenses
    lbgNamePrefixFilter,
    lbgNextToken,
    lbgMaxResults,

    -- * Destructuring the Response
    listBillingGroupsResponse,
    ListBillingGroupsResponse,

    -- * Response Lenses
    lbgrsNextToken,
    lbgrsBillingGroups,
    lbgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBillingGroups' smart constructor.
data ListBillingGroups = ListBillingGroups'
  { _lbgNamePrefixFilter ::
      !(Maybe Text),
    _lbgNextToken :: !(Maybe Text),
    _lbgMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBillingGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbgNamePrefixFilter' - Limit the results to billing groups whose names have the given prefix.
--
-- * 'lbgNextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- * 'lbgMaxResults' - The maximum number of results to return per request.
listBillingGroups ::
  ListBillingGroups
listBillingGroups =
  ListBillingGroups'
    { _lbgNamePrefixFilter = Nothing,
      _lbgNextToken = Nothing,
      _lbgMaxResults = Nothing
    }

-- | Limit the results to billing groups whose names have the given prefix.
lbgNamePrefixFilter :: Lens' ListBillingGroups (Maybe Text)
lbgNamePrefixFilter = lens _lbgNamePrefixFilter (\s a -> s {_lbgNamePrefixFilter = a})

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
lbgNextToken :: Lens' ListBillingGroups (Maybe Text)
lbgNextToken = lens _lbgNextToken (\s a -> s {_lbgNextToken = a})

-- | The maximum number of results to return per request.
lbgMaxResults :: Lens' ListBillingGroups (Maybe Natural)
lbgMaxResults = lens _lbgMaxResults (\s a -> s {_lbgMaxResults = a}) . mapping _Nat

instance AWSPager ListBillingGroups where
  page rq rs
    | stop (rs ^. lbgrsNextToken) = Nothing
    | stop (rs ^. lbgrsBillingGroups) = Nothing
    | otherwise = Just $ rq & lbgNextToken .~ rs ^. lbgrsNextToken

instance AWSRequest ListBillingGroups where
  type Rs ListBillingGroups = ListBillingGroupsResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListBillingGroupsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "billingGroups" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListBillingGroups

instance NFData ListBillingGroups

instance ToHeaders ListBillingGroups where
  toHeaders = const mempty

instance ToPath ListBillingGroups where
  toPath = const "/billing-groups"

instance ToQuery ListBillingGroups where
  toQuery ListBillingGroups' {..} =
    mconcat
      [ "namePrefixFilter" =: _lbgNamePrefixFilter,
        "nextToken" =: _lbgNextToken,
        "maxResults" =: _lbgMaxResults
      ]

-- | /See:/ 'listBillingGroupsResponse' smart constructor.
data ListBillingGroupsResponse = ListBillingGroupsResponse'
  { _lbgrsNextToken ::
      !(Maybe Text),
    _lbgrsBillingGroups ::
      !(Maybe [GroupNameAndARN]),
    _lbgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBillingGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbgrsNextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- * 'lbgrsBillingGroups' - The list of billing groups.
--
-- * 'lbgrsResponseStatus' - -- | The response status code.
listBillingGroupsResponse ::
  -- | 'lbgrsResponseStatus'
  Int ->
  ListBillingGroupsResponse
listBillingGroupsResponse pResponseStatus_ =
  ListBillingGroupsResponse'
    { _lbgrsNextToken = Nothing,
      _lbgrsBillingGroups = Nothing,
      _lbgrsResponseStatus = pResponseStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
lbgrsNextToken :: Lens' ListBillingGroupsResponse (Maybe Text)
lbgrsNextToken = lens _lbgrsNextToken (\s a -> s {_lbgrsNextToken = a})

-- | The list of billing groups.
lbgrsBillingGroups :: Lens' ListBillingGroupsResponse [GroupNameAndARN]
lbgrsBillingGroups = lens _lbgrsBillingGroups (\s a -> s {_lbgrsBillingGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
lbgrsResponseStatus :: Lens' ListBillingGroupsResponse Int
lbgrsResponseStatus = lens _lbgrsResponseStatus (\s a -> s {_lbgrsResponseStatus = a})

instance NFData ListBillingGroupsResponse
