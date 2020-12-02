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
-- Module      : Network.AWS.GuardDuty.ListOrganizationAdminAccounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the accounts configured as GuardDuty delegated administrators.
--
--
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListOrganizationAdminAccounts
  ( -- * Creating a Request
    listOrganizationAdminAccounts,
    ListOrganizationAdminAccounts,

    -- * Request Lenses
    loaaNextToken,
    loaaMaxResults,

    -- * Destructuring the Response
    listOrganizationAdminAccountsResponse,
    ListOrganizationAdminAccountsResponse,

    -- * Response Lenses
    loaarsAdminAccounts,
    loaarsNextToken,
    loaarsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listOrganizationAdminAccounts' smart constructor.
data ListOrganizationAdminAccounts = ListOrganizationAdminAccounts'
  { _loaaNextToken ::
      !(Maybe Text),
    _loaaMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListOrganizationAdminAccounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loaaNextToken' - A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
--
-- * 'loaaMaxResults' - The maximum number of results to return in the response.
listOrganizationAdminAccounts ::
  ListOrganizationAdminAccounts
listOrganizationAdminAccounts =
  ListOrganizationAdminAccounts'
    { _loaaNextToken = Nothing,
      _loaaMaxResults = Nothing
    }

-- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
loaaNextToken :: Lens' ListOrganizationAdminAccounts (Maybe Text)
loaaNextToken = lens _loaaNextToken (\s a -> s {_loaaNextToken = a})

-- | The maximum number of results to return in the response.
loaaMaxResults :: Lens' ListOrganizationAdminAccounts (Maybe Natural)
loaaMaxResults = lens _loaaMaxResults (\s a -> s {_loaaMaxResults = a}) . mapping _Nat

instance AWSPager ListOrganizationAdminAccounts where
  page rq rs
    | stop (rs ^. loaarsNextToken) = Nothing
    | stop (rs ^. loaarsAdminAccounts) = Nothing
    | otherwise = Just $ rq & loaaNextToken .~ rs ^. loaarsNextToken

instance AWSRequest ListOrganizationAdminAccounts where
  type
    Rs ListOrganizationAdminAccounts =
      ListOrganizationAdminAccountsResponse
  request = get guardDuty
  response =
    receiveJSON
      ( \s h x ->
          ListOrganizationAdminAccountsResponse'
            <$> (x .?> "adminAccounts" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListOrganizationAdminAccounts

instance NFData ListOrganizationAdminAccounts

instance ToHeaders ListOrganizationAdminAccounts where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListOrganizationAdminAccounts where
  toPath = const "/admin"

instance ToQuery ListOrganizationAdminAccounts where
  toQuery ListOrganizationAdminAccounts' {..} =
    mconcat
      ["nextToken" =: _loaaNextToken, "maxResults" =: _loaaMaxResults]

-- | /See:/ 'listOrganizationAdminAccountsResponse' smart constructor.
data ListOrganizationAdminAccountsResponse = ListOrganizationAdminAccountsResponse'
  { _loaarsAdminAccounts ::
      !( Maybe
           [AdminAccount]
       ),
    _loaarsNextToken ::
      !(Maybe Text),
    _loaarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListOrganizationAdminAccountsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loaarsAdminAccounts' - An AdminAccounts object that includes a list of accounts configured as GuardDuty delegated administrators.
--
-- * 'loaarsNextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
--
-- * 'loaarsResponseStatus' - -- | The response status code.
listOrganizationAdminAccountsResponse ::
  -- | 'loaarsResponseStatus'
  Int ->
  ListOrganizationAdminAccountsResponse
listOrganizationAdminAccountsResponse pResponseStatus_ =
  ListOrganizationAdminAccountsResponse'
    { _loaarsAdminAccounts =
        Nothing,
      _loaarsNextToken = Nothing,
      _loaarsResponseStatus = pResponseStatus_
    }

-- | An AdminAccounts object that includes a list of accounts configured as GuardDuty delegated administrators.
loaarsAdminAccounts :: Lens' ListOrganizationAdminAccountsResponse [AdminAccount]
loaarsAdminAccounts = lens _loaarsAdminAccounts (\s a -> s {_loaarsAdminAccounts = a}) . _Default . _Coerce

-- | The pagination parameter to be used on the next list operation to retrieve more items.
loaarsNextToken :: Lens' ListOrganizationAdminAccountsResponse (Maybe Text)
loaarsNextToken = lens _loaarsNextToken (\s a -> s {_loaarsNextToken = a})

-- | -- | The response status code.
loaarsResponseStatus :: Lens' ListOrganizationAdminAccountsResponse Int
loaarsResponseStatus = lens _loaarsResponseStatus (\s a -> s {_loaarsResponseStatus = a})

instance NFData ListOrganizationAdminAccountsResponse
