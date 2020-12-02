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
-- Module      : Network.AWS.Organizations.ListAccounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the accounts in the organization. To request only the accounts in a specified root or organizational unit (OU), use the 'ListAccountsForParent' operation instead.
--
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListAccounts
  ( -- * Creating a Request
    listAccounts,
    ListAccounts,

    -- * Request Lenses
    laNextToken,
    laMaxResults,

    -- * Destructuring the Response
    listAccountsResponse,
    ListAccountsResponse,

    -- * Response Lenses
    larsAccounts,
    larsNextToken,
    larsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAccounts' smart constructor.
data ListAccounts = ListAccounts'
  { _laNextToken :: !(Maybe Text),
    _laMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAccounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'laMaxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
listAccounts ::
  ListAccounts
listAccounts =
  ListAccounts' {_laNextToken = Nothing, _laMaxResults = Nothing}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
laNextToken :: Lens' ListAccounts (Maybe Text)
laNextToken = lens _laNextToken (\s a -> s {_laNextToken = a})

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
laMaxResults :: Lens' ListAccounts (Maybe Natural)
laMaxResults = lens _laMaxResults (\s a -> s {_laMaxResults = a}) . mapping _Nat

instance AWSPager ListAccounts where
  page rq rs
    | stop (rs ^. larsNextToken) = Nothing
    | stop (rs ^. larsAccounts) = Nothing
    | otherwise = Just $ rq & laNextToken .~ rs ^. larsNextToken

instance AWSRequest ListAccounts where
  type Rs ListAccounts = ListAccountsResponse
  request = postJSON organizations
  response =
    receiveJSON
      ( \s h x ->
          ListAccountsResponse'
            <$> (x .?> "Accounts" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListAccounts

instance NFData ListAccounts

instance ToHeaders ListAccounts where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSOrganizationsV20161128.ListAccounts" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListAccounts where
  toJSON ListAccounts' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _laNextToken,
            ("MaxResults" .=) <$> _laMaxResults
          ]
      )

instance ToPath ListAccounts where
  toPath = const "/"

instance ToQuery ListAccounts where
  toQuery = const mempty

-- | /See:/ 'listAccountsResponse' smart constructor.
data ListAccountsResponse = ListAccountsResponse'
  { _larsAccounts ::
      !(Maybe [Account]),
    _larsNextToken :: !(Maybe Text),
    _larsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAccountsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsAccounts' - A list of objects in the organization.
--
-- * 'larsNextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'larsResponseStatus' - -- | The response status code.
listAccountsResponse ::
  -- | 'larsResponseStatus'
  Int ->
  ListAccountsResponse
listAccountsResponse pResponseStatus_ =
  ListAccountsResponse'
    { _larsAccounts = Nothing,
      _larsNextToken = Nothing,
      _larsResponseStatus = pResponseStatus_
    }

-- | A list of objects in the organization.
larsAccounts :: Lens' ListAccountsResponse [Account]
larsAccounts = lens _larsAccounts (\s a -> s {_larsAccounts = a}) . _Default . _Coerce

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
larsNextToken :: Lens' ListAccountsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\s a -> s {_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAccountsResponse Int
larsResponseStatus = lens _larsResponseStatus (\s a -> s {_larsResponseStatus = a})

instance NFData ListAccountsResponse
