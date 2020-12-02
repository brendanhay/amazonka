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
-- Module      : Network.AWS.Organizations.ListDelegatedServicesForAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the AWS services for which the specified account is a delegated administrator.
--
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListDelegatedServicesForAccount
  ( -- * Creating a Request
    listDelegatedServicesForAccount,
    ListDelegatedServicesForAccount,

    -- * Request Lenses
    ldsfaNextToken,
    ldsfaMaxResults,
    ldsfaAccountId,

    -- * Destructuring the Response
    listDelegatedServicesForAccountResponse,
    ListDelegatedServicesForAccountResponse,

    -- * Response Lenses
    ldsfarsDelegatedServices,
    ldsfarsNextToken,
    ldsfarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDelegatedServicesForAccount' smart constructor.
data ListDelegatedServicesForAccount = ListDelegatedServicesForAccount'
  { _ldsfaNextToken ::
      !(Maybe Text),
    _ldsfaMaxResults ::
      !(Maybe Nat),
    _ldsfaAccountId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDelegatedServicesForAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldsfaNextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'ldsfaMaxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- * 'ldsfaAccountId' - The account ID number of a delegated administrator account in the organization.
listDelegatedServicesForAccount ::
  -- | 'ldsfaAccountId'
  Text ->
  ListDelegatedServicesForAccount
listDelegatedServicesForAccount pAccountId_ =
  ListDelegatedServicesForAccount'
    { _ldsfaNextToken = Nothing,
      _ldsfaMaxResults = Nothing,
      _ldsfaAccountId = pAccountId_
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
ldsfaNextToken :: Lens' ListDelegatedServicesForAccount (Maybe Text)
ldsfaNextToken = lens _ldsfaNextToken (\s a -> s {_ldsfaNextToken = a})

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
ldsfaMaxResults :: Lens' ListDelegatedServicesForAccount (Maybe Natural)
ldsfaMaxResults = lens _ldsfaMaxResults (\s a -> s {_ldsfaMaxResults = a}) . mapping _Nat

-- | The account ID number of a delegated administrator account in the organization.
ldsfaAccountId :: Lens' ListDelegatedServicesForAccount Text
ldsfaAccountId = lens _ldsfaAccountId (\s a -> s {_ldsfaAccountId = a})

instance AWSPager ListDelegatedServicesForAccount where
  page rq rs
    | stop (rs ^. ldsfarsNextToken) = Nothing
    | stop (rs ^. ldsfarsDelegatedServices) = Nothing
    | otherwise = Just $ rq & ldsfaNextToken .~ rs ^. ldsfarsNextToken

instance AWSRequest ListDelegatedServicesForAccount where
  type
    Rs ListDelegatedServicesForAccount =
      ListDelegatedServicesForAccountResponse
  request = postJSON organizations
  response =
    receiveJSON
      ( \s h x ->
          ListDelegatedServicesForAccountResponse'
            <$> (x .?> "DelegatedServices" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListDelegatedServicesForAccount

instance NFData ListDelegatedServicesForAccount

instance ToHeaders ListDelegatedServicesForAccount where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSOrganizationsV20161128.ListDelegatedServicesForAccount" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListDelegatedServicesForAccount where
  toJSON ListDelegatedServicesForAccount' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _ldsfaNextToken,
            ("MaxResults" .=) <$> _ldsfaMaxResults,
            Just ("AccountId" .= _ldsfaAccountId)
          ]
      )

instance ToPath ListDelegatedServicesForAccount where
  toPath = const "/"

instance ToQuery ListDelegatedServicesForAccount where
  toQuery = const mempty

-- | /See:/ 'listDelegatedServicesForAccountResponse' smart constructor.
data ListDelegatedServicesForAccountResponse = ListDelegatedServicesForAccountResponse'
  { _ldsfarsDelegatedServices ::
      !( Maybe
           [DelegatedService]
       ),
    _ldsfarsNextToken ::
      !( Maybe
           Text
       ),
    _ldsfarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDelegatedServicesForAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldsfarsDelegatedServices' - The services for which the account is a delegated administrator.
--
-- * 'ldsfarsNextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'ldsfarsResponseStatus' - -- | The response status code.
listDelegatedServicesForAccountResponse ::
  -- | 'ldsfarsResponseStatus'
  Int ->
  ListDelegatedServicesForAccountResponse
listDelegatedServicesForAccountResponse pResponseStatus_ =
  ListDelegatedServicesForAccountResponse'
    { _ldsfarsDelegatedServices =
        Nothing,
      _ldsfarsNextToken = Nothing,
      _ldsfarsResponseStatus = pResponseStatus_
    }

-- | The services for which the account is a delegated administrator.
ldsfarsDelegatedServices :: Lens' ListDelegatedServicesForAccountResponse [DelegatedService]
ldsfarsDelegatedServices = lens _ldsfarsDelegatedServices (\s a -> s {_ldsfarsDelegatedServices = a}) . _Default . _Coerce

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
ldsfarsNextToken :: Lens' ListDelegatedServicesForAccountResponse (Maybe Text)
ldsfarsNextToken = lens _ldsfarsNextToken (\s a -> s {_ldsfarsNextToken = a})

-- | -- | The response status code.
ldsfarsResponseStatus :: Lens' ListDelegatedServicesForAccountResponse Int
ldsfarsResponseStatus = lens _ldsfarsResponseStatus (\s a -> s {_ldsfarsResponseStatus = a})

instance NFData ListDelegatedServicesForAccountResponse
