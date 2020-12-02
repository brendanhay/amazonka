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
-- Module      : Network.AWS.Organizations.ListDelegatedAdministrators
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS accounts that are designated as delegated administrators in this organization.
--
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListDelegatedAdministrators
  ( -- * Creating a Request
    listDelegatedAdministrators,
    ListDelegatedAdministrators,

    -- * Request Lenses
    ldaServicePrincipal,
    ldaNextToken,
    ldaMaxResults,

    -- * Destructuring the Response
    listDelegatedAdministratorsResponse,
    ListDelegatedAdministratorsResponse,

    -- * Response Lenses
    ldarsDelegatedAdministrators,
    ldarsNextToken,
    ldarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDelegatedAdministrators' smart constructor.
data ListDelegatedAdministrators = ListDelegatedAdministrators'
  { _ldaServicePrincipal ::
      !(Maybe Text),
    _ldaNextToken :: !(Maybe Text),
    _ldaMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDelegatedAdministrators' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldaServicePrincipal' - Specifies a service principal name. If specified, then the operation lists the delegated administrators only for the specified service. If you don't specify a service principal, the operation lists all delegated administrators for all services in your organization.
--
-- * 'ldaNextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'ldaMaxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
listDelegatedAdministrators ::
  ListDelegatedAdministrators
listDelegatedAdministrators =
  ListDelegatedAdministrators'
    { _ldaServicePrincipal = Nothing,
      _ldaNextToken = Nothing,
      _ldaMaxResults = Nothing
    }

-- | Specifies a service principal name. If specified, then the operation lists the delegated administrators only for the specified service. If you don't specify a service principal, the operation lists all delegated administrators for all services in your organization.
ldaServicePrincipal :: Lens' ListDelegatedAdministrators (Maybe Text)
ldaServicePrincipal = lens _ldaServicePrincipal (\s a -> s {_ldaServicePrincipal = a})

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
ldaNextToken :: Lens' ListDelegatedAdministrators (Maybe Text)
ldaNextToken = lens _ldaNextToken (\s a -> s {_ldaNextToken = a})

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
ldaMaxResults :: Lens' ListDelegatedAdministrators (Maybe Natural)
ldaMaxResults = lens _ldaMaxResults (\s a -> s {_ldaMaxResults = a}) . mapping _Nat

instance AWSPager ListDelegatedAdministrators where
  page rq rs
    | stop (rs ^. ldarsNextToken) = Nothing
    | stop (rs ^. ldarsDelegatedAdministrators) = Nothing
    | otherwise = Just $ rq & ldaNextToken .~ rs ^. ldarsNextToken

instance AWSRequest ListDelegatedAdministrators where
  type
    Rs ListDelegatedAdministrators =
      ListDelegatedAdministratorsResponse
  request = postJSON organizations
  response =
    receiveJSON
      ( \s h x ->
          ListDelegatedAdministratorsResponse'
            <$> (x .?> "DelegatedAdministrators" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListDelegatedAdministrators

instance NFData ListDelegatedAdministrators

instance ToHeaders ListDelegatedAdministrators where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSOrganizationsV20161128.ListDelegatedAdministrators" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListDelegatedAdministrators where
  toJSON ListDelegatedAdministrators' {..} =
    object
      ( catMaybes
          [ ("ServicePrincipal" .=) <$> _ldaServicePrincipal,
            ("NextToken" .=) <$> _ldaNextToken,
            ("MaxResults" .=) <$> _ldaMaxResults
          ]
      )

instance ToPath ListDelegatedAdministrators where
  toPath = const "/"

instance ToQuery ListDelegatedAdministrators where
  toQuery = const mempty

-- | /See:/ 'listDelegatedAdministratorsResponse' smart constructor.
data ListDelegatedAdministratorsResponse = ListDelegatedAdministratorsResponse'
  { _ldarsDelegatedAdministrators ::
      !( Maybe
           [DelegatedAdministrator]
       ),
    _ldarsNextToken ::
      !(Maybe Text),
    _ldarsResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDelegatedAdministratorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldarsDelegatedAdministrators' - The list of delegated administrators in your organization.
--
-- * 'ldarsNextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'ldarsResponseStatus' - -- | The response status code.
listDelegatedAdministratorsResponse ::
  -- | 'ldarsResponseStatus'
  Int ->
  ListDelegatedAdministratorsResponse
listDelegatedAdministratorsResponse pResponseStatus_ =
  ListDelegatedAdministratorsResponse'
    { _ldarsDelegatedAdministrators =
        Nothing,
      _ldarsNextToken = Nothing,
      _ldarsResponseStatus = pResponseStatus_
    }

-- | The list of delegated administrators in your organization.
ldarsDelegatedAdministrators :: Lens' ListDelegatedAdministratorsResponse [DelegatedAdministrator]
ldarsDelegatedAdministrators = lens _ldarsDelegatedAdministrators (\s a -> s {_ldarsDelegatedAdministrators = a}) . _Default . _Coerce

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
ldarsNextToken :: Lens' ListDelegatedAdministratorsResponse (Maybe Text)
ldarsNextToken = lens _ldarsNextToken (\s a -> s {_ldarsNextToken = a})

-- | -- | The response status code.
ldarsResponseStatus :: Lens' ListDelegatedAdministratorsResponse Int
ldarsResponseStatus = lens _ldarsResponseStatus (\s a -> s {_ldarsResponseStatus = a})

instance NFData ListDelegatedAdministratorsResponse
