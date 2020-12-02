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
-- Module      : Network.AWS.CloudFormation.ListTypeRegistrations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of registration tokens for the specified type(s).
module Network.AWS.CloudFormation.ListTypeRegistrations
  ( -- * Creating a Request
    listTypeRegistrations,
    ListTypeRegistrations,

    -- * Request Lenses
    ltrTypeName,
    ltrRegistrationStatusFilter,
    ltrNextToken,
    ltrTypeARN,
    ltrType,
    ltrMaxResults,

    -- * Destructuring the Response
    listTypeRegistrationsResponse,
    ListTypeRegistrationsResponse,

    -- * Response Lenses
    ltrrsRegistrationTokenList,
    ltrrsNextToken,
    ltrrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTypeRegistrations' smart constructor.
data ListTypeRegistrations = ListTypeRegistrations'
  { _ltrTypeName ::
      !(Maybe Text),
    _ltrRegistrationStatusFilter ::
      !(Maybe RegistrationStatus),
    _ltrNextToken :: !(Maybe Text),
    _ltrTypeARN :: !(Maybe Text),
    _ltrType :: !(Maybe RegistryType),
    _ltrMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTypeRegistrations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrTypeName' - The name of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'ltrRegistrationStatusFilter' - The current status of the type registration request. The default is @IN_PROGRESS@ .
--
-- * 'ltrNextToken' - If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- * 'ltrTypeARN' - The Amazon Resource Name (ARN) of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'ltrType' - The kind of type. Currently the only valid value is @RESOURCE@ . Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'ltrMaxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
listTypeRegistrations ::
  ListTypeRegistrations
listTypeRegistrations =
  ListTypeRegistrations'
    { _ltrTypeName = Nothing,
      _ltrRegistrationStatusFilter = Nothing,
      _ltrNextToken = Nothing,
      _ltrTypeARN = Nothing,
      _ltrType = Nothing,
      _ltrMaxResults = Nothing
    }

-- | The name of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
ltrTypeName :: Lens' ListTypeRegistrations (Maybe Text)
ltrTypeName = lens _ltrTypeName (\s a -> s {_ltrTypeName = a})

-- | The current status of the type registration request. The default is @IN_PROGRESS@ .
ltrRegistrationStatusFilter :: Lens' ListTypeRegistrations (Maybe RegistrationStatus)
ltrRegistrationStatusFilter = lens _ltrRegistrationStatusFilter (\s a -> s {_ltrRegistrationStatusFilter = a})

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
ltrNextToken :: Lens' ListTypeRegistrations (Maybe Text)
ltrNextToken = lens _ltrNextToken (\s a -> s {_ltrNextToken = a})

-- | The Amazon Resource Name (ARN) of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
ltrTypeARN :: Lens' ListTypeRegistrations (Maybe Text)
ltrTypeARN = lens _ltrTypeARN (\s a -> s {_ltrTypeARN = a})

-- | The kind of type. Currently the only valid value is @RESOURCE@ . Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
ltrType :: Lens' ListTypeRegistrations (Maybe RegistryType)
ltrType = lens _ltrType (\s a -> s {_ltrType = a})

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
ltrMaxResults :: Lens' ListTypeRegistrations (Maybe Natural)
ltrMaxResults = lens _ltrMaxResults (\s a -> s {_ltrMaxResults = a}) . mapping _Nat

instance AWSRequest ListTypeRegistrations where
  type Rs ListTypeRegistrations = ListTypeRegistrationsResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "ListTypeRegistrationsResult"
      ( \s h x ->
          ListTypeRegistrationsResponse'
            <$> ( x .@? "RegistrationTokenList" .!@ mempty
                    >>= may (parseXMLList "member")
                )
            <*> (x .@? "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListTypeRegistrations

instance NFData ListTypeRegistrations

instance ToHeaders ListTypeRegistrations where
  toHeaders = const mempty

instance ToPath ListTypeRegistrations where
  toPath = const "/"

instance ToQuery ListTypeRegistrations where
  toQuery ListTypeRegistrations' {..} =
    mconcat
      [ "Action" =: ("ListTypeRegistrations" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "TypeName" =: _ltrTypeName,
        "RegistrationStatusFilter" =: _ltrRegistrationStatusFilter,
        "NextToken" =: _ltrNextToken,
        "TypeArn" =: _ltrTypeARN,
        "Type" =: _ltrType,
        "MaxResults" =: _ltrMaxResults
      ]

-- | /See:/ 'listTypeRegistrationsResponse' smart constructor.
data ListTypeRegistrationsResponse = ListTypeRegistrationsResponse'
  { _ltrrsRegistrationTokenList ::
      !(Maybe [Text]),
    _ltrrsNextToken ::
      !(Maybe Text),
    _ltrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTypeRegistrationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrrsRegistrationTokenList' - A list of type registration tokens. Use @'DescribeTypeRegistration' @ to return detailed information about a type registration request.
--
-- * 'ltrrsNextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- * 'ltrrsResponseStatus' - -- | The response status code.
listTypeRegistrationsResponse ::
  -- | 'ltrrsResponseStatus'
  Int ->
  ListTypeRegistrationsResponse
listTypeRegistrationsResponse pResponseStatus_ =
  ListTypeRegistrationsResponse'
    { _ltrrsRegistrationTokenList =
        Nothing,
      _ltrrsNextToken = Nothing,
      _ltrrsResponseStatus = pResponseStatus_
    }

-- | A list of type registration tokens. Use @'DescribeTypeRegistration' @ to return detailed information about a type registration request.
ltrrsRegistrationTokenList :: Lens' ListTypeRegistrationsResponse [Text]
ltrrsRegistrationTokenList = lens _ltrrsRegistrationTokenList (\s a -> s {_ltrrsRegistrationTokenList = a}) . _Default . _Coerce

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
ltrrsNextToken :: Lens' ListTypeRegistrationsResponse (Maybe Text)
ltrrsNextToken = lens _ltrrsNextToken (\s a -> s {_ltrrsNextToken = a})

-- | -- | The response status code.
ltrrsResponseStatus :: Lens' ListTypeRegistrationsResponse Int
ltrrsResponseStatus = lens _ltrrsResponseStatus (\s a -> s {_ltrrsResponseStatus = a})

instance NFData ListTypeRegistrationsResponse
