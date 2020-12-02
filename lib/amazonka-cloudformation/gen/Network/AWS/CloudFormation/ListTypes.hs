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
-- Module      : Network.AWS.CloudFormation.ListTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about types that have been registered with CloudFormation.
module Network.AWS.CloudFormation.ListTypes
  ( -- * Creating a Request
    listTypes,
    ListTypes,

    -- * Request Lenses
    ltVisibility,
    ltNextToken,
    ltDeprecatedStatus,
    ltType,
    ltMaxResults,
    ltProvisioningType,

    -- * Destructuring the Response
    listTypesResponse,
    ListTypesResponse,

    -- * Response Lenses
    ltrsTypeSummaries,
    ltrsNextToken,
    ltrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTypes' smart constructor.
data ListTypes = ListTypes'
  { _ltVisibility :: !(Maybe Visibility),
    _ltNextToken :: !(Maybe Text),
    _ltDeprecatedStatus :: !(Maybe DeprecatedStatus),
    _ltType :: !(Maybe RegistryType),
    _ltMaxResults :: !(Maybe Nat),
    _ltProvisioningType :: !(Maybe ProvisioningType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltVisibility' - The scope at which the type is visible and usable in CloudFormation operations. Valid values include:     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you create as @PRIVATE@ .     * @PUBLIC@ : The type is publically visible and usable within any Amazon account. The default is @PRIVATE@ .
--
-- * 'ltNextToken' - If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- * 'ltDeprecatedStatus' - The deprecation status of the types that you want to get summary information about. Valid values include:     * @LIVE@ : The type is registered for use in CloudFormation operations.     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
--
-- * 'ltType' - The type of extension.
--
-- * 'ltMaxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- * 'ltProvisioningType' - The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted. Valid values include:     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.     * @NON_PROVISIONABLE@ : The type does not include create, read, and delete handlers, and therefore cannot actually be provisioned.
listTypes ::
  ListTypes
listTypes =
  ListTypes'
    { _ltVisibility = Nothing,
      _ltNextToken = Nothing,
      _ltDeprecatedStatus = Nothing,
      _ltType = Nothing,
      _ltMaxResults = Nothing,
      _ltProvisioningType = Nothing
    }

-- | The scope at which the type is visible and usable in CloudFormation operations. Valid values include:     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you create as @PRIVATE@ .     * @PUBLIC@ : The type is publically visible and usable within any Amazon account. The default is @PRIVATE@ .
ltVisibility :: Lens' ListTypes (Maybe Visibility)
ltVisibility = lens _ltVisibility (\s a -> s {_ltVisibility = a})

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
ltNextToken :: Lens' ListTypes (Maybe Text)
ltNextToken = lens _ltNextToken (\s a -> s {_ltNextToken = a})

-- | The deprecation status of the types that you want to get summary information about. Valid values include:     * @LIVE@ : The type is registered for use in CloudFormation operations.     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
ltDeprecatedStatus :: Lens' ListTypes (Maybe DeprecatedStatus)
ltDeprecatedStatus = lens _ltDeprecatedStatus (\s a -> s {_ltDeprecatedStatus = a})

-- | The type of extension.
ltType :: Lens' ListTypes (Maybe RegistryType)
ltType = lens _ltType (\s a -> s {_ltType = a})

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
ltMaxResults :: Lens' ListTypes (Maybe Natural)
ltMaxResults = lens _ltMaxResults (\s a -> s {_ltMaxResults = a}) . mapping _Nat

-- | The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted. Valid values include:     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.     * @NON_PROVISIONABLE@ : The type does not include create, read, and delete handlers, and therefore cannot actually be provisioned.
ltProvisioningType :: Lens' ListTypes (Maybe ProvisioningType)
ltProvisioningType = lens _ltProvisioningType (\s a -> s {_ltProvisioningType = a})

instance AWSRequest ListTypes where
  type Rs ListTypes = ListTypesResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "ListTypesResult"
      ( \s h x ->
          ListTypesResponse'
            <$> (x .@? "TypeSummaries" .!@ mempty >>= may (parseXMLList "member"))
            <*> (x .@? "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListTypes

instance NFData ListTypes

instance ToHeaders ListTypes where
  toHeaders = const mempty

instance ToPath ListTypes where
  toPath = const "/"

instance ToQuery ListTypes where
  toQuery ListTypes' {..} =
    mconcat
      [ "Action" =: ("ListTypes" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "Visibility" =: _ltVisibility,
        "NextToken" =: _ltNextToken,
        "DeprecatedStatus" =: _ltDeprecatedStatus,
        "Type" =: _ltType,
        "MaxResults" =: _ltMaxResults,
        "ProvisioningType" =: _ltProvisioningType
      ]

-- | /See:/ 'listTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { _ltrsTypeSummaries ::
      !(Maybe [TypeSummary]),
    _ltrsNextToken :: !(Maybe Text),
    _ltrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsTypeSummaries' - A list of @TypeSummary@ structures that contain information about the specified types.
--
-- * 'ltrsNextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTypesResponse ::
  -- | 'ltrsResponseStatus'
  Int ->
  ListTypesResponse
listTypesResponse pResponseStatus_ =
  ListTypesResponse'
    { _ltrsTypeSummaries = Nothing,
      _ltrsNextToken = Nothing,
      _ltrsResponseStatus = pResponseStatus_
    }

-- | A list of @TypeSummary@ structures that contain information about the specified types.
ltrsTypeSummaries :: Lens' ListTypesResponse [TypeSummary]
ltrsTypeSummaries = lens _ltrsTypeSummaries (\s a -> s {_ltrsTypeSummaries = a}) . _Default . _Coerce

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
ltrsNextToken :: Lens' ListTypesResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\s a -> s {_ltrsNextToken = a})

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTypesResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\s a -> s {_ltrsResponseStatus = a})

instance NFData ListTypesResponse
