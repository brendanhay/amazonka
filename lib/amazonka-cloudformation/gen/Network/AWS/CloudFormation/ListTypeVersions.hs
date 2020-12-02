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
-- Module      : Network.AWS.CloudFormation.ListTypeVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about the versions of a type.
module Network.AWS.CloudFormation.ListTypeVersions
  ( -- * Creating a Request
    listTypeVersions,
    ListTypeVersions,

    -- * Request Lenses
    ltvTypeName,
    ltvARN,
    ltvNextToken,
    ltvDeprecatedStatus,
    ltvType,
    ltvMaxResults,

    -- * Destructuring the Response
    listTypeVersionsResponse,
    ListTypeVersionsResponse,

    -- * Response Lenses
    ltvrsNextToken,
    ltvrsTypeVersionSummaries,
    ltvrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTypeVersions' smart constructor.
data ListTypeVersions = ListTypeVersions'
  { _ltvTypeName ::
      !(Maybe Text),
    _ltvARN :: !(Maybe Text),
    _ltvNextToken :: !(Maybe Text),
    _ltvDeprecatedStatus :: !(Maybe DeprecatedStatus),
    _ltvType :: !(Maybe RegistryType),
    _ltvMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTypeVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltvTypeName' - The name of the type for which you want version summary information. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'ltvARN' - The Amazon Resource Name (ARN) of the type for which you want version summary information. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'ltvNextToken' - If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- * 'ltvDeprecatedStatus' - The deprecation status of the type versions that you want to get summary information about. Valid values include:     * @LIVE@ : The type version is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.     * @DEPRECATED@ : The type version has been deregistered and can no longer be used in CloudFormation operations.  The default is @LIVE@ .
--
-- * 'ltvType' - The kind of the type. Currently the only valid value is @RESOURCE@ . Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'ltvMaxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
listTypeVersions ::
  ListTypeVersions
listTypeVersions =
  ListTypeVersions'
    { _ltvTypeName = Nothing,
      _ltvARN = Nothing,
      _ltvNextToken = Nothing,
      _ltvDeprecatedStatus = Nothing,
      _ltvType = Nothing,
      _ltvMaxResults = Nothing
    }

-- | The name of the type for which you want version summary information. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
ltvTypeName :: Lens' ListTypeVersions (Maybe Text)
ltvTypeName = lens _ltvTypeName (\s a -> s {_ltvTypeName = a})

-- | The Amazon Resource Name (ARN) of the type for which you want version summary information. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
ltvARN :: Lens' ListTypeVersions (Maybe Text)
ltvARN = lens _ltvARN (\s a -> s {_ltvARN = a})

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
ltvNextToken :: Lens' ListTypeVersions (Maybe Text)
ltvNextToken = lens _ltvNextToken (\s a -> s {_ltvNextToken = a})

-- | The deprecation status of the type versions that you want to get summary information about. Valid values include:     * @LIVE@ : The type version is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.     * @DEPRECATED@ : The type version has been deregistered and can no longer be used in CloudFormation operations.  The default is @LIVE@ .
ltvDeprecatedStatus :: Lens' ListTypeVersions (Maybe DeprecatedStatus)
ltvDeprecatedStatus = lens _ltvDeprecatedStatus (\s a -> s {_ltvDeprecatedStatus = a})

-- | The kind of the type. Currently the only valid value is @RESOURCE@ . Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
ltvType :: Lens' ListTypeVersions (Maybe RegistryType)
ltvType = lens _ltvType (\s a -> s {_ltvType = a})

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
ltvMaxResults :: Lens' ListTypeVersions (Maybe Natural)
ltvMaxResults = lens _ltvMaxResults (\s a -> s {_ltvMaxResults = a}) . mapping _Nat

instance AWSRequest ListTypeVersions where
  type Rs ListTypeVersions = ListTypeVersionsResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "ListTypeVersionsResult"
      ( \s h x ->
          ListTypeVersionsResponse'
            <$> (x .@? "NextToken")
            <*> ( x .@? "TypeVersionSummaries" .!@ mempty
                    >>= may (parseXMLList "member")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable ListTypeVersions

instance NFData ListTypeVersions

instance ToHeaders ListTypeVersions where
  toHeaders = const mempty

instance ToPath ListTypeVersions where
  toPath = const "/"

instance ToQuery ListTypeVersions where
  toQuery ListTypeVersions' {..} =
    mconcat
      [ "Action" =: ("ListTypeVersions" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "TypeName" =: _ltvTypeName,
        "Arn" =: _ltvARN,
        "NextToken" =: _ltvNextToken,
        "DeprecatedStatus" =: _ltvDeprecatedStatus,
        "Type" =: _ltvType,
        "MaxResults" =: _ltvMaxResults
      ]

-- | /See:/ 'listTypeVersionsResponse' smart constructor.
data ListTypeVersionsResponse = ListTypeVersionsResponse'
  { _ltvrsNextToken ::
      !(Maybe Text),
    _ltvrsTypeVersionSummaries ::
      !(Maybe [TypeVersionSummary]),
    _ltvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTypeVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltvrsNextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- * 'ltvrsTypeVersionSummaries' - A list of @TypeVersionSummary@ structures that contain information about the specified type's versions.
--
-- * 'ltvrsResponseStatus' - -- | The response status code.
listTypeVersionsResponse ::
  -- | 'ltvrsResponseStatus'
  Int ->
  ListTypeVersionsResponse
listTypeVersionsResponse pResponseStatus_ =
  ListTypeVersionsResponse'
    { _ltvrsNextToken = Nothing,
      _ltvrsTypeVersionSummaries = Nothing,
      _ltvrsResponseStatus = pResponseStatus_
    }

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call this action again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
ltvrsNextToken :: Lens' ListTypeVersionsResponse (Maybe Text)
ltvrsNextToken = lens _ltvrsNextToken (\s a -> s {_ltvrsNextToken = a})

-- | A list of @TypeVersionSummary@ structures that contain information about the specified type's versions.
ltvrsTypeVersionSummaries :: Lens' ListTypeVersionsResponse [TypeVersionSummary]
ltvrsTypeVersionSummaries = lens _ltvrsTypeVersionSummaries (\s a -> s {_ltvrsTypeVersionSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
ltvrsResponseStatus :: Lens' ListTypeVersionsResponse Int
ltvrsResponseStatus = lens _ltvrsResponseStatus (\s a -> s {_ltvrsResponseStatus = a})

instance NFData ListTypeVersionsResponse
