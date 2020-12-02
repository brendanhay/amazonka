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
-- Module      : Network.AWS.ResourceGroups.ListGroupResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ARNs of the resources that are members of a specified resource group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.ListGroupResources
  ( -- * Creating a Request
    listGroupResources,
    ListGroupResources,

    -- * Request Lenses
    lgrGroup,
    lgrFilters,
    lgrNextToken,
    lgrGroupName,
    lgrMaxResults,

    -- * Destructuring the Response
    listGroupResourcesResponse,
    ListGroupResourcesResponse,

    -- * Response Lenses
    lgrrsQueryErrors,
    lgrrsNextToken,
    lgrrsResourceIdentifiers,
    lgrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'listGroupResources' smart constructor.
data ListGroupResources = ListGroupResources'
  { _lgrGroup ::
      !(Maybe Text),
    _lgrFilters :: !(Maybe [ResourceFilter]),
    _lgrNextToken :: !(Maybe Text),
    _lgrGroupName :: !(Maybe Text),
    _lgrMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGroupResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrGroup' - The name or the ARN of the resource group
--
-- * 'lgrFilters' - Filters, formatted as 'ResourceFilter' objects, that you want to apply to a @ListGroupResources@ operation. Filters the results to include only those of the specified resource types.     * @resource-type@ - Filter resources by their type. Specify up to five resource types in the format @AWS::ServiceCode::ResourceType@ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .  When you specify a @resource-type@ filter for @ListGroupResources@ , AWS Resource Groups validates your filter resource types against the types that are defined in the query associated with the group. For example, if a group contains only S3 buckets because its query specifies only that resource type, but your @resource-type@ filter includes EC2 instances, AWS Resource Groups does not filter for EC2 instances. In this case, a @ListGroupResources@ request returns a @BadRequestException@ error with a message similar to the following: @The resource types specified as filters in the request are not valid.@  The error includes a list of resource types that failed the validation because they are not part of the query associated with the group. This validation doesn't occur when the group query specifies @AWS::AllSupported@ , because a group based on such a query can contain any of the allowed resource types for the query type (tag-based or AWS CloudFormation stack-based queries).
--
-- * 'lgrNextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lgrGroupName' - Don't use this parameter. Use @Group@ instead.
--
-- * 'lgrMaxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
listGroupResources ::
  ListGroupResources
listGroupResources =
  ListGroupResources'
    { _lgrGroup = Nothing,
      _lgrFilters = Nothing,
      _lgrNextToken = Nothing,
      _lgrGroupName = Nothing,
      _lgrMaxResults = Nothing
    }

-- | The name or the ARN of the resource group
lgrGroup :: Lens' ListGroupResources (Maybe Text)
lgrGroup = lens _lgrGroup (\s a -> s {_lgrGroup = a})

-- | Filters, formatted as 'ResourceFilter' objects, that you want to apply to a @ListGroupResources@ operation. Filters the results to include only those of the specified resource types.     * @resource-type@ - Filter resources by their type. Specify up to five resource types in the format @AWS::ServiceCode::ResourceType@ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .  When you specify a @resource-type@ filter for @ListGroupResources@ , AWS Resource Groups validates your filter resource types against the types that are defined in the query associated with the group. For example, if a group contains only S3 buckets because its query specifies only that resource type, but your @resource-type@ filter includes EC2 instances, AWS Resource Groups does not filter for EC2 instances. In this case, a @ListGroupResources@ request returns a @BadRequestException@ error with a message similar to the following: @The resource types specified as filters in the request are not valid.@  The error includes a list of resource types that failed the validation because they are not part of the query associated with the group. This validation doesn't occur when the group query specifies @AWS::AllSupported@ , because a group based on such a query can contain any of the allowed resource types for the query type (tag-based or AWS CloudFormation stack-based queries).
lgrFilters :: Lens' ListGroupResources [ResourceFilter]
lgrFilters = lens _lgrFilters (\s a -> s {_lgrFilters = a}) . _Default . _Coerce

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
lgrNextToken :: Lens' ListGroupResources (Maybe Text)
lgrNextToken = lens _lgrNextToken (\s a -> s {_lgrNextToken = a})

-- | Don't use this parameter. Use @Group@ instead.
lgrGroupName :: Lens' ListGroupResources (Maybe Text)
lgrGroupName = lens _lgrGroupName (\s a -> s {_lgrGroupName = a})

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lgrMaxResults :: Lens' ListGroupResources (Maybe Natural)
lgrMaxResults = lens _lgrMaxResults (\s a -> s {_lgrMaxResults = a}) . mapping _Nat

instance AWSPager ListGroupResources where
  page rq rs
    | stop (rs ^. lgrrsNextToken) = Nothing
    | stop (rs ^. lgrrsResourceIdentifiers) = Nothing
    | otherwise = Just $ rq & lgrNextToken .~ rs ^. lgrrsNextToken

instance AWSRequest ListGroupResources where
  type Rs ListGroupResources = ListGroupResourcesResponse
  request = postJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          ListGroupResourcesResponse'
            <$> (x .?> "QueryErrors" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (x .?> "ResourceIdentifiers" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListGroupResources

instance NFData ListGroupResources

instance ToHeaders ListGroupResources where
  toHeaders = const mempty

instance ToJSON ListGroupResources where
  toJSON ListGroupResources' {..} =
    object
      ( catMaybes
          [ ("Group" .=) <$> _lgrGroup,
            ("Filters" .=) <$> _lgrFilters,
            ("NextToken" .=) <$> _lgrNextToken,
            ("GroupName" .=) <$> _lgrGroupName,
            ("MaxResults" .=) <$> _lgrMaxResults
          ]
      )

instance ToPath ListGroupResources where
  toPath = const "/list-group-resources"

instance ToQuery ListGroupResources where
  toQuery = const mempty

-- | /See:/ 'listGroupResourcesResponse' smart constructor.
data ListGroupResourcesResponse = ListGroupResourcesResponse'
  { _lgrrsQueryErrors ::
      !(Maybe [QueryError]),
    _lgrrsNextToken :: !(Maybe Text),
    _lgrrsResourceIdentifiers ::
      !(Maybe [ResourceIdentifier]),
    _lgrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGroupResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrrsQueryErrors' - A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
--
-- * 'lgrrsNextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'lgrrsResourceIdentifiers' - The ARNs and resource types of resources that are members of the group that you specified.
--
-- * 'lgrrsResponseStatus' - -- | The response status code.
listGroupResourcesResponse ::
  -- | 'lgrrsResponseStatus'
  Int ->
  ListGroupResourcesResponse
listGroupResourcesResponse pResponseStatus_ =
  ListGroupResourcesResponse'
    { _lgrrsQueryErrors = Nothing,
      _lgrrsNextToken = Nothing,
      _lgrrsResourceIdentifiers = Nothing,
      _lgrrsResponseStatus = pResponseStatus_
    }

-- | A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
lgrrsQueryErrors :: Lens' ListGroupResourcesResponse [QueryError]
lgrrsQueryErrors = lens _lgrrsQueryErrors (\s a -> s {_lgrrsQueryErrors = a}) . _Default . _Coerce

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
lgrrsNextToken :: Lens' ListGroupResourcesResponse (Maybe Text)
lgrrsNextToken = lens _lgrrsNextToken (\s a -> s {_lgrrsNextToken = a})

-- | The ARNs and resource types of resources that are members of the group that you specified.
lgrrsResourceIdentifiers :: Lens' ListGroupResourcesResponse [ResourceIdentifier]
lgrrsResourceIdentifiers = lens _lgrrsResourceIdentifiers (\s a -> s {_lgrrsResourceIdentifiers = a}) . _Default . _Coerce

-- | -- | The response status code.
lgrrsResponseStatus :: Lens' ListGroupResourcesResponse Int
lgrrsResponseStatus = lens _lgrrsResponseStatus (\s a -> s {_lgrrsResponseStatus = a})

instance NFData ListGroupResourcesResponse
