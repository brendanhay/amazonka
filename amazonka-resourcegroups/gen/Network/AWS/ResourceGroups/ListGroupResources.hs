{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.ListGroupResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ARNs of the resources that are members of a specified
-- resource group.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:ListGroupResources@
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.ListGroupResources
  ( -- * Creating a Request
    ListGroupResources (..),
    newListGroupResources,

    -- * Request Lenses
    listGroupResources_nextToken,
    listGroupResources_maxResults,
    listGroupResources_groupName,
    listGroupResources_group,
    listGroupResources_filters,

    -- * Destructuring the Response
    ListGroupResourcesResponse (..),
    newListGroupResourcesResponse,

    -- * Response Lenses
    listGroupResourcesResponse_nextToken,
    listGroupResourcesResponse_resourceIdentifiers,
    listGroupResourcesResponse_resources,
    listGroupResourcesResponse_queryErrors,
    listGroupResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGroupResources' smart constructor.
data ListGroupResources = ListGroupResources'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- provided by a previous call\'s @NextToken@ response to indicate where
    -- the output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that the service might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | /__Deprecated - don\'t use this parameter. Use the @Group@ request field
    -- instead.__/
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The name or the ARN of the resource group
    group' :: Prelude.Maybe Prelude.Text,
    -- | Filters, formatted as ResourceFilter objects, that you want to apply to
    -- a @ListGroupResources@ operation. Filters the results to include only
    -- those of the specified resource types.
    --
    -- -   @resource-type@ - Filter resources by their type. Specify up to five
    --     resource types in the format @AWS::ServiceCode::ResourceType@. For
    --     example, @AWS::EC2::Instance@, or @AWS::S3::Bucket@.
    --
    -- When you specify a @resource-type@ filter for @ListGroupResources@, AWS
    -- Resource Groups validates your filter resource types against the types
    -- that are defined in the query associated with the group. For example, if
    -- a group contains only S3 buckets because its query specifies only that
    -- resource type, but your @resource-type@ filter includes EC2 instances,
    -- AWS Resource Groups does not filter for EC2 instances. In this case, a
    -- @ListGroupResources@ request returns a @BadRequestException@ error with
    -- a message similar to the following:
    --
    -- @The resource types specified as filters in the request are not valid.@
    --
    -- The error includes a list of resource types that failed the validation
    -- because they are not part of the query associated with the group. This
    -- validation doesn\'t occur when the group query specifies
    -- @AWS::AllSupported@, because a group based on such a query can contain
    -- any of the allowed resource types for the query type (tag-based or AWS
    -- CloudFormation stack-based queries).
    filters :: Prelude.Maybe [ResourceFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListGroupResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGroupResources_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- provided by a previous call\'s @NextToken@ response to indicate where
-- the output should continue from.
--
-- 'maxResults', 'listGroupResources_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that the service might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
--
-- 'groupName', 'listGroupResources_groupName' - /__Deprecated - don\'t use this parameter. Use the @Group@ request field
-- instead.__/
--
-- 'group'', 'listGroupResources_group' - The name or the ARN of the resource group
--
-- 'filters', 'listGroupResources_filters' - Filters, formatted as ResourceFilter objects, that you want to apply to
-- a @ListGroupResources@ operation. Filters the results to include only
-- those of the specified resource types.
--
-- -   @resource-type@ - Filter resources by their type. Specify up to five
--     resource types in the format @AWS::ServiceCode::ResourceType@. For
--     example, @AWS::EC2::Instance@, or @AWS::S3::Bucket@.
--
-- When you specify a @resource-type@ filter for @ListGroupResources@, AWS
-- Resource Groups validates your filter resource types against the types
-- that are defined in the query associated with the group. For example, if
-- a group contains only S3 buckets because its query specifies only that
-- resource type, but your @resource-type@ filter includes EC2 instances,
-- AWS Resource Groups does not filter for EC2 instances. In this case, a
-- @ListGroupResources@ request returns a @BadRequestException@ error with
-- a message similar to the following:
--
-- @The resource types specified as filters in the request are not valid.@
--
-- The error includes a list of resource types that failed the validation
-- because they are not part of the query associated with the group. This
-- validation doesn\'t occur when the group query specifies
-- @AWS::AllSupported@, because a group based on such a query can contain
-- any of the allowed resource types for the query type (tag-based or AWS
-- CloudFormation stack-based queries).
newListGroupResources ::
  ListGroupResources
newListGroupResources =
  ListGroupResources'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      groupName = Prelude.Nothing,
      group' = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- provided by a previous call\'s @NextToken@ response to indicate where
-- the output should continue from.
listGroupResources_nextToken :: Lens.Lens' ListGroupResources (Prelude.Maybe Prelude.Text)
listGroupResources_nextToken = Lens.lens (\ListGroupResources' {nextToken} -> nextToken) (\s@ListGroupResources' {} a -> s {nextToken = a} :: ListGroupResources)

-- | The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that the service might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
listGroupResources_maxResults :: Lens.Lens' ListGroupResources (Prelude.Maybe Prelude.Natural)
listGroupResources_maxResults = Lens.lens (\ListGroupResources' {maxResults} -> maxResults) (\s@ListGroupResources' {} a -> s {maxResults = a} :: ListGroupResources)

-- | /__Deprecated - don\'t use this parameter. Use the @Group@ request field
-- instead.__/
listGroupResources_groupName :: Lens.Lens' ListGroupResources (Prelude.Maybe Prelude.Text)
listGroupResources_groupName = Lens.lens (\ListGroupResources' {groupName} -> groupName) (\s@ListGroupResources' {} a -> s {groupName = a} :: ListGroupResources)

-- | The name or the ARN of the resource group
listGroupResources_group :: Lens.Lens' ListGroupResources (Prelude.Maybe Prelude.Text)
listGroupResources_group = Lens.lens (\ListGroupResources' {group'} -> group') (\s@ListGroupResources' {} a -> s {group' = a} :: ListGroupResources)

-- | Filters, formatted as ResourceFilter objects, that you want to apply to
-- a @ListGroupResources@ operation. Filters the results to include only
-- those of the specified resource types.
--
-- -   @resource-type@ - Filter resources by their type. Specify up to five
--     resource types in the format @AWS::ServiceCode::ResourceType@. For
--     example, @AWS::EC2::Instance@, or @AWS::S3::Bucket@.
--
-- When you specify a @resource-type@ filter for @ListGroupResources@, AWS
-- Resource Groups validates your filter resource types against the types
-- that are defined in the query associated with the group. For example, if
-- a group contains only S3 buckets because its query specifies only that
-- resource type, but your @resource-type@ filter includes EC2 instances,
-- AWS Resource Groups does not filter for EC2 instances. In this case, a
-- @ListGroupResources@ request returns a @BadRequestException@ error with
-- a message similar to the following:
--
-- @The resource types specified as filters in the request are not valid.@
--
-- The error includes a list of resource types that failed the validation
-- because they are not part of the query associated with the group. This
-- validation doesn\'t occur when the group query specifies
-- @AWS::AllSupported@, because a group based on such a query can contain
-- any of the allowed resource types for the query type (tag-based or AWS
-- CloudFormation stack-based queries).
listGroupResources_filters :: Lens.Lens' ListGroupResources (Prelude.Maybe [ResourceFilter])
listGroupResources_filters = Lens.lens (\ListGroupResources' {filters} -> filters) (\s@ListGroupResources' {} a -> s {filters = a} :: ListGroupResources) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager ListGroupResources where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listGroupResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listGroupResourcesResponse_resourceIdentifiers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listGroupResourcesResponse_resources
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listGroupResources_nextToken
          Lens..~ rs
          Lens.^? listGroupResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListGroupResources where
  type
    Rs ListGroupResources =
      ListGroupResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupResourcesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ResourceIdentifiers"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..?> "Resources"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..?> "QueryErrors"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGroupResources

instance Prelude.NFData ListGroupResources

instance Prelude.ToHeaders ListGroupResources where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON ListGroupResources where
  toJSON ListGroupResources' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("GroupName" Prelude..=) Prelude.<$> groupName,
            ("Group" Prelude..=) Prelude.<$> group',
            ("Filters" Prelude..=) Prelude.<$> filters
          ]
      )

instance Prelude.ToPath ListGroupResources where
  toPath = Prelude.const "/list-group-resources"

instance Prelude.ToQuery ListGroupResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGroupResourcesResponse' smart constructor.
data ListGroupResourcesResponse = ListGroupResourcesResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | __/Deprecated - don\'t use this parameter. Use the @Resources@ response
    -- field instead./__
    resourceIdentifiers :: Prelude.Maybe [ResourceIdentifier],
    -- | An array of resources from which you can determine each resource\'s
    -- identity, type, and group membership status.
    resources :: Prelude.Maybe [ListGroupResourcesItem],
    -- | A list of @QueryError@ objects. Each error is an object that contains
    -- @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@
    -- are @CLOUDFORMATION_STACK_INACTIVE@ and
    -- @CLOUDFORMATION_STACK_NOT_EXISTING@.
    queryErrors :: Prelude.Maybe [QueryError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListGroupResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGroupResourcesResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'resourceIdentifiers', 'listGroupResourcesResponse_resourceIdentifiers' - __/Deprecated - don\'t use this parameter. Use the @Resources@ response
-- field instead./__
--
-- 'resources', 'listGroupResourcesResponse_resources' - An array of resources from which you can determine each resource\'s
-- identity, type, and group membership status.
--
-- 'queryErrors', 'listGroupResourcesResponse_queryErrors' - A list of @QueryError@ objects. Each error is an object that contains
-- @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@
-- are @CLOUDFORMATION_STACK_INACTIVE@ and
-- @CLOUDFORMATION_STACK_NOT_EXISTING@.
--
-- 'httpStatus', 'listGroupResourcesResponse_httpStatus' - The response's http status code.
newListGroupResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupResourcesResponse
newListGroupResourcesResponse pHttpStatus_ =
  ListGroupResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      resourceIdentifiers = Prelude.Nothing,
      resources = Prelude.Nothing,
      queryErrors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listGroupResourcesResponse_nextToken :: Lens.Lens' ListGroupResourcesResponse (Prelude.Maybe Prelude.Text)
listGroupResourcesResponse_nextToken = Lens.lens (\ListGroupResourcesResponse' {nextToken} -> nextToken) (\s@ListGroupResourcesResponse' {} a -> s {nextToken = a} :: ListGroupResourcesResponse)

-- | __/Deprecated - don\'t use this parameter. Use the @Resources@ response
-- field instead./__
listGroupResourcesResponse_resourceIdentifiers :: Lens.Lens' ListGroupResourcesResponse (Prelude.Maybe [ResourceIdentifier])
listGroupResourcesResponse_resourceIdentifiers = Lens.lens (\ListGroupResourcesResponse' {resourceIdentifiers} -> resourceIdentifiers) (\s@ListGroupResourcesResponse' {} a -> s {resourceIdentifiers = a} :: ListGroupResourcesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An array of resources from which you can determine each resource\'s
-- identity, type, and group membership status.
listGroupResourcesResponse_resources :: Lens.Lens' ListGroupResourcesResponse (Prelude.Maybe [ListGroupResourcesItem])
listGroupResourcesResponse_resources = Lens.lens (\ListGroupResourcesResponse' {resources} -> resources) (\s@ListGroupResourcesResponse' {} a -> s {resources = a} :: ListGroupResourcesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of @QueryError@ objects. Each error is an object that contains
-- @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@
-- are @CLOUDFORMATION_STACK_INACTIVE@ and
-- @CLOUDFORMATION_STACK_NOT_EXISTING@.
listGroupResourcesResponse_queryErrors :: Lens.Lens' ListGroupResourcesResponse (Prelude.Maybe [QueryError])
listGroupResourcesResponse_queryErrors = Lens.lens (\ListGroupResourcesResponse' {queryErrors} -> queryErrors) (\s@ListGroupResourcesResponse' {} a -> s {queryErrors = a} :: ListGroupResourcesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listGroupResourcesResponse_httpStatus :: Lens.Lens' ListGroupResourcesResponse Prelude.Int
listGroupResourcesResponse_httpStatus = Lens.lens (\ListGroupResourcesResponse' {httpStatus} -> httpStatus) (\s@ListGroupResourcesResponse' {} a -> s {httpStatus = a} :: ListGroupResourcesResponse)

instance Prelude.NFData ListGroupResourcesResponse
