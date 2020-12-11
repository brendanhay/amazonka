{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.ListGroupResources
  ( -- * Creating a request
    ListGroupResources (..),
    mkListGroupResources,

    -- ** Request lenses
    lgrGroup,
    lgrFilters,
    lgrNextToken,
    lgrGroupName,
    lgrMaxResults,

    -- * Destructuring the response
    ListGroupResourcesResponse (..),
    mkListGroupResourcesResponse,

    -- ** Response lenses
    lgrrsQueryErrors,
    lgrrsNextToken,
    lgrrsResourceIdentifiers,
    lgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGroupResources' smart constructor.
data ListGroupResources = ListGroupResources'
  { group ::
      Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [ResourceFilter],
    nextToken :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupResources' with the minimum fields required to make a request.
--
-- * 'filters' - Filters, formatted as 'ResourceFilter' objects, that you want to apply to a @ListGroupResources@ operation. Filters the results to include only those of the specified resource types.
--
--
--     * @resource-type@ - Filter resources by their type. Specify up to five resource types in the format @AWS::ServiceCode::ResourceType@ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .
--
--
-- When you specify a @resource-type@ filter for @ListGroupResources@ , AWS Resource Groups validates your filter resource types against the types that are defined in the query associated with the group. For example, if a group contains only S3 buckets because its query specifies only that resource type, but your @resource-type@ filter includes EC2 instances, AWS Resource Groups does not filter for EC2 instances. In this case, a @ListGroupResources@ request returns a @BadRequestException@ error with a message similar to the following:
-- @The resource types specified as filters in the request are not valid.@
-- The error includes a list of resource types that failed the validation because they are not part of the query associated with the group. This validation doesn't occur when the group query specifies @AWS::AllSupported@ , because a group based on such a query can contain any of the allowed resource types for the query type (tag-based or AWS CloudFormation stack-based queries).
-- * 'group' - The name or the ARN of the resource group
-- * 'groupName' - Don't use this parameter. Use @Group@ instead.
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
mkListGroupResources ::
  ListGroupResources
mkListGroupResources =
  ListGroupResources'
    { group = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      groupName = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name or the ARN of the resource group
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrGroup :: Lens.Lens' ListGroupResources (Lude.Maybe Lude.Text)
lgrGroup = Lens.lens (group :: ListGroupResources -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: ListGroupResources)
{-# DEPRECATED lgrGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | Filters, formatted as 'ResourceFilter' objects, that you want to apply to a @ListGroupResources@ operation. Filters the results to include only those of the specified resource types.
--
--
--     * @resource-type@ - Filter resources by their type. Specify up to five resource types in the format @AWS::ServiceCode::ResourceType@ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .
--
--
-- When you specify a @resource-type@ filter for @ListGroupResources@ , AWS Resource Groups validates your filter resource types against the types that are defined in the query associated with the group. For example, if a group contains only S3 buckets because its query specifies only that resource type, but your @resource-type@ filter includes EC2 instances, AWS Resource Groups does not filter for EC2 instances. In this case, a @ListGroupResources@ request returns a @BadRequestException@ error with a message similar to the following:
-- @The resource types specified as filters in the request are not valid.@
-- The error includes a list of resource types that failed the validation because they are not part of the query associated with the group. This validation doesn't occur when the group query specifies @AWS::AllSupported@ , because a group based on such a query can contain any of the allowed resource types for the query type (tag-based or AWS CloudFormation stack-based queries).
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrFilters :: Lens.Lens' ListGroupResources (Lude.Maybe [ResourceFilter])
lgrFilters = Lens.lens (filters :: ListGroupResources -> Lude.Maybe [ResourceFilter]) (\s a -> s {filters = a} :: ListGroupResources)
{-# DEPRECATED lgrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrNextToken :: Lens.Lens' ListGroupResources (Lude.Maybe Lude.Text)
lgrNextToken = Lens.lens (nextToken :: ListGroupResources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroupResources)
{-# DEPRECATED lgrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrGroupName :: Lens.Lens' ListGroupResources (Lude.Maybe Lude.Text)
lgrGroupName = Lens.lens (groupName :: ListGroupResources -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: ListGroupResources)
{-# DEPRECATED lgrGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrMaxResults :: Lens.Lens' ListGroupResources (Lude.Maybe Lude.Natural)
lgrMaxResults = Lens.lens (maxResults :: ListGroupResources -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListGroupResources)
{-# DEPRECATED lgrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListGroupResources where
  page rq rs
    | Page.stop (rs Lens.^. lgrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lgrrsResourceIdentifiers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgrNextToken Lens..~ rs Lens.^. lgrrsNextToken

instance Lude.AWSRequest ListGroupResources where
  type Rs ListGroupResources = ListGroupResourcesResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGroupResourcesResponse'
            Lude.<$> (x Lude..?> "QueryErrors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ResourceIdentifiers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGroupResources where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListGroupResources where
  toJSON ListGroupResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Group" Lude..=) Lude.<$> group,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("GroupName" Lude..=) Lude.<$> groupName,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListGroupResources where
  toPath = Lude.const "/list-group-resources"

instance Lude.ToQuery ListGroupResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGroupResourcesResponse' smart constructor.
data ListGroupResourcesResponse = ListGroupResourcesResponse'
  { queryErrors ::
      Lude.Maybe [QueryError],
    nextToken :: Lude.Maybe Lude.Text,
    resourceIdentifiers ::
      Lude.Maybe [ResourceIdentifier],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupResourcesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'queryErrors' - A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
-- * 'resourceIdentifiers' - The ARNs and resource types of resources that are members of the group that you specified.
-- * 'responseStatus' - The response status code.
mkListGroupResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupResourcesResponse
mkListGroupResourcesResponse pResponseStatus_ =
  ListGroupResourcesResponse'
    { queryErrors = Lude.Nothing,
      nextToken = Lude.Nothing,
      resourceIdentifiers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
--
-- /Note:/ Consider using 'queryErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsQueryErrors :: Lens.Lens' ListGroupResourcesResponse (Lude.Maybe [QueryError])
lgrrsQueryErrors = Lens.lens (queryErrors :: ListGroupResourcesResponse -> Lude.Maybe [QueryError]) (\s a -> s {queryErrors = a} :: ListGroupResourcesResponse)
{-# DEPRECATED lgrrsQueryErrors "Use generic-lens or generic-optics with 'queryErrors' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsNextToken :: Lens.Lens' ListGroupResourcesResponse (Lude.Maybe Lude.Text)
lgrrsNextToken = Lens.lens (nextToken :: ListGroupResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroupResourcesResponse)
{-# DEPRECATED lgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ARNs and resource types of resources that are members of the group that you specified.
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsResourceIdentifiers :: Lens.Lens' ListGroupResourcesResponse (Lude.Maybe [ResourceIdentifier])
lgrrsResourceIdentifiers = Lens.lens (resourceIdentifiers :: ListGroupResourcesResponse -> Lude.Maybe [ResourceIdentifier]) (\s a -> s {resourceIdentifiers = a} :: ListGroupResourcesResponse)
{-# DEPRECATED lgrrsResourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsResponseStatus :: Lens.Lens' ListGroupResourcesResponse Lude.Int
lgrrsResponseStatus = Lens.lens (responseStatus :: ListGroupResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupResourcesResponse)
{-# DEPRECATED lgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
