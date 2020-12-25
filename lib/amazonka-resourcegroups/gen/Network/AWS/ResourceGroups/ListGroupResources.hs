{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.ListGroupResources
  ( -- * Creating a request
    ListGroupResources (..),
    mkListGroupResources,

    -- ** Request lenses
    lgrFilters,
    lgrGroup,
    lgrGroupName,
    lgrMaxResults,
    lgrNextToken,

    -- * Destructuring the response
    ListGroupResourcesResponse (..),
    mkListGroupResourcesResponse,

    -- ** Response lenses
    lgrrrsNextToken,
    lgrrrsQueryErrors,
    lgrrrsResourceIdentifiers,
    lgrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGroupResources' smart constructor.
data ListGroupResources = ListGroupResources'
  { -- | Filters, formatted as 'ResourceFilter' objects, that you want to apply to a @ListGroupResources@ operation. Filters the results to include only those of the specified resource types.
    --
    --
    --     * @resource-type@ - Filter resources by their type. Specify up to five resource types in the format @AWS::ServiceCode::ResourceType@ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .
    --
    --
    -- When you specify a @resource-type@ filter for @ListGroupResources@ , AWS Resource Groups validates your filter resource types against the types that are defined in the query associated with the group. For example, if a group contains only S3 buckets because its query specifies only that resource type, but your @resource-type@ filter includes EC2 instances, AWS Resource Groups does not filter for EC2 instances. In this case, a @ListGroupResources@ request returns a @BadRequestException@ error with a message similar to the following:
    -- @The resource types specified as filters in the request are not valid.@
    -- The error includes a list of resource types that failed the validation because they are not part of the query associated with the group. This validation doesn't occur when the group query specifies @AWS::AllSupported@ , because a group based on such a query can contain any of the allowed resource types for the query type (tag-based or AWS CloudFormation stack-based queries).
    filters :: Core.Maybe [Types.ResourceFilter],
    -- | The name or the ARN of the resource group
    group :: Core.Maybe Types.GroupString,
    -- | Don't use this parameter. Use @Group@ instead.
    groupName :: Core.Maybe Types.GroupName,
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroupResources' value with any optional fields omitted.
mkListGroupResources ::
  ListGroupResources
mkListGroupResources =
  ListGroupResources'
    { filters = Core.Nothing,
      group = Core.Nothing,
      groupName = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

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
lgrFilters :: Lens.Lens' ListGroupResources (Core.Maybe [Types.ResourceFilter])
lgrFilters = Lens.field @"filters"
{-# DEPRECATED lgrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The name or the ARN of the resource group
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrGroup :: Lens.Lens' ListGroupResources (Core.Maybe Types.GroupString)
lgrGroup = Lens.field @"group"
{-# DEPRECATED lgrGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrGroupName :: Lens.Lens' ListGroupResources (Core.Maybe Types.GroupName)
lgrGroupName = Lens.field @"groupName"
{-# DEPRECATED lgrGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrMaxResults :: Lens.Lens' ListGroupResources (Core.Maybe Core.Natural)
lgrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lgrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrNextToken :: Lens.Lens' ListGroupResources (Core.Maybe Types.NextToken)
lgrNextToken = Lens.field @"nextToken"
{-# DEPRECATED lgrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListGroupResources where
  toJSON ListGroupResources {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("Group" Core..=) Core.<$> group,
            ("GroupName" Core..=) Core.<$> groupName,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListGroupResources where
  type Rs ListGroupResources = ListGroupResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/list-group-resources",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupResourcesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "QueryErrors")
            Core.<*> (x Core..:? "ResourceIdentifiers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListGroupResources where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"resourceIdentifiers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListGroupResourcesResponse' smart constructor.
data ListGroupResourcesResponse = ListGroupResourcesResponse'
  { -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
    queryErrors :: Core.Maybe [Types.QueryError],
    -- | The ARNs and resource types of resources that are members of the group that you specified.
    resourceIdentifiers :: Core.Maybe [Types.ResourceIdentifier],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroupResourcesResponse' value with any optional fields omitted.
mkListGroupResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListGroupResourcesResponse
mkListGroupResourcesResponse responseStatus =
  ListGroupResourcesResponse'
    { nextToken = Core.Nothing,
      queryErrors = Core.Nothing,
      resourceIdentifiers = Core.Nothing,
      responseStatus
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrrsNextToken :: Lens.Lens' ListGroupResourcesResponse (Core.Maybe Types.NextToken)
lgrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lgrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
--
-- /Note:/ Consider using 'queryErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrrsQueryErrors :: Lens.Lens' ListGroupResourcesResponse (Core.Maybe [Types.QueryError])
lgrrrsQueryErrors = Lens.field @"queryErrors"
{-# DEPRECATED lgrrrsQueryErrors "Use generic-lens or generic-optics with 'queryErrors' instead." #-}

-- | The ARNs and resource types of resources that are members of the group that you specified.
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrrsResourceIdentifiers :: Lens.Lens' ListGroupResourcesResponse (Core.Maybe [Types.ResourceIdentifier])
lgrrrsResourceIdentifiers = Lens.field @"resourceIdentifiers"
{-# DEPRECATED lgrrrsResourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrrsResponseStatus :: Lens.Lens' ListGroupResourcesResponse Core.Int
lgrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lgrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
