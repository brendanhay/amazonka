{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.SearchResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of AWS resource identifiers that matches the specified query. The query uses the same format as a resource query in a CreateGroup or UpdateGroupQuery operation.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.SearchResources
  ( -- * Creating a request
    SearchResources (..),
    mkSearchResources,

    -- ** Request lenses
    srResourceQuery,
    srMaxResults,
    srNextToken,

    -- * Destructuring the response
    SearchResourcesResponse (..),
    mkSearchResourcesResponse,

    -- ** Response lenses
    srrrsNextToken,
    srrrsQueryErrors,
    srrrsResourceIdentifiers,
    srrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchResources' smart constructor.
data SearchResources = SearchResources'
  { -- | The search query, using the same formats that are supported for resource group definition. For more information, see 'CreateGroup' .
    resourceQuery :: Types.ResourceQuery,
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchResources' value with any optional fields omitted.
mkSearchResources ::
  -- | 'resourceQuery'
  Types.ResourceQuery ->
  SearchResources
mkSearchResources resourceQuery =
  SearchResources'
    { resourceQuery,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The search query, using the same formats that are supported for resource group definition. For more information, see 'CreateGroup' .
--
-- /Note:/ Consider using 'resourceQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srResourceQuery :: Lens.Lens' SearchResources Types.ResourceQuery
srResourceQuery = Lens.field @"resourceQuery"
{-# DEPRECATED srResourceQuery "Use generic-lens or generic-optics with 'resourceQuery' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srMaxResults :: Lens.Lens' SearchResources (Core.Maybe Core.Natural)
srMaxResults = Lens.field @"maxResults"
{-# DEPRECATED srMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srNextToken :: Lens.Lens' SearchResources (Core.Maybe Types.NextToken)
srNextToken = Lens.field @"nextToken"
{-# DEPRECATED srNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON SearchResources where
  toJSON SearchResources {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceQuery" Core..= resourceQuery),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest SearchResources where
  type Rs SearchResources = SearchResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/resources/search",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchResourcesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "QueryErrors")
            Core.<*> (x Core..:? "ResourceIdentifiers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager SearchResources where
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

-- | /See:/ 'mkSearchResourcesResponse' smart constructor.
data SearchResourcesResponse = SearchResourcesResponse'
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

-- | Creates a 'SearchResourcesResponse' value with any optional fields omitted.
mkSearchResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchResourcesResponse
mkSearchResourcesResponse responseStatus =
  SearchResourcesResponse'
    { nextToken = Core.Nothing,
      queryErrors = Core.Nothing,
      resourceIdentifiers = Core.Nothing,
      responseStatus
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsNextToken :: Lens.Lens' SearchResourcesResponse (Core.Maybe Types.NextToken)
srrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED srrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
--
-- /Note:/ Consider using 'queryErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsQueryErrors :: Lens.Lens' SearchResourcesResponse (Core.Maybe [Types.QueryError])
srrrsQueryErrors = Lens.field @"queryErrors"
{-# DEPRECATED srrrsQueryErrors "Use generic-lens or generic-optics with 'queryErrors' instead." #-}

-- | The ARNs and resource types of resources that are members of the group that you specified.
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsResourceIdentifiers :: Lens.Lens' SearchResourcesResponse (Core.Maybe [Types.ResourceIdentifier])
srrrsResourceIdentifiers = Lens.field @"resourceIdentifiers"
{-# DEPRECATED srrrsResourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsResponseStatus :: Lens.Lens' SearchResourcesResponse Core.Int
srrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
