{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.ListGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing resource groups in your account.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.ListGroups
  ( -- * Creating a request
    ListGroups (..),
    mkListGroups,

    -- ** Request lenses
    lgFilters,
    lgMaxResults,
    lgNextToken,

    -- * Destructuring the response
    ListGroupsResponse (..),
    mkListGroupsResponse,

    -- ** Response lenses
    lgrrsGroupIdentifiers,
    lgrrsGroups,
    lgrrsNextToken,
    lgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGroups' smart constructor.
data ListGroups = ListGroups'
  { -- | Filters, formatted as 'GroupFilter' objects, that you want to apply to a @ListGroups@ operation.
    --
    --
    --     * @resource-type@ - Filter the results to include only those of the specified resource types. Specify up to five resource types in the format @AWS::/ServiceCode/ ::/ResourceType/ @ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .
    --
    --
    --     * @configuration-type@ - Filter the results to include only those groups that have the specified configuration types attached. The current supported values are:
    --
    --     * AWS:EC2::CapacityReservationPool
    filters :: Core.Maybe [Types.GroupFilter],
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroups' value with any optional fields omitted.
mkListGroups ::
  ListGroups
mkListGroups =
  ListGroups'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Filters, formatted as 'GroupFilter' objects, that you want to apply to a @ListGroups@ operation.
--
--
--     * @resource-type@ - Filter the results to include only those of the specified resource types. Specify up to five resource types in the format @AWS::/ServiceCode/ ::/ResourceType/ @ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .
--
--
--     * @configuration-type@ - Filter the results to include only those groups that have the specified configuration types attached. The current supported values are:
--
--     * AWS:EC2::CapacityReservationPool
--
--
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgFilters :: Lens.Lens' ListGroups (Core.Maybe [Types.GroupFilter])
lgFilters = Lens.field @"filters"
{-# DEPRECATED lgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMaxResults :: Lens.Lens' ListGroups (Core.Maybe Core.Natural)
lgMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgNextToken :: Lens.Lens' ListGroups (Core.Maybe Types.NextToken)
lgNextToken = Lens.field @"nextToken"
{-# DEPRECATED lgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListGroups where
  toJSON ListGroups {..} =
    Core.object
      (Core.catMaybes [("Filters" Core..=) Core.<$> filters])

instance Core.AWSRequest ListGroups where
  type Rs ListGroups = ListGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/groups-list",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupsResponse'
            Core.<$> (x Core..:? "GroupIdentifiers")
            Core.<*> (x Core..:? "Groups")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"groupIdentifiers" Core.. Lens._Just) =
      Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"groups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { -- | A list of 'GroupIdentifier' objects. Each identifier is an object that contains both the @Name@ and the @GroupArn@ .
    groupIdentifiers :: Core.Maybe [Types.GroupIdentifier],
    -- | This output element is deprecated and shouldn't be used. Refer to @GroupIdentifiers@ instead.
    groups :: Core.Maybe [Types.Group],
    -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroupsResponse' value with any optional fields omitted.
mkListGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListGroupsResponse
mkListGroupsResponse responseStatus =
  ListGroupsResponse'
    { groupIdentifiers = Core.Nothing,
      groups = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of 'GroupIdentifier' objects. Each identifier is an object that contains both the @Name@ and the @GroupArn@ .
--
-- /Note:/ Consider using 'groupIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsGroupIdentifiers :: Lens.Lens' ListGroupsResponse (Core.Maybe [Types.GroupIdentifier])
lgrrsGroupIdentifiers = Lens.field @"groupIdentifiers"
{-# DEPRECATED lgrrsGroupIdentifiers "Use generic-lens or generic-optics with 'groupIdentifiers' instead." #-}

-- | This output element is deprecated and shouldn't be used. Refer to @GroupIdentifiers@ instead.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsGroups :: Lens.Lens' ListGroupsResponse (Core.Maybe [Types.Group])
lgrrsGroups = Lens.field @"groups"
{-# DEPRECATED lgrrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsNextToken :: Lens.Lens' ListGroupsResponse (Core.Maybe Types.NextToken)
lgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsResponseStatus :: Lens.Lens' ListGroupsResponse Core.Int
lgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
