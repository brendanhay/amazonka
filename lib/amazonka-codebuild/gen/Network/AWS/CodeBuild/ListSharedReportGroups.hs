{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListSharedReportGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of report groups that are shared with other AWS accounts or users.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListSharedReportGroups
  ( -- * Creating a request
    ListSharedReportGroups (..),
    mkListSharedReportGroups,

    -- ** Request lenses
    lsrgMaxResults,
    lsrgNextToken,
    lsrgSortBy,
    lsrgSortOrder,

    -- * Destructuring the response
    ListSharedReportGroupsResponse (..),
    mkListSharedReportGroupsResponse,

    -- ** Response lenses
    lsrgrrsNextToken,
    lsrgrrsReportGroups,
    lsrgrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSharedReportGroups' smart constructor.
data ListSharedReportGroups = ListSharedReportGroups'
  { -- | The maximum number of paginated shared report groups per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Core.Maybe Types.String,
    -- | The criterion to be used to list report groups shared with the current AWS account or user. Valid values include:
    --
    --
    --     * @ARN@ : List based on the ARN.
    --
    --
    --     * @MODIFIED_TIME@ : List based on when information about the shared report group was last changed.
    sortBy :: Core.Maybe Types.SharedResourceSortByType,
    -- | The order in which to list shared report groups. Valid values include:
    --
    --
    --     * @ASCENDING@ : List in ascending order.
    --
    --
    --     * @DESCENDING@ : List in descending order.
    sortOrder :: Core.Maybe Types.SortOrderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSharedReportGroups' value with any optional fields omitted.
mkListSharedReportGroups ::
  ListSharedReportGroups
mkListSharedReportGroups =
  ListSharedReportGroups'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | The maximum number of paginated shared report groups per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgMaxResults :: Lens.Lens' ListSharedReportGroups (Core.Maybe Core.Natural)
lsrgMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lsrgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgNextToken :: Lens.Lens' ListSharedReportGroups (Core.Maybe Types.String)
lsrgNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsrgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The criterion to be used to list report groups shared with the current AWS account or user. Valid values include:
--
--
--     * @ARN@ : List based on the ARN.
--
--
--     * @MODIFIED_TIME@ : List based on when information about the shared report group was last changed.
--
--
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgSortBy :: Lens.Lens' ListSharedReportGroups (Core.Maybe Types.SharedResourceSortByType)
lsrgSortBy = Lens.field @"sortBy"
{-# DEPRECATED lsrgSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The order in which to list shared report groups. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgSortOrder :: Lens.Lens' ListSharedReportGroups (Core.Maybe Types.SortOrderType)
lsrgSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lsrgSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListSharedReportGroups where
  toJSON ListSharedReportGroups {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("sortBy" Core..=) Core.<$> sortBy,
            ("sortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListSharedReportGroups where
  type Rs ListSharedReportGroups = ListSharedReportGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeBuild_20161006.ListSharedReportGroups")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSharedReportGroupsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "reportGroups")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSharedReportGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"reportGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSharedReportGroupsResponse' smart constructor.
data ListSharedReportGroupsResponse = ListSharedReportGroupsResponse'
  { -- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Core.Maybe Types.String,
    -- | The list of ARNs for the report groups shared with the current AWS account or user.
    reportGroups :: Core.Maybe (Core.NonEmpty Types.NonEmptyString),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSharedReportGroupsResponse' value with any optional fields omitted.
mkListSharedReportGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSharedReportGroupsResponse
mkListSharedReportGroupsResponse responseStatus =
  ListSharedReportGroupsResponse'
    { nextToken = Core.Nothing,
      reportGroups = Core.Nothing,
      responseStatus
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrrsNextToken :: Lens.Lens' ListSharedReportGroupsResponse (Core.Maybe Types.String)
lsrgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsrgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of ARNs for the report groups shared with the current AWS account or user.
--
-- /Note:/ Consider using 'reportGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrrsReportGroups :: Lens.Lens' ListSharedReportGroupsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
lsrgrrsReportGroups = Lens.field @"reportGroups"
{-# DEPRECATED lsrgrrsReportGroups "Use generic-lens or generic-optics with 'reportGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrrsResponseStatus :: Lens.Lens' ListSharedReportGroupsResponse Core.Int
lsrgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsrgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
