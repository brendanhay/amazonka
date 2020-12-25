{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLaunchTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more launch templates.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLaunchTemplates
  ( -- * Creating a request
    DescribeLaunchTemplates (..),
    mkDescribeLaunchTemplates,

    -- ** Request lenses
    dltsDryRun,
    dltsFilters,
    dltsLaunchTemplateIds,
    dltsLaunchTemplateNames,
    dltsMaxResults,
    dltsNextToken,

    -- * Destructuring the response
    DescribeLaunchTemplatesResponse (..),
    mkDescribeLaunchTemplatesResponse,

    -- ** Response lenses
    dltrfrsLaunchTemplates,
    dltrfrsNextToken,
    dltrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLaunchTemplates' smart constructor.
data DescribeLaunchTemplates = DescribeLaunchTemplates'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @create-time@ - The time the launch template was created.
    --
    --
    --     * @launch-template-name@ - The name of the launch template.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    filters :: Core.Maybe [Types.Filter],
    -- | One or more launch template IDs.
    launchTemplateIds :: Core.Maybe [Types.LaunchTemplateId],
    -- | One or more launch template names.
    launchTemplateNames :: Core.Maybe [Types.LaunchTemplateName],
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to request the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLaunchTemplates' value with any optional fields omitted.
mkDescribeLaunchTemplates ::
  DescribeLaunchTemplates
mkDescribeLaunchTemplates =
  DescribeLaunchTemplates'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      launchTemplateIds = Core.Nothing,
      launchTemplateNames = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsDryRun :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe Core.Bool)
dltsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dltsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters.
--
--
--     * @create-time@ - The time the launch template was created.
--
--
--     * @launch-template-name@ - The name of the launch template.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsFilters :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe [Types.Filter])
dltsFilters = Lens.field @"filters"
{-# DEPRECATED dltsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more launch template IDs.
--
-- /Note:/ Consider using 'launchTemplateIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsLaunchTemplateIds :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe [Types.LaunchTemplateId])
dltsLaunchTemplateIds = Lens.field @"launchTemplateIds"
{-# DEPRECATED dltsLaunchTemplateIds "Use generic-lens or generic-optics with 'launchTemplateIds' instead." #-}

-- | One or more launch template names.
--
-- /Note:/ Consider using 'launchTemplateNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsLaunchTemplateNames :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe [Types.LaunchTemplateName])
dltsLaunchTemplateNames = Lens.field @"launchTemplateNames"
{-# DEPRECATED dltsLaunchTemplateNames "Use generic-lens or generic-optics with 'launchTemplateNames' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsMaxResults :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe Core.Natural)
dltsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dltsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsNextToken :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe Types.String)
dltsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dltsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeLaunchTemplates where
  type Rs DescribeLaunchTemplates = DescribeLaunchTemplatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeLaunchTemplates")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "LaunchTemplateId" Core.<$> launchTemplateIds)
                Core.<> ( Core.toQueryList "LaunchTemplateName"
                            Core.<$> launchTemplateNames
                        )
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLaunchTemplatesResponse'
            Core.<$> (x Core..@? "launchTemplates" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeLaunchTemplates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"launchTemplates" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeLaunchTemplatesResponse' smart constructor.
data DescribeLaunchTemplatesResponse = DescribeLaunchTemplatesResponse'
  { -- | Information about the launch templates.
    launchTemplates :: Core.Maybe [Types.LaunchTemplate],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeLaunchTemplatesResponse' value with any optional fields omitted.
mkDescribeLaunchTemplatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLaunchTemplatesResponse
mkDescribeLaunchTemplatesResponse responseStatus =
  DescribeLaunchTemplatesResponse'
    { launchTemplates = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the launch templates.
--
-- /Note:/ Consider using 'launchTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltrfrsLaunchTemplates :: Lens.Lens' DescribeLaunchTemplatesResponse (Core.Maybe [Types.LaunchTemplate])
dltrfrsLaunchTemplates = Lens.field @"launchTemplates"
{-# DEPRECATED dltrfrsLaunchTemplates "Use generic-lens or generic-optics with 'launchTemplates' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltrfrsNextToken :: Lens.Lens' DescribeLaunchTemplatesResponse (Core.Maybe Types.String)
dltrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dltrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltrfrsResponseStatus :: Lens.Lens' DescribeLaunchTemplatesResponse Core.Int
dltrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dltrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
