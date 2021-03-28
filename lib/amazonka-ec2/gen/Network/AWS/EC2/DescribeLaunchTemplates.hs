{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeLaunchTemplates (..)
    , mkDescribeLaunchTemplates
    -- ** Request lenses
    , dltsDryRun
    , dltsFilters
    , dltsLaunchTemplateIds
    , dltsLaunchTemplateNames
    , dltsMaxResults
    , dltsNextToken

    -- * Destructuring the response
    , DescribeLaunchTemplatesResponse (..)
    , mkDescribeLaunchTemplatesResponse
    -- ** Response lenses
    , dltrfrsLaunchTemplates
    , dltrfrsNextToken
    , dltrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLaunchTemplates' smart constructor.
data DescribeLaunchTemplates = DescribeLaunchTemplates'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
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
  , launchTemplateIds :: Core.Maybe [Types.LaunchTemplateId]
    -- ^ One or more launch template IDs.
  , launchTemplateNames :: Core.Maybe [Types.LaunchTemplateName]
    -- ^ One or more launch template names.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to request the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLaunchTemplates' value with any optional fields omitted.
mkDescribeLaunchTemplates
    :: DescribeLaunchTemplates
mkDescribeLaunchTemplates
  = DescribeLaunchTemplates'{dryRun = Core.Nothing,
                             filters = Core.Nothing, launchTemplateIds = Core.Nothing,
                             launchTemplateNames = Core.Nothing, maxResults = Core.Nothing,
                             nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsDryRun :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe Core.Bool)
dltsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dltsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
{-# INLINEABLE dltsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | One or more launch template IDs.
--
-- /Note:/ Consider using 'launchTemplateIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsLaunchTemplateIds :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe [Types.LaunchTemplateId])
dltsLaunchTemplateIds = Lens.field @"launchTemplateIds"
{-# INLINEABLE dltsLaunchTemplateIds #-}
{-# DEPRECATED launchTemplateIds "Use generic-lens or generic-optics with 'launchTemplateIds' instead"  #-}

-- | One or more launch template names.
--
-- /Note:/ Consider using 'launchTemplateNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsLaunchTemplateNames :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe [Types.LaunchTemplateName])
dltsLaunchTemplateNames = Lens.field @"launchTemplateNames"
{-# INLINEABLE dltsLaunchTemplateNames #-}
{-# DEPRECATED launchTemplateNames "Use generic-lens or generic-optics with 'launchTemplateNames' instead"  #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsMaxResults :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe Core.Natural)
dltsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dltsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsNextToken :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe Core.Text)
dltsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dltsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeLaunchTemplates where
        toQuery DescribeLaunchTemplates{..}
          = Core.toQueryPair "Action"
              ("DescribeLaunchTemplates" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "LaunchTemplateId")
                launchTemplateIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "LaunchTemplateName")
                launchTemplateNames
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeLaunchTemplates where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeLaunchTemplates where
        type Rs DescribeLaunchTemplates = DescribeLaunchTemplatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeLaunchTemplatesResponse' Core.<$>
                   (x Core..@? "launchTemplates" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeLaunchTemplates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"launchTemplates" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeLaunchTemplatesResponse' smart constructor.
data DescribeLaunchTemplatesResponse = DescribeLaunchTemplatesResponse'
  { launchTemplates :: Core.Maybe [Types.LaunchTemplate]
    -- ^ Information about the launch templates.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeLaunchTemplatesResponse' value with any optional fields omitted.
mkDescribeLaunchTemplatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLaunchTemplatesResponse
mkDescribeLaunchTemplatesResponse responseStatus
  = DescribeLaunchTemplatesResponse'{launchTemplates = Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | Information about the launch templates.
--
-- /Note:/ Consider using 'launchTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltrfrsLaunchTemplates :: Lens.Lens' DescribeLaunchTemplatesResponse (Core.Maybe [Types.LaunchTemplate])
dltrfrsLaunchTemplates = Lens.field @"launchTemplates"
{-# INLINEABLE dltrfrsLaunchTemplates #-}
{-# DEPRECATED launchTemplates "Use generic-lens or generic-optics with 'launchTemplates' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltrfrsNextToken :: Lens.Lens' DescribeLaunchTemplatesResponse (Core.Maybe Core.Text)
dltrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dltrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltrfrsResponseStatus :: Lens.Lens' DescribeLaunchTemplatesResponse Core.Int
dltrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dltrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
