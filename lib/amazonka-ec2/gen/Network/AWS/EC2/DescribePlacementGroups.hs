{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePlacementGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified placement groups or all of your placement groups. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribePlacementGroups
    (
    -- * Creating a request
      DescribePlacementGroups (..)
    , mkDescribePlacementGroups
    -- ** Request lenses
    , dpgsDryRun
    , dpgsFilters
    , dpgsGroupIds
    , dpgsGroupNames

    -- * Destructuring the response
    , DescribePlacementGroupsResponse (..)
    , mkDescribePlacementGroupsResponse
    -- ** Response lenses
    , dpgrrsPlacementGroups
    , dpgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePlacementGroups' smart constructor.
data DescribePlacementGroups = DescribePlacementGroups'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ The filters.
--
--
--     * @group-name@ - The name of the placement group.
--
--
--     * @state@ - The state of the placement group (@pending@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @strategy@ - The strategy of the placement group (@cluster@ | @spread@ | @partition@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources that have a tag with a specific key, regardless of the tag value.
--
--
  , groupIds :: Core.Maybe [Types.PlacementGroupId]
    -- ^ The IDs of the placement groups.
  , groupNames :: Core.Maybe [Types.PlacementGroupName]
    -- ^ The names of the placement groups.
--
-- Default: Describes all your placement groups, or only those otherwise specified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePlacementGroups' value with any optional fields omitted.
mkDescribePlacementGroups
    :: DescribePlacementGroups
mkDescribePlacementGroups
  = DescribePlacementGroups'{dryRun = Core.Nothing,
                             filters = Core.Nothing, groupIds = Core.Nothing,
                             groupNames = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsDryRun :: Lens.Lens' DescribePlacementGroups (Core.Maybe Core.Bool)
dpgsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dpgsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The filters.
--
--
--     * @group-name@ - The name of the placement group.
--
--
--     * @state@ - The state of the placement group (@pending@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @strategy@ - The strategy of the placement group (@cluster@ | @spread@ | @partition@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources that have a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsFilters :: Lens.Lens' DescribePlacementGroups (Core.Maybe [Types.Filter])
dpgsFilters = Lens.field @"filters"
{-# INLINEABLE dpgsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The IDs of the placement groups.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsGroupIds :: Lens.Lens' DescribePlacementGroups (Core.Maybe [Types.PlacementGroupId])
dpgsGroupIds = Lens.field @"groupIds"
{-# INLINEABLE dpgsGroupIds #-}
{-# DEPRECATED groupIds "Use generic-lens or generic-optics with 'groupIds' instead"  #-}

-- | The names of the placement groups.
--
-- Default: Describes all your placement groups, or only those otherwise specified.
--
-- /Note:/ Consider using 'groupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsGroupNames :: Lens.Lens' DescribePlacementGroups (Core.Maybe [Types.PlacementGroupName])
dpgsGroupNames = Lens.field @"groupNames"
{-# INLINEABLE dpgsGroupNames #-}
{-# DEPRECATED groupNames "Use generic-lens or generic-optics with 'groupNames' instead"  #-}

instance Core.ToQuery DescribePlacementGroups where
        toQuery DescribePlacementGroups{..}
          = Core.toQueryPair "Action"
              ("DescribePlacementGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "GroupId") groupIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "GroupName") groupNames

instance Core.ToHeaders DescribePlacementGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribePlacementGroups where
        type Rs DescribePlacementGroups = DescribePlacementGroupsResponse
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
                 DescribePlacementGroupsResponse' Core.<$>
                   (x Core..@? "placementGroupSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribePlacementGroupsResponse' smart constructor.
data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse'
  { placementGroups :: Core.Maybe [Types.PlacementGroup]
    -- ^ Information about the placement groups.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePlacementGroupsResponse' value with any optional fields omitted.
mkDescribePlacementGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePlacementGroupsResponse
mkDescribePlacementGroupsResponse responseStatus
  = DescribePlacementGroupsResponse'{placementGroups = Core.Nothing,
                                     responseStatus}

-- | Information about the placement groups.
--
-- /Note:/ Consider using 'placementGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrrsPlacementGroups :: Lens.Lens' DescribePlacementGroupsResponse (Core.Maybe [Types.PlacementGroup])
dpgrrsPlacementGroups = Lens.field @"placementGroups"
{-# INLINEABLE dpgrrsPlacementGroups #-}
{-# DEPRECATED placementGroups "Use generic-lens or generic-optics with 'placementGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrrsResponseStatus :: Lens.Lens' DescribePlacementGroupsResponse Core.Int
dpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
