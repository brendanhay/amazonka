{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreatePlacementGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a placement group in which to launch instances. The strategy of the placement group determines how the instances are organized within the group.
--
-- A @cluster@ placement group is a logical grouping of instances within a single Availability Zone that benefit from low network latency, high network throughput. A @spread@ placement group places instances on distinct hardware. A @partition@ placement group places groups of instances in different partitions, where instances in one partition do not share the same hardware with instances in another partition.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreatePlacementGroup
  ( -- * Creating a request
    CreatePlacementGroup (..),
    mkCreatePlacementGroup,

    -- ** Request lenses
    cpgDryRun,
    cpgGroupName,
    cpgPartitionCount,
    cpgStrategy,
    cpgTagSpecifications,

    -- * Destructuring the response
    CreatePlacementGroupResponse (..),
    mkCreatePlacementGroupResponse,

    -- ** Response lenses
    cpgrrsPlacementGroup,
    cpgrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePlacementGroup' smart constructor.
data CreatePlacementGroup = CreatePlacementGroup'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | A name for the placement group. Must be unique within the scope of your account for the Region.
    --
    -- Constraints: Up to 255 ASCII characters
    groupName :: Core.Maybe Types.String,
    -- | The number of partitions. Valid only when __Strategy__ is set to @partition@ .
    partitionCount :: Core.Maybe Core.Int,
    -- | The placement strategy.
    strategy :: Core.Maybe Types.PlacementStrategy,
    -- | The tags to apply to the new placement group.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlacementGroup' value with any optional fields omitted.
mkCreatePlacementGroup ::
  CreatePlacementGroup
mkCreatePlacementGroup =
  CreatePlacementGroup'
    { dryRun = Core.Nothing,
      groupName = Core.Nothing,
      partitionCount = Core.Nothing,
      strategy = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgDryRun :: Lens.Lens' CreatePlacementGroup (Core.Maybe Core.Bool)
cpgDryRun = Lens.field @"dryRun"
{-# DEPRECATED cpgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | A name for the placement group. Must be unique within the scope of your account for the Region.
--
-- Constraints: Up to 255 ASCII characters
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgGroupName :: Lens.Lens' CreatePlacementGroup (Core.Maybe Types.String)
cpgGroupName = Lens.field @"groupName"
{-# DEPRECATED cpgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The number of partitions. Valid only when __Strategy__ is set to @partition@ .
--
-- /Note:/ Consider using 'partitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgPartitionCount :: Lens.Lens' CreatePlacementGroup (Core.Maybe Core.Int)
cpgPartitionCount = Lens.field @"partitionCount"
{-# DEPRECATED cpgPartitionCount "Use generic-lens or generic-optics with 'partitionCount' instead." #-}

-- | The placement strategy.
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgStrategy :: Lens.Lens' CreatePlacementGroup (Core.Maybe Types.PlacementStrategy)
cpgStrategy = Lens.field @"strategy"
{-# DEPRECATED cpgStrategy "Use generic-lens or generic-optics with 'strategy' instead." #-}

-- | The tags to apply to the new placement group.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgTagSpecifications :: Lens.Lens' CreatePlacementGroup (Core.Maybe [Types.TagSpecification])
cpgTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED cpgTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreatePlacementGroup where
  type Rs CreatePlacementGroup = CreatePlacementGroupResponse
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
            ( Core.pure ("Action", "CreatePlacementGroup")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "GroupName" Core.<$> groupName)
                Core.<> (Core.toQueryValue "PartitionCount" Core.<$> partitionCount)
                Core.<> (Core.toQueryValue "Strategy" Core.<$> strategy)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreatePlacementGroupResponse'
            Core.<$> (x Core..@? "placementGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePlacementGroupResponse' smart constructor.
data CreatePlacementGroupResponse = CreatePlacementGroupResponse'
  { placementGroup :: Core.Maybe Types.PlacementGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlacementGroupResponse' value with any optional fields omitted.
mkCreatePlacementGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePlacementGroupResponse
mkCreatePlacementGroupResponse responseStatus =
  CreatePlacementGroupResponse'
    { placementGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrrsPlacementGroup :: Lens.Lens' CreatePlacementGroupResponse (Core.Maybe Types.PlacementGroup)
cpgrrsPlacementGroup = Lens.field @"placementGroup"
{-# DEPRECATED cpgrrsPlacementGroup "Use generic-lens or generic-optics with 'placementGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrrsResponseStatus :: Lens.Lens' CreatePlacementGroupResponse Core.Int
cpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
