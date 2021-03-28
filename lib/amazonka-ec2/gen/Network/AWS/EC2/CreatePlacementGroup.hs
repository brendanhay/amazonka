{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreatePlacementGroup (..)
    , mkCreatePlacementGroup
    -- ** Request lenses
    , cpgDryRun
    , cpgGroupName
    , cpgPartitionCount
    , cpgStrategy
    , cpgTagSpecifications

    -- * Destructuring the response
    , CreatePlacementGroupResponse (..)
    , mkCreatePlacementGroupResponse
    -- ** Response lenses
    , cpgrrsPlacementGroup
    , cpgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePlacementGroup' smart constructor.
data CreatePlacementGroup = CreatePlacementGroup'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , groupName :: Core.Maybe Core.Text
    -- ^ A name for the placement group. Must be unique within the scope of your account for the Region.
--
-- Constraints: Up to 255 ASCII characters
  , partitionCount :: Core.Maybe Core.Int
    -- ^ The number of partitions. Valid only when __Strategy__ is set to @partition@ .
  , strategy :: Core.Maybe Types.PlacementStrategy
    -- ^ The placement strategy.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the new placement group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlacementGroup' value with any optional fields omitted.
mkCreatePlacementGroup
    :: CreatePlacementGroup
mkCreatePlacementGroup
  = CreatePlacementGroup'{dryRun = Core.Nothing,
                          groupName = Core.Nothing, partitionCount = Core.Nothing,
                          strategy = Core.Nothing, tagSpecifications = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgDryRun :: Lens.Lens' CreatePlacementGroup (Core.Maybe Core.Bool)
cpgDryRun = Lens.field @"dryRun"
{-# INLINEABLE cpgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | A name for the placement group. Must be unique within the scope of your account for the Region.
--
-- Constraints: Up to 255 ASCII characters
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgGroupName :: Lens.Lens' CreatePlacementGroup (Core.Maybe Core.Text)
cpgGroupName = Lens.field @"groupName"
{-# INLINEABLE cpgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The number of partitions. Valid only when __Strategy__ is set to @partition@ .
--
-- /Note:/ Consider using 'partitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgPartitionCount :: Lens.Lens' CreatePlacementGroup (Core.Maybe Core.Int)
cpgPartitionCount = Lens.field @"partitionCount"
{-# INLINEABLE cpgPartitionCount #-}
{-# DEPRECATED partitionCount "Use generic-lens or generic-optics with 'partitionCount' instead"  #-}

-- | The placement strategy.
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgStrategy :: Lens.Lens' CreatePlacementGroup (Core.Maybe Types.PlacementStrategy)
cpgStrategy = Lens.field @"strategy"
{-# INLINEABLE cpgStrategy #-}
{-# DEPRECATED strategy "Use generic-lens or generic-optics with 'strategy' instead"  #-}

-- | The tags to apply to the new placement group.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgTagSpecifications :: Lens.Lens' CreatePlacementGroup (Core.Maybe [Types.TagSpecification])
cpgTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cpgTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreatePlacementGroup where
        toQuery CreatePlacementGroup{..}
          = Core.toQueryPair "Action" ("CreatePlacementGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupName") groupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PartitionCount")
                partitionCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Strategy") strategy
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreatePlacementGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreatePlacementGroup where
        type Rs CreatePlacementGroup = CreatePlacementGroupResponse
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
                 CreatePlacementGroupResponse' Core.<$>
                   (x Core..@? "placementGroup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePlacementGroupResponse' smart constructor.
data CreatePlacementGroupResponse = CreatePlacementGroupResponse'
  { placementGroup :: Core.Maybe Types.PlacementGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlacementGroupResponse' value with any optional fields omitted.
mkCreatePlacementGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePlacementGroupResponse
mkCreatePlacementGroupResponse responseStatus
  = CreatePlacementGroupResponse'{placementGroup = Core.Nothing,
                                  responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrrsPlacementGroup :: Lens.Lens' CreatePlacementGroupResponse (Core.Maybe Types.PlacementGroup)
cpgrrsPlacementGroup = Lens.field @"placementGroup"
{-# INLINEABLE cpgrrsPlacementGroup #-}
{-# DEPRECATED placementGroup "Use generic-lens or generic-optics with 'placementGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrrsResponseStatus :: Lens.Lens' CreatePlacementGroupResponse Core.Int
cpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
