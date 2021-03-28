{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeletePlacementGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified placement group. You must terminate all instances in the placement group before you can delete the placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DeletePlacementGroup
    (
    -- * Creating a request
      DeletePlacementGroup (..)
    , mkDeletePlacementGroup
    -- ** Request lenses
    , dpgGroupName
    , dpgDryRun

    -- * Destructuring the response
    , DeletePlacementGroupResponse (..)
    , mkDeletePlacementGroupResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePlacementGroup' smart constructor.
data DeletePlacementGroup = DeletePlacementGroup'
  { groupName :: Types.GroupName
    -- ^ The name of the placement group.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePlacementGroup' value with any optional fields omitted.
mkDeletePlacementGroup
    :: Types.GroupName -- ^ 'groupName'
    -> DeletePlacementGroup
mkDeletePlacementGroup groupName
  = DeletePlacementGroup'{groupName, dryRun = Core.Nothing}

-- | The name of the placement group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgGroupName :: Lens.Lens' DeletePlacementGroup Types.GroupName
dpgGroupName = Lens.field @"groupName"
{-# INLINEABLE dpgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgDryRun :: Lens.Lens' DeletePlacementGroup (Core.Maybe Core.Bool)
dpgDryRun = Lens.field @"dryRun"
{-# INLINEABLE dpgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeletePlacementGroup where
        toQuery DeletePlacementGroup{..}
          = Core.toQueryPair "Action" ("DeletePlacementGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "GroupName" groupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeletePlacementGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeletePlacementGroup where
        type Rs DeletePlacementGroup = DeletePlacementGroupResponse
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
        parseResponse = Response.receiveNull DeletePlacementGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePlacementGroupResponse' smart constructor.
data DeletePlacementGroupResponse = DeletePlacementGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePlacementGroupResponse' value with any optional fields omitted.
mkDeletePlacementGroupResponse
    :: DeletePlacementGroupResponse
mkDeletePlacementGroupResponse = DeletePlacementGroupResponse'
