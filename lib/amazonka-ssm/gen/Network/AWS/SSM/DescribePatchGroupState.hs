{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribePatchGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns high-level aggregated patch compliance state for a patch group.
module Network.AWS.SSM.DescribePatchGroupState
    (
    -- * Creating a request
      DescribePatchGroupState (..)
    , mkDescribePatchGroupState
    -- ** Request lenses
    , dpgsPatchGroup

    -- * Destructuring the response
    , DescribePatchGroupStateResponse (..)
    , mkDescribePatchGroupStateResponse
    -- ** Response lenses
    , dpgsrrsInstances
    , dpgsrrsInstancesWithFailedPatches
    , dpgsrrsInstancesWithInstalledOtherPatches
    , dpgsrrsInstancesWithInstalledPatches
    , dpgsrrsInstancesWithInstalledPendingRebootPatches
    , dpgsrrsInstancesWithInstalledRejectedPatches
    , dpgsrrsInstancesWithMissingPatches
    , dpgsrrsInstancesWithNotApplicablePatches
    , dpgsrrsInstancesWithUnreportedNotApplicablePatches
    , dpgsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribePatchGroupState' smart constructor.
newtype DescribePatchGroupState = DescribePatchGroupState'
  { patchGroup :: Types.PatchGroup
    -- ^ The name of the patch group whose patch snapshot should be retrieved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePatchGroupState' value with any optional fields omitted.
mkDescribePatchGroupState
    :: Types.PatchGroup -- ^ 'patchGroup'
    -> DescribePatchGroupState
mkDescribePatchGroupState patchGroup
  = DescribePatchGroupState'{patchGroup}

-- | The name of the patch group whose patch snapshot should be retrieved.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsPatchGroup :: Lens.Lens' DescribePatchGroupState Types.PatchGroup
dpgsPatchGroup = Lens.field @"patchGroup"
{-# INLINEABLE dpgsPatchGroup #-}
{-# DEPRECATED patchGroup "Use generic-lens or generic-optics with 'patchGroup' instead"  #-}

instance Core.ToQuery DescribePatchGroupState where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePatchGroupState where
        toHeaders DescribePatchGroupState{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DescribePatchGroupState")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribePatchGroupState where
        toJSON DescribePatchGroupState{..}
          = Core.object
              (Core.catMaybes [Core.Just ("PatchGroup" Core..= patchGroup)])

instance Core.AWSRequest DescribePatchGroupState where
        type Rs DescribePatchGroupState = DescribePatchGroupStateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePatchGroupStateResponse' Core.<$>
                   (x Core..:? "Instances") Core.<*>
                     x Core..:? "InstancesWithFailedPatches"
                     Core.<*> x Core..:? "InstancesWithInstalledOtherPatches"
                     Core.<*> x Core..:? "InstancesWithInstalledPatches"
                     Core.<*> x Core..:? "InstancesWithInstalledPendingRebootPatches"
                     Core.<*> x Core..:? "InstancesWithInstalledRejectedPatches"
                     Core.<*> x Core..:? "InstancesWithMissingPatches"
                     Core.<*> x Core..:? "InstancesWithNotApplicablePatches"
                     Core.<*> x Core..:? "InstancesWithUnreportedNotApplicablePatches"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribePatchGroupStateResponse' smart constructor.
data DescribePatchGroupStateResponse = DescribePatchGroupStateResponse'
  { instances :: Core.Maybe Core.Int
    -- ^ The number of instances in the patch group.
  , instancesWithFailedPatches :: Core.Maybe Core.Int
    -- ^ The number of instances with patches from the patch baseline that failed to install.
  , instancesWithInstalledOtherPatches :: Core.Maybe Core.Int
    -- ^ The number of instances with patches installed that aren't defined in the patch baseline.
  , instancesWithInstalledPatches :: Core.Maybe Core.Int
    -- ^ The number of instances with installed patches.
  , instancesWithInstalledPendingRebootPatches :: Core.Maybe Core.Int
    -- ^ The number of instances with patches installed by Patch Manager that have not been rebooted after the patch installation. The status of these instances is NON_COMPLIANT.
  , instancesWithInstalledRejectedPatches :: Core.Maybe Core.Int
    -- ^ The number of instances with patches installed that are specified in a RejectedPatches list. Patches with a status of /INSTALLED_REJECTED/ were typically installed before they were added to a RejectedPatches list.
  , instancesWithMissingPatches :: Core.Maybe Core.Int
    -- ^ The number of instances with missing patches from the patch baseline.
  , instancesWithNotApplicablePatches :: Core.Maybe Core.Int
    -- ^ The number of instances with patches that aren't applicable.
  , instancesWithUnreportedNotApplicablePatches :: Core.Maybe Core.Int
    -- ^ The number of instances with @NotApplicable@ patches beyond the supported limit, which are not reported by name to Systems Manager Inventory.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePatchGroupStateResponse' value with any optional fields omitted.
mkDescribePatchGroupStateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePatchGroupStateResponse
mkDescribePatchGroupStateResponse responseStatus
  = DescribePatchGroupStateResponse'{instances = Core.Nothing,
                                     instancesWithFailedPatches = Core.Nothing,
                                     instancesWithInstalledOtherPatches = Core.Nothing,
                                     instancesWithInstalledPatches = Core.Nothing,
                                     instancesWithInstalledPendingRebootPatches = Core.Nothing,
                                     instancesWithInstalledRejectedPatches = Core.Nothing,
                                     instancesWithMissingPatches = Core.Nothing,
                                     instancesWithNotApplicablePatches = Core.Nothing,
                                     instancesWithUnreportedNotApplicablePatches = Core.Nothing,
                                     responseStatus}

-- | The number of instances in the patch group.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrrsInstances :: Lens.Lens' DescribePatchGroupStateResponse (Core.Maybe Core.Int)
dpgsrrsInstances = Lens.field @"instances"
{-# INLINEABLE dpgsrrsInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | The number of instances with patches from the patch baseline that failed to install.
--
-- /Note:/ Consider using 'instancesWithFailedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrrsInstancesWithFailedPatches :: Lens.Lens' DescribePatchGroupStateResponse (Core.Maybe Core.Int)
dpgsrrsInstancesWithFailedPatches = Lens.field @"instancesWithFailedPatches"
{-# INLINEABLE dpgsrrsInstancesWithFailedPatches #-}
{-# DEPRECATED instancesWithFailedPatches "Use generic-lens or generic-optics with 'instancesWithFailedPatches' instead"  #-}

-- | The number of instances with patches installed that aren't defined in the patch baseline.
--
-- /Note:/ Consider using 'instancesWithInstalledOtherPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrrsInstancesWithInstalledOtherPatches :: Lens.Lens' DescribePatchGroupStateResponse (Core.Maybe Core.Int)
dpgsrrsInstancesWithInstalledOtherPatches = Lens.field @"instancesWithInstalledOtherPatches"
{-# INLINEABLE dpgsrrsInstancesWithInstalledOtherPatches #-}
{-# DEPRECATED instancesWithInstalledOtherPatches "Use generic-lens or generic-optics with 'instancesWithInstalledOtherPatches' instead"  #-}

-- | The number of instances with installed patches.
--
-- /Note:/ Consider using 'instancesWithInstalledPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrrsInstancesWithInstalledPatches :: Lens.Lens' DescribePatchGroupStateResponse (Core.Maybe Core.Int)
dpgsrrsInstancesWithInstalledPatches = Lens.field @"instancesWithInstalledPatches"
{-# INLINEABLE dpgsrrsInstancesWithInstalledPatches #-}
{-# DEPRECATED instancesWithInstalledPatches "Use generic-lens or generic-optics with 'instancesWithInstalledPatches' instead"  #-}

-- | The number of instances with patches installed by Patch Manager that have not been rebooted after the patch installation. The status of these instances is NON_COMPLIANT.
--
-- /Note:/ Consider using 'instancesWithInstalledPendingRebootPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrrsInstancesWithInstalledPendingRebootPatches :: Lens.Lens' DescribePatchGroupStateResponse (Core.Maybe Core.Int)
dpgsrrsInstancesWithInstalledPendingRebootPatches = Lens.field @"instancesWithInstalledPendingRebootPatches"
{-# INLINEABLE dpgsrrsInstancesWithInstalledPendingRebootPatches #-}
{-# DEPRECATED instancesWithInstalledPendingRebootPatches "Use generic-lens or generic-optics with 'instancesWithInstalledPendingRebootPatches' instead"  #-}

-- | The number of instances with patches installed that are specified in a RejectedPatches list. Patches with a status of /INSTALLED_REJECTED/ were typically installed before they were added to a RejectedPatches list.
--
-- /Note:/ Consider using 'instancesWithInstalledRejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrrsInstancesWithInstalledRejectedPatches :: Lens.Lens' DescribePatchGroupStateResponse (Core.Maybe Core.Int)
dpgsrrsInstancesWithInstalledRejectedPatches = Lens.field @"instancesWithInstalledRejectedPatches"
{-# INLINEABLE dpgsrrsInstancesWithInstalledRejectedPatches #-}
{-# DEPRECATED instancesWithInstalledRejectedPatches "Use generic-lens or generic-optics with 'instancesWithInstalledRejectedPatches' instead"  #-}

-- | The number of instances with missing patches from the patch baseline.
--
-- /Note:/ Consider using 'instancesWithMissingPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrrsInstancesWithMissingPatches :: Lens.Lens' DescribePatchGroupStateResponse (Core.Maybe Core.Int)
dpgsrrsInstancesWithMissingPatches = Lens.field @"instancesWithMissingPatches"
{-# INLINEABLE dpgsrrsInstancesWithMissingPatches #-}
{-# DEPRECATED instancesWithMissingPatches "Use generic-lens or generic-optics with 'instancesWithMissingPatches' instead"  #-}

-- | The number of instances with patches that aren't applicable.
--
-- /Note:/ Consider using 'instancesWithNotApplicablePatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrrsInstancesWithNotApplicablePatches :: Lens.Lens' DescribePatchGroupStateResponse (Core.Maybe Core.Int)
dpgsrrsInstancesWithNotApplicablePatches = Lens.field @"instancesWithNotApplicablePatches"
{-# INLINEABLE dpgsrrsInstancesWithNotApplicablePatches #-}
{-# DEPRECATED instancesWithNotApplicablePatches "Use generic-lens or generic-optics with 'instancesWithNotApplicablePatches' instead"  #-}

-- | The number of instances with @NotApplicable@ patches beyond the supported limit, which are not reported by name to Systems Manager Inventory.
--
-- /Note:/ Consider using 'instancesWithUnreportedNotApplicablePatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrrsInstancesWithUnreportedNotApplicablePatches :: Lens.Lens' DescribePatchGroupStateResponse (Core.Maybe Core.Int)
dpgsrrsInstancesWithUnreportedNotApplicablePatches = Lens.field @"instancesWithUnreportedNotApplicablePatches"
{-# INLINEABLE dpgsrrsInstancesWithUnreportedNotApplicablePatches #-}
{-# DEPRECATED instancesWithUnreportedNotApplicablePatches "Use generic-lens or generic-optics with 'instancesWithUnreportedNotApplicablePatches' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrrsResponseStatus :: Lens.Lens' DescribePatchGroupStateResponse Core.Int
dpgsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpgsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
