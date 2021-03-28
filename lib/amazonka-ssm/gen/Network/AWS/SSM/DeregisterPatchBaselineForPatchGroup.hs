{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeregisterPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a patch group from a patch baseline.
module Network.AWS.SSM.DeregisterPatchBaselineForPatchGroup
    (
    -- * Creating a request
      DeregisterPatchBaselineForPatchGroup (..)
    , mkDeregisterPatchBaselineForPatchGroup
    -- ** Request lenses
    , dpbfpgBaselineId
    , dpbfpgPatchGroup

    -- * Destructuring the response
    , DeregisterPatchBaselineForPatchGroupResponse (..)
    , mkDeregisterPatchBaselineForPatchGroupResponse
    -- ** Response lenses
    , dpbfpgrrsBaselineId
    , dpbfpgrrsPatchGroup
    , dpbfpgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeregisterPatchBaselineForPatchGroup' smart constructor.
data DeregisterPatchBaselineForPatchGroup = DeregisterPatchBaselineForPatchGroup'
  { baselineId :: Types.BaselineId
    -- ^ The ID of the patch baseline to deregister the patch group from.
  , patchGroup :: Types.PatchGroup
    -- ^ The name of the patch group that should be deregistered from the patch baseline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterPatchBaselineForPatchGroup' value with any optional fields omitted.
mkDeregisterPatchBaselineForPatchGroup
    :: Types.BaselineId -- ^ 'baselineId'
    -> Types.PatchGroup -- ^ 'patchGroup'
    -> DeregisterPatchBaselineForPatchGroup
mkDeregisterPatchBaselineForPatchGroup baselineId patchGroup
  = DeregisterPatchBaselineForPatchGroup'{baselineId, patchGroup}

-- | The ID of the patch baseline to deregister the patch group from.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgBaselineId :: Lens.Lens' DeregisterPatchBaselineForPatchGroup Types.BaselineId
dpbfpgBaselineId = Lens.field @"baselineId"
{-# INLINEABLE dpbfpgBaselineId #-}
{-# DEPRECATED baselineId "Use generic-lens or generic-optics with 'baselineId' instead"  #-}

-- | The name of the patch group that should be deregistered from the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgPatchGroup :: Lens.Lens' DeregisterPatchBaselineForPatchGroup Types.PatchGroup
dpbfpgPatchGroup = Lens.field @"patchGroup"
{-# INLINEABLE dpbfpgPatchGroup #-}
{-# DEPRECATED patchGroup "Use generic-lens or generic-optics with 'patchGroup' instead"  #-}

instance Core.ToQuery DeregisterPatchBaselineForPatchGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterPatchBaselineForPatchGroup where
        toHeaders DeregisterPatchBaselineForPatchGroup{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.DeregisterPatchBaselineForPatchGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterPatchBaselineForPatchGroup where
        toJSON DeregisterPatchBaselineForPatchGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("BaselineId" Core..= baselineId),
                  Core.Just ("PatchGroup" Core..= patchGroup)])

instance Core.AWSRequest DeregisterPatchBaselineForPatchGroup where
        type Rs DeregisterPatchBaselineForPatchGroup =
             DeregisterPatchBaselineForPatchGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeregisterPatchBaselineForPatchGroupResponse' Core.<$>
                   (x Core..:? "BaselineId") Core.<*> x Core..:? "PatchGroup" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterPatchBaselineForPatchGroupResponse' smart constructor.
data DeregisterPatchBaselineForPatchGroupResponse = DeregisterPatchBaselineForPatchGroupResponse'
  { baselineId :: Core.Maybe Types.BaselineId
    -- ^ The ID of the patch baseline the patch group was deregistered from.
  , patchGroup :: Core.Maybe Types.PatchGroup
    -- ^ The name of the patch group deregistered from the patch baseline.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterPatchBaselineForPatchGroupResponse' value with any optional fields omitted.
mkDeregisterPatchBaselineForPatchGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterPatchBaselineForPatchGroupResponse
mkDeregisterPatchBaselineForPatchGroupResponse responseStatus
  = DeregisterPatchBaselineForPatchGroupResponse'{baselineId =
                                                    Core.Nothing,
                                                  patchGroup = Core.Nothing, responseStatus}

-- | The ID of the patch baseline the patch group was deregistered from.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgrrsBaselineId :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse (Core.Maybe Types.BaselineId)
dpbfpgrrsBaselineId = Lens.field @"baselineId"
{-# INLINEABLE dpbfpgrrsBaselineId #-}
{-# DEPRECATED baselineId "Use generic-lens or generic-optics with 'baselineId' instead"  #-}

-- | The name of the patch group deregistered from the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgrrsPatchGroup :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse (Core.Maybe Types.PatchGroup)
dpbfpgrrsPatchGroup = Lens.field @"patchGroup"
{-# INLINEABLE dpbfpgrrsPatchGroup #-}
{-# DEPRECATED patchGroup "Use generic-lens or generic-optics with 'patchGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgrrsResponseStatus :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse Core.Int
dpbfpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpbfpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
