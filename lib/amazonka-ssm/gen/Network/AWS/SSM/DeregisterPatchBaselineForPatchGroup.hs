{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeregisterPatchBaselineForPatchGroup (..),
    mkDeregisterPatchBaselineForPatchGroup,

    -- ** Request lenses
    dpbfpgBaselineId,
    dpbfpgPatchGroup,

    -- * Destructuring the response
    DeregisterPatchBaselineForPatchGroupResponse (..),
    mkDeregisterPatchBaselineForPatchGroupResponse,

    -- ** Response lenses
    dpbfpgrrsBaselineId,
    dpbfpgrrsPatchGroup,
    dpbfpgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeregisterPatchBaselineForPatchGroup' smart constructor.
data DeregisterPatchBaselineForPatchGroup = DeregisterPatchBaselineForPatchGroup'
  { -- | The ID of the patch baseline to deregister the patch group from.
    baselineId :: Types.BaselineId,
    -- | The name of the patch group that should be deregistered from the patch baseline.
    patchGroup :: Types.PatchGroup
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterPatchBaselineForPatchGroup' value with any optional fields omitted.
mkDeregisterPatchBaselineForPatchGroup ::
  -- | 'baselineId'
  Types.BaselineId ->
  -- | 'patchGroup'
  Types.PatchGroup ->
  DeregisterPatchBaselineForPatchGroup
mkDeregisterPatchBaselineForPatchGroup baselineId patchGroup =
  DeregisterPatchBaselineForPatchGroup' {baselineId, patchGroup}

-- | The ID of the patch baseline to deregister the patch group from.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgBaselineId :: Lens.Lens' DeregisterPatchBaselineForPatchGroup Types.BaselineId
dpbfpgBaselineId = Lens.field @"baselineId"
{-# DEPRECATED dpbfpgBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The name of the patch group that should be deregistered from the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgPatchGroup :: Lens.Lens' DeregisterPatchBaselineForPatchGroup Types.PatchGroup
dpbfpgPatchGroup = Lens.field @"patchGroup"
{-# DEPRECATED dpbfpgPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

instance Core.FromJSON DeregisterPatchBaselineForPatchGroup where
  toJSON DeregisterPatchBaselineForPatchGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BaselineId" Core..= baselineId),
            Core.Just ("PatchGroup" Core..= patchGroup)
          ]
      )

instance Core.AWSRequest DeregisterPatchBaselineForPatchGroup where
  type
    Rs DeregisterPatchBaselineForPatchGroup =
      DeregisterPatchBaselineForPatchGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DeregisterPatchBaselineForPatchGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterPatchBaselineForPatchGroupResponse'
            Core.<$> (x Core..:? "BaselineId")
            Core.<*> (x Core..:? "PatchGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterPatchBaselineForPatchGroupResponse' smart constructor.
data DeregisterPatchBaselineForPatchGroupResponse = DeregisterPatchBaselineForPatchGroupResponse'
  { -- | The ID of the patch baseline the patch group was deregistered from.
    baselineId :: Core.Maybe Types.BaselineId,
    -- | The name of the patch group deregistered from the patch baseline.
    patchGroup :: Core.Maybe Types.PatchGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterPatchBaselineForPatchGroupResponse' value with any optional fields omitted.
mkDeregisterPatchBaselineForPatchGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterPatchBaselineForPatchGroupResponse
mkDeregisterPatchBaselineForPatchGroupResponse responseStatus =
  DeregisterPatchBaselineForPatchGroupResponse'
    { baselineId =
        Core.Nothing,
      patchGroup = Core.Nothing,
      responseStatus
    }

-- | The ID of the patch baseline the patch group was deregistered from.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgrrsBaselineId :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse (Core.Maybe Types.BaselineId)
dpbfpgrrsBaselineId = Lens.field @"baselineId"
{-# DEPRECATED dpbfpgrrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The name of the patch group deregistered from the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgrrsPatchGroup :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse (Core.Maybe Types.PatchGroup)
dpbfpgrrsPatchGroup = Lens.field @"patchGroup"
{-# DEPRECATED dpbfpgrrsPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgrrsResponseStatus :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse Core.Int
dpbfpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpbfpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
