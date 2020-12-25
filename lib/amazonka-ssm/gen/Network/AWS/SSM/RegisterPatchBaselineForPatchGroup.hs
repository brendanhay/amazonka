{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.RegisterPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a patch baseline for a patch group.
module Network.AWS.SSM.RegisterPatchBaselineForPatchGroup
  ( -- * Creating a request
    RegisterPatchBaselineForPatchGroup (..),
    mkRegisterPatchBaselineForPatchGroup,

    -- ** Request lenses
    rpbfpgBaselineId,
    rpbfpgPatchGroup,

    -- * Destructuring the response
    RegisterPatchBaselineForPatchGroupResponse (..),
    mkRegisterPatchBaselineForPatchGroupResponse,

    -- ** Response lenses
    rpbfpgrrsBaselineId,
    rpbfpgrrsPatchGroup,
    rpbfpgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkRegisterPatchBaselineForPatchGroup' smart constructor.
data RegisterPatchBaselineForPatchGroup = RegisterPatchBaselineForPatchGroup'
  { -- | The ID of the patch baseline to register the patch group with.
    baselineId :: Types.BaselineId,
    -- | The name of the patch group that should be registered with the patch baseline.
    patchGroup :: Types.PatchGroup
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterPatchBaselineForPatchGroup' value with any optional fields omitted.
mkRegisterPatchBaselineForPatchGroup ::
  -- | 'baselineId'
  Types.BaselineId ->
  -- | 'patchGroup'
  Types.PatchGroup ->
  RegisterPatchBaselineForPatchGroup
mkRegisterPatchBaselineForPatchGroup baselineId patchGroup =
  RegisterPatchBaselineForPatchGroup' {baselineId, patchGroup}

-- | The ID of the patch baseline to register the patch group with.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpbfpgBaselineId :: Lens.Lens' RegisterPatchBaselineForPatchGroup Types.BaselineId
rpbfpgBaselineId = Lens.field @"baselineId"
{-# DEPRECATED rpbfpgBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The name of the patch group that should be registered with the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpbfpgPatchGroup :: Lens.Lens' RegisterPatchBaselineForPatchGroup Types.PatchGroup
rpbfpgPatchGroup = Lens.field @"patchGroup"
{-# DEPRECATED rpbfpgPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

instance Core.FromJSON RegisterPatchBaselineForPatchGroup where
  toJSON RegisterPatchBaselineForPatchGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BaselineId" Core..= baselineId),
            Core.Just ("PatchGroup" Core..= patchGroup)
          ]
      )

instance Core.AWSRequest RegisterPatchBaselineForPatchGroup where
  type
    Rs RegisterPatchBaselineForPatchGroup =
      RegisterPatchBaselineForPatchGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.RegisterPatchBaselineForPatchGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterPatchBaselineForPatchGroupResponse'
            Core.<$> (x Core..:? "BaselineId")
            Core.<*> (x Core..:? "PatchGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterPatchBaselineForPatchGroupResponse' smart constructor.
data RegisterPatchBaselineForPatchGroupResponse = RegisterPatchBaselineForPatchGroupResponse'
  { -- | The ID of the patch baseline the patch group was registered with.
    baselineId :: Core.Maybe Types.BaselineId,
    -- | The name of the patch group registered with the patch baseline.
    patchGroup :: Core.Maybe Types.PatchGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterPatchBaselineForPatchGroupResponse' value with any optional fields omitted.
mkRegisterPatchBaselineForPatchGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterPatchBaselineForPatchGroupResponse
mkRegisterPatchBaselineForPatchGroupResponse responseStatus =
  RegisterPatchBaselineForPatchGroupResponse'
    { baselineId =
        Core.Nothing,
      patchGroup = Core.Nothing,
      responseStatus
    }

-- | The ID of the patch baseline the patch group was registered with.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpbfpgrrsBaselineId :: Lens.Lens' RegisterPatchBaselineForPatchGroupResponse (Core.Maybe Types.BaselineId)
rpbfpgrrsBaselineId = Lens.field @"baselineId"
{-# DEPRECATED rpbfpgrrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The name of the patch group registered with the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpbfpgrrsPatchGroup :: Lens.Lens' RegisterPatchBaselineForPatchGroupResponse (Core.Maybe Types.PatchGroup)
rpbfpgrrsPatchGroup = Lens.field @"patchGroup"
{-# DEPRECATED rpbfpgrrsPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpbfpgrrsResponseStatus :: Lens.Lens' RegisterPatchBaselineForPatchGroupResponse Core.Int
rpbfpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rpbfpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
