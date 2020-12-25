{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the patch baseline that should be used for the specified patch group.
module Network.AWS.SSM.GetPatchBaselineForPatchGroup
  ( -- * Creating a request
    GetPatchBaselineForPatchGroup (..),
    mkGetPatchBaselineForPatchGroup,

    -- ** Request lenses
    gpbfpgPatchGroup,
    gpbfpgOperatingSystem,

    -- * Destructuring the response
    GetPatchBaselineForPatchGroupResponse (..),
    mkGetPatchBaselineForPatchGroupResponse,

    -- ** Response lenses
    gpbfpgrrsBaselineId,
    gpbfpgrrsOperatingSystem,
    gpbfpgrrsPatchGroup,
    gpbfpgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetPatchBaselineForPatchGroup' smart constructor.
data GetPatchBaselineForPatchGroup = GetPatchBaselineForPatchGroup'
  { -- | The name of the patch group whose patch baseline should be retrieved.
    patchGroup :: Types.PatchGroup,
    -- | Returns he operating system rule specified for patch groups using the patch baseline.
    operatingSystem :: Core.Maybe Types.OperatingSystem
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPatchBaselineForPatchGroup' value with any optional fields omitted.
mkGetPatchBaselineForPatchGroup ::
  -- | 'patchGroup'
  Types.PatchGroup ->
  GetPatchBaselineForPatchGroup
mkGetPatchBaselineForPatchGroup patchGroup =
  GetPatchBaselineForPatchGroup'
    { patchGroup,
      operatingSystem = Core.Nothing
    }

-- | The name of the patch group whose patch baseline should be retrieved.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgPatchGroup :: Lens.Lens' GetPatchBaselineForPatchGroup Types.PatchGroup
gpbfpgPatchGroup = Lens.field @"patchGroup"
{-# DEPRECATED gpbfpgPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

-- | Returns he operating system rule specified for patch groups using the patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgOperatingSystem :: Lens.Lens' GetPatchBaselineForPatchGroup (Core.Maybe Types.OperatingSystem)
gpbfpgOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED gpbfpgOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

instance Core.FromJSON GetPatchBaselineForPatchGroup where
  toJSON GetPatchBaselineForPatchGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PatchGroup" Core..= patchGroup),
            ("OperatingSystem" Core..=) Core.<$> operatingSystem
          ]
      )

instance Core.AWSRequest GetPatchBaselineForPatchGroup where
  type
    Rs GetPatchBaselineForPatchGroup =
      GetPatchBaselineForPatchGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.GetPatchBaselineForPatchGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPatchBaselineForPatchGroupResponse'
            Core.<$> (x Core..:? "BaselineId")
            Core.<*> (x Core..:? "OperatingSystem")
            Core.<*> (x Core..:? "PatchGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetPatchBaselineForPatchGroupResponse' smart constructor.
data GetPatchBaselineForPatchGroupResponse = GetPatchBaselineForPatchGroupResponse'
  { -- | The ID of the patch baseline that should be used for the patch group.
    baselineId :: Core.Maybe Types.BaselineId,
    -- | The operating system rule specified for patch groups using the patch baseline.
    operatingSystem :: Core.Maybe Types.OperatingSystem,
    -- | The name of the patch group.
    patchGroup :: Core.Maybe Types.PatchGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPatchBaselineForPatchGroupResponse' value with any optional fields omitted.
mkGetPatchBaselineForPatchGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPatchBaselineForPatchGroupResponse
mkGetPatchBaselineForPatchGroupResponse responseStatus =
  GetPatchBaselineForPatchGroupResponse'
    { baselineId = Core.Nothing,
      operatingSystem = Core.Nothing,
      patchGroup = Core.Nothing,
      responseStatus
    }

-- | The ID of the patch baseline that should be used for the patch group.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrrsBaselineId :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Core.Maybe Types.BaselineId)
gpbfpgrrsBaselineId = Lens.field @"baselineId"
{-# DEPRECATED gpbfpgrrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The operating system rule specified for patch groups using the patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrrsOperatingSystem :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Core.Maybe Types.OperatingSystem)
gpbfpgrrsOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED gpbfpgrrsOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | The name of the patch group.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrrsPatchGroup :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Core.Maybe Types.PatchGroup)
gpbfpgrrsPatchGroup = Lens.field @"patchGroup"
{-# DEPRECATED gpbfpgrrsPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrrsResponseStatus :: Lens.Lens' GetPatchBaselineForPatchGroupResponse Core.Int
gpbfpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpbfpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
