{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetDefaultPatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the default patch baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.
--
-- If you do not specify an operating system value, the default patch baseline for Windows is returned.
module Network.AWS.SSM.GetDefaultPatchBaseline
  ( -- * Creating a request
    GetDefaultPatchBaseline (..),
    mkGetDefaultPatchBaseline,

    -- ** Request lenses
    gdpbOperatingSystem,

    -- * Destructuring the response
    GetDefaultPatchBaselineResponse (..),
    mkGetDefaultPatchBaselineResponse,

    -- ** Response lenses
    gdpbrrsBaselineId,
    gdpbrrsOperatingSystem,
    gdpbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetDefaultPatchBaseline' smart constructor.
newtype GetDefaultPatchBaseline = GetDefaultPatchBaseline'
  { -- | Returns the default patch baseline for the specified operating system.
    operatingSystem :: Core.Maybe Types.OperatingSystem
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDefaultPatchBaseline' value with any optional fields omitted.
mkGetDefaultPatchBaseline ::
  GetDefaultPatchBaseline
mkGetDefaultPatchBaseline =
  GetDefaultPatchBaseline' {operatingSystem = Core.Nothing}

-- | Returns the default patch baseline for the specified operating system.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpbOperatingSystem :: Lens.Lens' GetDefaultPatchBaseline (Core.Maybe Types.OperatingSystem)
gdpbOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED gdpbOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

instance Core.FromJSON GetDefaultPatchBaseline where
  toJSON GetDefaultPatchBaseline {..} =
    Core.object
      ( Core.catMaybes
          [("OperatingSystem" Core..=) Core.<$> operatingSystem]
      )

instance Core.AWSRequest GetDefaultPatchBaseline where
  type Rs GetDefaultPatchBaseline = GetDefaultPatchBaselineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetDefaultPatchBaseline")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDefaultPatchBaselineResponse'
            Core.<$> (x Core..:? "BaselineId")
            Core.<*> (x Core..:? "OperatingSystem")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDefaultPatchBaselineResponse' smart constructor.
data GetDefaultPatchBaselineResponse = GetDefaultPatchBaselineResponse'
  { -- | The ID of the default patch baseline.
    baselineId :: Core.Maybe Types.BaselineId,
    -- | The operating system for the returned patch baseline.
    operatingSystem :: Core.Maybe Types.OperatingSystem,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDefaultPatchBaselineResponse' value with any optional fields omitted.
mkGetDefaultPatchBaselineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDefaultPatchBaselineResponse
mkGetDefaultPatchBaselineResponse responseStatus =
  GetDefaultPatchBaselineResponse'
    { baselineId = Core.Nothing,
      operatingSystem = Core.Nothing,
      responseStatus
    }

-- | The ID of the default patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpbrrsBaselineId :: Lens.Lens' GetDefaultPatchBaselineResponse (Core.Maybe Types.BaselineId)
gdpbrrsBaselineId = Lens.field @"baselineId"
{-# DEPRECATED gdpbrrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The operating system for the returned patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpbrrsOperatingSystem :: Lens.Lens' GetDefaultPatchBaselineResponse (Core.Maybe Types.OperatingSystem)
gdpbrrsOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED gdpbrrsOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpbrrsResponseStatus :: Lens.Lens' GetDefaultPatchBaselineResponse Core.Int
gdpbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdpbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
