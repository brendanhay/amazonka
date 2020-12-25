{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a run.
module Network.AWS.DeviceFarm.GetRun
  ( -- * Creating a request
    GetRun (..),
    mkGetRun,

    -- ** Request lenses
    grArn,

    -- * Destructuring the response
    GetRunResponse (..),
    mkGetRunResponse,

    -- ** Response lenses
    grrrsRun,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get run operation.
--
-- /See:/ 'mkGetRun' smart constructor.
newtype GetRun = GetRun'
  { -- | The run's ARN.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRun' value with any optional fields omitted.
mkGetRun ::
  -- | 'arn'
  Types.Arn ->
  GetRun
mkGetRun arn = GetRun' {arn}

-- | The run's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grArn :: Lens.Lens' GetRun Types.Arn
grArn = Lens.field @"arn"
{-# DEPRECATED grArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON GetRun where
  toJSON GetRun {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetRun where
  type Rs GetRun = GetRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRunResponse'
            Core.<$> (x Core..:? "run") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of a get run request.
--
-- /See:/ 'mkGetRunResponse' smart constructor.
data GetRunResponse = GetRunResponse'
  { -- | The run to get results from.
    run :: Core.Maybe Types.Run,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetRunResponse' value with any optional fields omitted.
mkGetRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRunResponse
mkGetRunResponse responseStatus =
  GetRunResponse' {run = Core.Nothing, responseStatus}

-- | The run to get results from.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRun :: Lens.Lens' GetRunResponse (Core.Maybe Types.Run)
grrrsRun = Lens.field @"run"
{-# DEPRECATED grrrsRun "Use generic-lens or generic-optics with 'run' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRunResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
