{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeLags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all your link aggregation groups (LAG) or the specified LAG.
module Network.AWS.DirectConnect.DescribeLags
  ( -- * Creating a request
    DescribeLags (..),
    mkDescribeLags,

    -- ** Request lenses
    dlLagId,

    -- * Destructuring the response
    DescribeLagsResponse (..),
    mkDescribeLagsResponse,

    -- ** Response lenses
    dlrfrsLags,
    dlrfrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLags' smart constructor.
newtype DescribeLags = DescribeLags'
  { -- | The ID of the LAG.
    lagId :: Core.Maybe Types.LagId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLags' value with any optional fields omitted.
mkDescribeLags ::
  DescribeLags
mkDescribeLags = DescribeLags' {lagId = Core.Nothing}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLagId :: Lens.Lens' DescribeLags (Core.Maybe Types.LagId)
dlLagId = Lens.field @"lagId"
{-# DEPRECATED dlLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

instance Core.FromJSON DescribeLags where
  toJSON DescribeLags {..} =
    Core.object (Core.catMaybes [("lagId" Core..=) Core.<$> lagId])

instance Core.AWSRequest DescribeLags where
  type Rs DescribeLags = DescribeLagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.DescribeLags")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLagsResponse'
            Core.<$> (x Core..:? "lags") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeLagsResponse' smart constructor.
data DescribeLagsResponse = DescribeLagsResponse'
  { -- | The LAGs.
    lags :: Core.Maybe [Types.Lag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeLagsResponse' value with any optional fields omitted.
mkDescribeLagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLagsResponse
mkDescribeLagsResponse responseStatus =
  DescribeLagsResponse' {lags = Core.Nothing, responseStatus}

-- | The LAGs.
--
-- /Note:/ Consider using 'lags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrfrsLags :: Lens.Lens' DescribeLagsResponse (Core.Maybe [Types.Lag])
dlrfrsLags = Lens.field @"lags"
{-# DEPRECATED dlrfrsLags "Use generic-lens or generic-optics with 'lags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrfrsResponseStatus :: Lens.Lens' DescribeLagsResponse Core.Int
dlrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
