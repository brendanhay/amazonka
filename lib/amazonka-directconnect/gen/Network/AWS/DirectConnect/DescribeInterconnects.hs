{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeInterconnects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the interconnects owned by the AWS account or only the specified interconnect.
module Network.AWS.DirectConnect.DescribeInterconnects
  ( -- * Creating a request
    DescribeInterconnects (..),
    mkDescribeInterconnects,

    -- ** Request lenses
    diInterconnectId,

    -- * Destructuring the response
    DescribeInterconnectsResponse (..),
    mkDescribeInterconnectsResponse,

    -- ** Response lenses
    drsInterconnects,
    drsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInterconnects' smart constructor.
newtype DescribeInterconnects = DescribeInterconnects'
  { -- | The ID of the interconnect.
    interconnectId :: Core.Maybe Types.InterconnectId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInterconnects' value with any optional fields omitted.
mkDescribeInterconnects ::
  DescribeInterconnects
mkDescribeInterconnects =
  DescribeInterconnects' {interconnectId = Core.Nothing}

-- | The ID of the interconnect.
--
-- /Note:/ Consider using 'interconnectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInterconnectId :: Lens.Lens' DescribeInterconnects (Core.Maybe Types.InterconnectId)
diInterconnectId = Lens.field @"interconnectId"
{-# DEPRECATED diInterconnectId "Use generic-lens or generic-optics with 'interconnectId' instead." #-}

instance Core.FromJSON DescribeInterconnects where
  toJSON DescribeInterconnects {..} =
    Core.object
      ( Core.catMaybes
          [("interconnectId" Core..=) Core.<$> interconnectId]
      )

instance Core.AWSRequest DescribeInterconnects where
  type Rs DescribeInterconnects = DescribeInterconnectsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.DescribeInterconnects")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInterconnectsResponse'
            Core.<$> (x Core..:? "interconnects")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeInterconnectsResponse' smart constructor.
data DescribeInterconnectsResponse = DescribeInterconnectsResponse'
  { -- | The interconnects.
    interconnects :: Core.Maybe [Types.Interconnect],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeInterconnectsResponse' value with any optional fields omitted.
mkDescribeInterconnectsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInterconnectsResponse
mkDescribeInterconnectsResponse responseStatus =
  DescribeInterconnectsResponse'
    { interconnects = Core.Nothing,
      responseStatus
    }

-- | The interconnects.
--
-- /Note:/ Consider using 'interconnects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsInterconnects :: Lens.Lens' DescribeInterconnectsResponse (Core.Maybe [Types.Interconnect])
drsInterconnects = Lens.field @"interconnects"
{-# DEPRECATED drsInterconnects "Use generic-lens or generic-optics with 'interconnects' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeInterconnectsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
