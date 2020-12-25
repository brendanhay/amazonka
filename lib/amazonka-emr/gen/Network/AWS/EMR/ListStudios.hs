{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListStudios
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all Amazon EMR Studios associated with the AWS account. The list includes details such as ID, Studio Access URL, and creation time for each Studio.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListStudios
  ( -- * Creating a request
    ListStudios (..),
    mkListStudios,

    -- ** Request lenses
    lMarker,

    -- * Destructuring the response
    ListStudiosResponse (..),
    mkListStudiosResponse,

    -- ** Response lenses
    lrsMarker,
    lrsStudios,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListStudios' smart constructor.
newtype ListStudios = ListStudios'
  { -- | The pagination token that indicates the set of results to retrieve.
    marker :: Core.Maybe Types.Marker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListStudios' value with any optional fields omitted.
mkListStudios ::
  ListStudios
mkListStudios = ListStudios' {marker = Core.Nothing}

-- | The pagination token that indicates the set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMarker :: Lens.Lens' ListStudios (Core.Maybe Types.Marker)
lMarker = Lens.field @"marker"
{-# DEPRECATED lMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.FromJSON ListStudios where
  toJSON ListStudios {..} =
    Core.object (Core.catMaybes [("Marker" Core..=) Core.<$> marker])

instance Core.AWSRequest ListStudios where
  type Rs ListStudios = ListStudiosResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.ListStudios")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudiosResponse'
            Core.<$> (x Core..:? "Marker")
            Core.<*> (x Core..:? "Studios")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListStudios where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"studios" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkListStudiosResponse' smart constructor.
data ListStudiosResponse = ListStudiosResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Types.Marker,
    -- | The list of Studio summary objects.
    studios :: Core.Maybe [Types.StudioSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListStudiosResponse' value with any optional fields omitted.
mkListStudiosResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListStudiosResponse
mkListStudiosResponse responseStatus =
  ListStudiosResponse'
    { marker = Core.Nothing,
      studios = Core.Nothing,
      responseStatus
    }

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsMarker :: Lens.Lens' ListStudiosResponse (Core.Maybe Types.Marker)
lrsMarker = Lens.field @"marker"
{-# DEPRECATED lrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The list of Studio summary objects.
--
-- /Note:/ Consider using 'studios' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsStudios :: Lens.Lens' ListStudiosResponse (Core.Maybe [Types.StudioSummary])
lrsStudios = Lens.field @"studios"
{-# DEPRECATED lrsStudios "Use generic-lens or generic-optics with 'studios' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListStudiosResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
