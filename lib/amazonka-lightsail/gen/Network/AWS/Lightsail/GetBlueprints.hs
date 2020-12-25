{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetBlueprints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of available instance images, or /blueprints/ . You can use a blueprint to create a new instance already running a specific operating system, as well as a preinstalled app or development stack. The software each instance is running depends on the blueprint image you choose.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetBlueprints
  ( -- * Creating a request
    GetBlueprints (..),
    mkGetBlueprints,

    -- ** Request lenses
    gbIncludeInactive,
    gbPageToken,

    -- * Destructuring the response
    GetBlueprintsResponse (..),
    mkGetBlueprintsResponse,

    -- ** Response lenses
    gbrrsBlueprints,
    gbrrsNextPageToken,
    gbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBlueprints' smart constructor.
data GetBlueprints = GetBlueprints'
  { -- | A Boolean value indicating whether to include inactive results in your request.
    includeInactive :: Core.Maybe Core.Bool,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBlueprints' value with any optional fields omitted.
mkGetBlueprints ::
  GetBlueprints
mkGetBlueprints =
  GetBlueprints'
    { includeInactive = Core.Nothing,
      pageToken = Core.Nothing
    }

-- | A Boolean value indicating whether to include inactive results in your request.
--
-- /Note:/ Consider using 'includeInactive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbIncludeInactive :: Lens.Lens' GetBlueprints (Core.Maybe Core.Bool)
gbIncludeInactive = Lens.field @"includeInactive"
{-# DEPRECATED gbIncludeInactive "Use generic-lens or generic-optics with 'includeInactive' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbPageToken :: Lens.Lens' GetBlueprints (Core.Maybe Types.String)
gbPageToken = Lens.field @"pageToken"
{-# DEPRECATED gbPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON GetBlueprints where
  toJSON GetBlueprints {..} =
    Core.object
      ( Core.catMaybes
          [ ("includeInactive" Core..=) Core.<$> includeInactive,
            ("pageToken" Core..=) Core.<$> pageToken
          ]
      )

instance Core.AWSRequest GetBlueprints where
  type Rs GetBlueprints = GetBlueprintsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetBlueprints")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlueprintsResponse'
            Core.<$> (x Core..:? "blueprints")
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetBlueprints where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"blueprints" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkGetBlueprintsResponse' smart constructor.
data GetBlueprintsResponse = GetBlueprintsResponse'
  { -- | An array of key-value pairs that contains information about the available blueprints.
    blueprints :: Core.Maybe [Types.Blueprint],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetBlueprints@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBlueprintsResponse' value with any optional fields omitted.
mkGetBlueprintsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBlueprintsResponse
mkGetBlueprintsResponse responseStatus =
  GetBlueprintsResponse'
    { blueprints = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | An array of key-value pairs that contains information about the available blueprints.
--
-- /Note:/ Consider using 'blueprints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsBlueprints :: Lens.Lens' GetBlueprintsResponse (Core.Maybe [Types.Blueprint])
gbrrsBlueprints = Lens.field @"blueprints"
{-# DEPRECATED gbrrsBlueprints "Use generic-lens or generic-optics with 'blueprints' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetBlueprints@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsNextPageToken :: Lens.Lens' GetBlueprintsResponse (Core.Maybe Types.String)
gbrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gbrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsResponseStatus :: Lens.Lens' GetBlueprintsResponse Core.Int
gbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
