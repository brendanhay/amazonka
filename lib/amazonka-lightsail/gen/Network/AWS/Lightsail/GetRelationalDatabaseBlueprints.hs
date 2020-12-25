{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available database blueprints in Amazon Lightsail. A blueprint describes the major engine version of a database.
--
-- You can use a blueprint ID to create a new database that runs a specific database engine.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
  ( -- * Creating a request
    GetRelationalDatabaseBlueprints (..),
    mkGetRelationalDatabaseBlueprints,

    -- ** Request lenses
    grdbPageToken,

    -- * Destructuring the response
    GetRelationalDatabaseBlueprintsResponse (..),
    mkGetRelationalDatabaseBlueprintsResponse,

    -- ** Response lenses
    grdbrrsBlueprints,
    grdbrrsNextPageToken,
    grdbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabaseBlueprints' smart constructor.
newtype GetRelationalDatabaseBlueprints = GetRelationalDatabaseBlueprints'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseBlueprints' value with any optional fields omitted.
mkGetRelationalDatabaseBlueprints ::
  GetRelationalDatabaseBlueprints
mkGetRelationalDatabaseBlueprints =
  GetRelationalDatabaseBlueprints' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbPageToken :: Lens.Lens' GetRelationalDatabaseBlueprints (Core.Maybe Types.String)
grdbPageToken = Lens.field @"pageToken"
{-# DEPRECATED grdbPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON GetRelationalDatabaseBlueprints where
  toJSON GetRelationalDatabaseBlueprints {..} =
    Core.object
      (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetRelationalDatabaseBlueprints where
  type
    Rs GetRelationalDatabaseBlueprints =
      GetRelationalDatabaseBlueprintsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.GetRelationalDatabaseBlueprints"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseBlueprintsResponse'
            Core.<$> (x Core..:? "blueprints")
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetRelationalDatabaseBlueprints where
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

-- | /See:/ 'mkGetRelationalDatabaseBlueprintsResponse' smart constructor.
data GetRelationalDatabaseBlueprintsResponse = GetRelationalDatabaseBlueprintsResponse'
  { -- | An object describing the result of your get relational database blueprints request.
    blueprints :: Core.Maybe [Types.RelationalDatabaseBlueprint],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetRelationalDatabaseBlueprints@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseBlueprintsResponse' value with any optional fields omitted.
mkGetRelationalDatabaseBlueprintsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRelationalDatabaseBlueprintsResponse
mkGetRelationalDatabaseBlueprintsResponse responseStatus =
  GetRelationalDatabaseBlueprintsResponse'
    { blueprints =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | An object describing the result of your get relational database blueprints request.
--
-- /Note:/ Consider using 'blueprints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrrsBlueprints :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse (Core.Maybe [Types.RelationalDatabaseBlueprint])
grdbrrsBlueprints = Lens.field @"blueprints"
{-# DEPRECATED grdbrrsBlueprints "Use generic-lens or generic-optics with 'blueprints' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseBlueprints@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrrsNextPageToken :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse (Core.Maybe Types.String)
grdbrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED grdbrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrrsResponseStatus :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse Core.Int
grdbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grdbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
