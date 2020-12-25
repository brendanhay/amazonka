{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of your databases in Amazon Lightsail.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabases
  ( -- * Creating a request
    GetRelationalDatabases (..),
    mkGetRelationalDatabases,

    -- ** Request lenses
    grdPageToken,

    -- * Destructuring the response
    GetRelationalDatabasesResponse (..),
    mkGetRelationalDatabasesResponse,

    -- ** Response lenses
    grdrrsNextPageToken,
    grdrrsRelationalDatabases,
    grdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabases' smart constructor.
newtype GetRelationalDatabases = GetRelationalDatabases'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabases@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.PageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabases' value with any optional fields omitted.
mkGetRelationalDatabases ::
  GetRelationalDatabases
mkGetRelationalDatabases =
  GetRelationalDatabases' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabases@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdPageToken :: Lens.Lens' GetRelationalDatabases (Core.Maybe Types.PageToken)
grdPageToken = Lens.field @"pageToken"
{-# DEPRECATED grdPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON GetRelationalDatabases where
  toJSON GetRelationalDatabases {..} =
    Core.object
      (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetRelationalDatabases where
  type Rs GetRelationalDatabases = GetRelationalDatabasesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.GetRelationalDatabases")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabasesResponse'
            Core.<$> (x Core..:? "nextPageToken")
            Core.<*> (x Core..:? "relationalDatabases")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetRelationalDatabases where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"relationalDatabases" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkGetRelationalDatabasesResponse' smart constructor.
data GetRelationalDatabasesResponse = GetRelationalDatabasesResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetRelationalDatabases@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | An object describing the result of your get relational databases request.
    relationalDatabases :: Core.Maybe [Types.RelationalDatabase],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetRelationalDatabasesResponse' value with any optional fields omitted.
mkGetRelationalDatabasesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRelationalDatabasesResponse
mkGetRelationalDatabasesResponse responseStatus =
  GetRelationalDatabasesResponse'
    { nextPageToken = Core.Nothing,
      relationalDatabases = Core.Nothing,
      responseStatus
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabases@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsNextPageToken :: Lens.Lens' GetRelationalDatabasesResponse (Core.Maybe Types.NextPageToken)
grdrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED grdrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An object describing the result of your get relational databases request.
--
-- /Note:/ Consider using 'relationalDatabases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsRelationalDatabases :: Lens.Lens' GetRelationalDatabasesResponse (Core.Maybe [Types.RelationalDatabase])
grdrrsRelationalDatabases = Lens.field @"relationalDatabases"
{-# DEPRECATED grdrrsRelationalDatabases "Use generic-lens or generic-optics with 'relationalDatabases' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrrsResponseStatus :: Lens.Lens' GetRelationalDatabasesResponse Core.Int
grdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
