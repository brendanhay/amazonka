{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available log streams for a specific database in Amazon Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
  ( -- * Creating a request
    GetRelationalDatabaseLogStreams (..),
    mkGetRelationalDatabaseLogStreams,

    -- ** Request lenses
    grdlsRelationalDatabaseName,

    -- * Destructuring the response
    GetRelationalDatabaseLogStreamsResponse (..),
    mkGetRelationalDatabaseLogStreamsResponse,

    -- ** Response lenses
    grdlsrrsLogStreams,
    grdlsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabaseLogStreams' smart constructor.
newtype GetRelationalDatabaseLogStreams = GetRelationalDatabaseLogStreams'
  { -- | The name of your database for which to get log streams.
    relationalDatabaseName :: Types.RelationalDatabaseName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseLogStreams' value with any optional fields omitted.
mkGetRelationalDatabaseLogStreams ::
  -- | 'relationalDatabaseName'
  Types.RelationalDatabaseName ->
  GetRelationalDatabaseLogStreams
mkGetRelationalDatabaseLogStreams relationalDatabaseName =
  GetRelationalDatabaseLogStreams' {relationalDatabaseName}

-- | The name of your database for which to get log streams.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlsRelationalDatabaseName :: Lens.Lens' GetRelationalDatabaseLogStreams Types.RelationalDatabaseName
grdlsRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# DEPRECATED grdlsRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Core.FromJSON GetRelationalDatabaseLogStreams where
  toJSON GetRelationalDatabaseLogStreams {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("relationalDatabaseName" Core..= relationalDatabaseName)
          ]
      )

instance Core.AWSRequest GetRelationalDatabaseLogStreams where
  type
    Rs GetRelationalDatabaseLogStreams =
      GetRelationalDatabaseLogStreamsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.GetRelationalDatabaseLogStreams"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseLogStreamsResponse'
            Core.<$> (x Core..:? "logStreams") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRelationalDatabaseLogStreamsResponse' smart constructor.
data GetRelationalDatabaseLogStreamsResponse = GetRelationalDatabaseLogStreamsResponse'
  { -- | An object describing the result of your get relational database log streams request.
    logStreams :: Core.Maybe [Types.String],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseLogStreamsResponse' value with any optional fields omitted.
mkGetRelationalDatabaseLogStreamsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRelationalDatabaseLogStreamsResponse
mkGetRelationalDatabaseLogStreamsResponse responseStatus =
  GetRelationalDatabaseLogStreamsResponse'
    { logStreams =
        Core.Nothing,
      responseStatus
    }

-- | An object describing the result of your get relational database log streams request.
--
-- /Note:/ Consider using 'logStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlsrrsLogStreams :: Lens.Lens' GetRelationalDatabaseLogStreamsResponse (Core.Maybe [Types.String])
grdlsrrsLogStreams = Lens.field @"logStreams"
{-# DEPRECATED grdlsrrsLogStreams "Use generic-lens or generic-optics with 'logStreams' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdlsrrsResponseStatus :: Lens.Lens' GetRelationalDatabaseLogStreamsResponse Core.Int
grdlsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grdlsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
