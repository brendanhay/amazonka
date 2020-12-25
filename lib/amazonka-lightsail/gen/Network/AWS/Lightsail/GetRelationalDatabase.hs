{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific database in Amazon Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabase
  ( -- * Creating a request
    GetRelationalDatabase (..),
    mkGetRelationalDatabase,

    -- ** Request lenses
    grdRelationalDatabaseName,

    -- * Destructuring the response
    GetRelationalDatabaseResponse (..),
    mkGetRelationalDatabaseResponse,

    -- ** Response lenses
    grdrfrsRelationalDatabase,
    grdrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabase' smart constructor.
newtype GetRelationalDatabase = GetRelationalDatabase'
  { -- | The name of the database that you are looking up.
    relationalDatabaseName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabase' value with any optional fields omitted.
mkGetRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Types.ResourceName ->
  GetRelationalDatabase
mkGetRelationalDatabase relationalDatabaseName =
  GetRelationalDatabase' {relationalDatabaseName}

-- | The name of the database that you are looking up.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdRelationalDatabaseName :: Lens.Lens' GetRelationalDatabase Types.ResourceName
grdRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# DEPRECATED grdRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Core.FromJSON GetRelationalDatabase where
  toJSON GetRelationalDatabase {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("relationalDatabaseName" Core..= relationalDatabaseName)
          ]
      )

instance Core.AWSRequest GetRelationalDatabase where
  type Rs GetRelationalDatabase = GetRelationalDatabaseResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.GetRelationalDatabase")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseResponse'
            Core.<$> (x Core..:? "relationalDatabase")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRelationalDatabaseResponse' smart constructor.
data GetRelationalDatabaseResponse = GetRelationalDatabaseResponse'
  { -- | An object describing the specified database.
    relationalDatabase :: Core.Maybe Types.RelationalDatabase,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetRelationalDatabaseResponse' value with any optional fields omitted.
mkGetRelationalDatabaseResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRelationalDatabaseResponse
mkGetRelationalDatabaseResponse responseStatus =
  GetRelationalDatabaseResponse'
    { relationalDatabase = Core.Nothing,
      responseStatus
    }

-- | An object describing the specified database.
--
-- /Note:/ Consider using 'relationalDatabase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrfrsRelationalDatabase :: Lens.Lens' GetRelationalDatabaseResponse (Core.Maybe Types.RelationalDatabase)
grdrfrsRelationalDatabase = Lens.field @"relationalDatabase"
{-# DEPRECATED grdrfrsRelationalDatabase "Use generic-lens or generic-optics with 'relationalDatabase' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrfrsResponseStatus :: Lens.Lens' GetRelationalDatabaseResponse Core.Int
grdrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grdrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
