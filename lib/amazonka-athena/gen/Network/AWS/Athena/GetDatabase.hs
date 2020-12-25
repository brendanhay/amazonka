{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database object for the specfied database and data catalog.
module Network.AWS.Athena.GetDatabase
  ( -- * Creating a request
    GetDatabase (..),
    mkGetDatabase,

    -- ** Request lenses
    gdCatalogName,
    gdDatabaseName,

    -- * Destructuring the response
    GetDatabaseResponse (..),
    mkGetDatabaseResponse,

    -- ** Response lenses
    gdrrsDatabase,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { -- | The name of the data catalog that contains the database to return.
    catalogName :: Types.CatalogName,
    -- | The name of the database to return.
    databaseName :: Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDatabase' value with any optional fields omitted.
mkGetDatabase ::
  -- | 'catalogName'
  Types.CatalogName ->
  -- | 'databaseName'
  Types.NameString ->
  GetDatabase
mkGetDatabase catalogName databaseName =
  GetDatabase' {catalogName, databaseName}

-- | The name of the data catalog that contains the database to return.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdCatalogName :: Lens.Lens' GetDatabase Types.CatalogName
gdCatalogName = Lens.field @"catalogName"
{-# DEPRECATED gdCatalogName "Use generic-lens or generic-optics with 'catalogName' instead." #-}

-- | The name of the database to return.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDatabaseName :: Lens.Lens' GetDatabase Types.NameString
gdDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED gdDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Core.FromJSON GetDatabase where
  toJSON GetDatabase {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CatalogName" Core..= catalogName),
            Core.Just ("DatabaseName" Core..= databaseName)
          ]
      )

instance Core.AWSRequest GetDatabase where
  type Rs GetDatabase = GetDatabaseResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.GetDatabase")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatabaseResponse'
            Core.<$> (x Core..:? "Database") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'
  { -- | The database returned.
    database :: Core.Maybe Types.Database,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDatabaseResponse' value with any optional fields omitted.
mkGetDatabaseResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDatabaseResponse
mkGetDatabaseResponse responseStatus =
  GetDatabaseResponse' {database = Core.Nothing, responseStatus}

-- | The database returned.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDatabase :: Lens.Lens' GetDatabaseResponse (Core.Maybe Types.Database)
gdrrsDatabase = Lens.field @"database"
{-# DEPRECATED gdrrsDatabase "Use generic-lens or generic-optics with 'database' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDatabaseResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
