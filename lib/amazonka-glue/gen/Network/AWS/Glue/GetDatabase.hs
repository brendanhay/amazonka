{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a specified database.
module Network.AWS.Glue.GetDatabase
  ( -- * Creating a request
    GetDatabase (..),
    mkGetDatabase,

    -- ** Request lenses
    gdfName,
    gdfCatalogId,

    -- * Destructuring the response
    GetDatabaseResponse (..),
    mkGetDatabaseResponse,

    -- ** Response lenses
    gdrrsDatabase,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { -- | The name of the database to retrieve. For Hive compatibility, this should be all lowercase.
    name :: Types.NameString,
    -- | The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDatabase' value with any optional fields omitted.
mkGetDatabase ::
  -- | 'name'
  Types.NameString ->
  GetDatabase
mkGetDatabase name = GetDatabase' {name, catalogId = Core.Nothing}

-- | The name of the database to retrieve. For Hive compatibility, this should be all lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdfName :: Lens.Lens' GetDatabase Types.NameString
gdfName = Lens.field @"name"
{-# DEPRECATED gdfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdfCatalogId :: Lens.Lens' GetDatabase (Core.Maybe Types.CatalogId)
gdfCatalogId = Lens.field @"catalogId"
{-# DEPRECATED gdfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON GetDatabase where
  toJSON GetDatabase {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("CatalogId" Core..=) Core.<$> catalogId
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
          Core.pure ("X-Amz-Target", "AWSGlue.GetDatabase")
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
  { -- | The definition of the specified database in the Data Catalog.
    database :: Core.Maybe Types.Database,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDatabaseResponse' value with any optional fields omitted.
mkGetDatabaseResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDatabaseResponse
mkGetDatabaseResponse responseStatus =
  GetDatabaseResponse' {database = Core.Nothing, responseStatus}

-- | The definition of the specified database in the Data Catalog.
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
