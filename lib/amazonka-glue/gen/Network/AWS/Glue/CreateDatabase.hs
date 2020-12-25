{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database in a Data Catalog.
module Network.AWS.Glue.CreateDatabase
  ( -- * Creating a request
    CreateDatabase (..),
    mkCreateDatabase,

    -- ** Request lenses
    cdDatabaseInput,
    cdCatalogId,

    -- * Destructuring the response
    CreateDatabaseResponse (..),
    mkCreateDatabaseResponse,

    -- ** Response lenses
    cdrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDatabase' smart constructor.
data CreateDatabase = CreateDatabase'
  { -- | The metadata for the database.
    databaseInput :: Types.DatabaseInput,
    -- | The ID of the Data Catalog in which to create the database. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDatabase' value with any optional fields omitted.
mkCreateDatabase ::
  -- | 'databaseInput'
  Types.DatabaseInput ->
  CreateDatabase
mkCreateDatabase databaseInput =
  CreateDatabase' {databaseInput, catalogId = Core.Nothing}

-- | The metadata for the database.
--
-- /Note:/ Consider using 'databaseInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDatabaseInput :: Lens.Lens' CreateDatabase Types.DatabaseInput
cdDatabaseInput = Lens.field @"databaseInput"
{-# DEPRECATED cdDatabaseInput "Use generic-lens or generic-optics with 'databaseInput' instead." #-}

-- | The ID of the Data Catalog in which to create the database. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCatalogId :: Lens.Lens' CreateDatabase (Core.Maybe Types.CatalogId)
cdCatalogId = Lens.field @"catalogId"
{-# DEPRECATED cdCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON CreateDatabase where
  toJSON CreateDatabase {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseInput" Core..= databaseInput),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest CreateDatabase where
  type Rs CreateDatabase = CreateDatabaseResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CreateDatabase")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDatabaseResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDatabaseResponse' smart constructor.
newtype CreateDatabaseResponse = CreateDatabaseResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDatabaseResponse' value with any optional fields omitted.
mkCreateDatabaseResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDatabaseResponse
mkCreateDatabaseResponse responseStatus =
  CreateDatabaseResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDatabaseResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
