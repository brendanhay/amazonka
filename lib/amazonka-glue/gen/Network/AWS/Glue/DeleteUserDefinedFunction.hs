{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteUserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing function definition from the Data Catalog.
module Network.AWS.Glue.DeleteUserDefinedFunction
  ( -- * Creating a request
    DeleteUserDefinedFunction (..),
    mkDeleteUserDefinedFunction,

    -- ** Request lenses
    dudfDatabaseName,
    dudfFunctionName,
    dudfCatalogId,

    -- * Destructuring the response
    DeleteUserDefinedFunctionResponse (..),
    mkDeleteUserDefinedFunctionResponse,

    -- ** Response lenses
    dudfrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUserDefinedFunction' smart constructor.
data DeleteUserDefinedFunction = DeleteUserDefinedFunction'
  { -- | The name of the catalog database where the function is located.
    databaseName :: Types.DatabaseName,
    -- | The name of the function definition to be deleted.
    functionName :: Types.FunctionName,
    -- | The ID of the Data Catalog where the function to be deleted is located. If none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserDefinedFunction' value with any optional fields omitted.
mkDeleteUserDefinedFunction ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'functionName'
  Types.FunctionName ->
  DeleteUserDefinedFunction
mkDeleteUserDefinedFunction databaseName functionName =
  DeleteUserDefinedFunction'
    { databaseName,
      functionName,
      catalogId = Core.Nothing
    }

-- | The name of the catalog database where the function is located.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfDatabaseName :: Lens.Lens' DeleteUserDefinedFunction Types.DatabaseName
dudfDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED dudfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the function definition to be deleted.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfFunctionName :: Lens.Lens' DeleteUserDefinedFunction Types.FunctionName
dudfFunctionName = Lens.field @"functionName"
{-# DEPRECATED dudfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The ID of the Data Catalog where the function to be deleted is located. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfCatalogId :: Lens.Lens' DeleteUserDefinedFunction (Core.Maybe Types.CatalogId)
dudfCatalogId = Lens.field @"catalogId"
{-# DEPRECATED dudfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON DeleteUserDefinedFunction where
  toJSON DeleteUserDefinedFunction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("FunctionName" Core..= functionName),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest DeleteUserDefinedFunction where
  type
    Rs DeleteUserDefinedFunction =
      DeleteUserDefinedFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeleteUserDefinedFunction")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserDefinedFunctionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteUserDefinedFunctionResponse' smart constructor.
newtype DeleteUserDefinedFunctionResponse = DeleteUserDefinedFunctionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserDefinedFunctionResponse' value with any optional fields omitted.
mkDeleteUserDefinedFunctionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteUserDefinedFunctionResponse
mkDeleteUserDefinedFunctionResponse responseStatus =
  DeleteUserDefinedFunctionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfrrsResponseStatus :: Lens.Lens' DeleteUserDefinedFunctionResponse Core.Int
dudfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dudfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
