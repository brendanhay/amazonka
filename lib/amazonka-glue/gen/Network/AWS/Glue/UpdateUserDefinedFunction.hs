{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateUserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing function definition in the Data Catalog.
module Network.AWS.Glue.UpdateUserDefinedFunction
  ( -- * Creating a request
    UpdateUserDefinedFunction (..),
    mkUpdateUserDefinedFunction,

    -- ** Request lenses
    uudfDatabaseName,
    uudfFunctionName,
    uudfFunctionInput,
    uudfCatalogId,

    -- * Destructuring the response
    UpdateUserDefinedFunctionResponse (..),
    mkUpdateUserDefinedFunctionResponse,

    -- ** Response lenses
    uudfrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserDefinedFunction' smart constructor.
data UpdateUserDefinedFunction = UpdateUserDefinedFunction'
  { -- | The name of the catalog database where the function to be updated is located.
    databaseName :: Types.DatabaseName,
    -- | The name of the function.
    functionName :: Types.FunctionName,
    -- | A @FunctionInput@ object that redefines the function in the Data Catalog.
    functionInput :: Types.UserDefinedFunctionInput,
    -- | The ID of the Data Catalog where the function to be updated is located. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserDefinedFunction' value with any optional fields omitted.
mkUpdateUserDefinedFunction ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'functionName'
  Types.FunctionName ->
  -- | 'functionInput'
  Types.UserDefinedFunctionInput ->
  UpdateUserDefinedFunction
mkUpdateUserDefinedFunction databaseName functionName functionInput =
  UpdateUserDefinedFunction'
    { databaseName,
      functionName,
      functionInput,
      catalogId = Core.Nothing
    }

-- | The name of the catalog database where the function to be updated is located.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uudfDatabaseName :: Lens.Lens' UpdateUserDefinedFunction Types.DatabaseName
uudfDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED uudfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uudfFunctionName :: Lens.Lens' UpdateUserDefinedFunction Types.FunctionName
uudfFunctionName = Lens.field @"functionName"
{-# DEPRECATED uudfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | A @FunctionInput@ object that redefines the function in the Data Catalog.
--
-- /Note:/ Consider using 'functionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uudfFunctionInput :: Lens.Lens' UpdateUserDefinedFunction Types.UserDefinedFunctionInput
uudfFunctionInput = Lens.field @"functionInput"
{-# DEPRECATED uudfFunctionInput "Use generic-lens or generic-optics with 'functionInput' instead." #-}

-- | The ID of the Data Catalog where the function to be updated is located. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uudfCatalogId :: Lens.Lens' UpdateUserDefinedFunction (Core.Maybe Types.CatalogId)
uudfCatalogId = Lens.field @"catalogId"
{-# DEPRECATED uudfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON UpdateUserDefinedFunction where
  toJSON UpdateUserDefinedFunction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("FunctionName" Core..= functionName),
            Core.Just ("FunctionInput" Core..= functionInput),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest UpdateUserDefinedFunction where
  type
    Rs UpdateUserDefinedFunction =
      UpdateUserDefinedFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdateUserDefinedFunction")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateUserDefinedFunctionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateUserDefinedFunctionResponse' smart constructor.
newtype UpdateUserDefinedFunctionResponse = UpdateUserDefinedFunctionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserDefinedFunctionResponse' value with any optional fields omitted.
mkUpdateUserDefinedFunctionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateUserDefinedFunctionResponse
mkUpdateUserDefinedFunctionResponse responseStatus =
  UpdateUserDefinedFunctionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uudfrrsResponseStatus :: Lens.Lens' UpdateUserDefinedFunctionResponse Core.Int
uudfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uudfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
