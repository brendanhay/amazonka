{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetUserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified function definition from the Data Catalog.
module Network.AWS.Glue.GetUserDefinedFunction
  ( -- * Creating a request
    GetUserDefinedFunction (..),
    mkGetUserDefinedFunction,

    -- ** Request lenses
    gudffDatabaseName,
    gudffFunctionName,
    gudffCatalogId,

    -- * Destructuring the response
    GetUserDefinedFunctionResponse (..),
    mkGetUserDefinedFunctionResponse,

    -- ** Response lenses
    gudfrfrsUserDefinedFunction,
    gudfrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUserDefinedFunction' smart constructor.
data GetUserDefinedFunction = GetUserDefinedFunction'
  { -- | The name of the catalog database where the function is located.
    databaseName :: Types.DatabaseName,
    -- | The name of the function.
    functionName :: Types.FunctionName,
    -- | The ID of the Data Catalog where the function to be retrieved is located. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserDefinedFunction' value with any optional fields omitted.
mkGetUserDefinedFunction ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'functionName'
  Types.FunctionName ->
  GetUserDefinedFunction
mkGetUserDefinedFunction databaseName functionName =
  GetUserDefinedFunction'
    { databaseName,
      functionName,
      catalogId = Core.Nothing
    }

-- | The name of the catalog database where the function is located.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudffDatabaseName :: Lens.Lens' GetUserDefinedFunction Types.DatabaseName
gudffDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED gudffDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudffFunctionName :: Lens.Lens' GetUserDefinedFunction Types.FunctionName
gudffFunctionName = Lens.field @"functionName"
{-# DEPRECATED gudffFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The ID of the Data Catalog where the function to be retrieved is located. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudffCatalogId :: Lens.Lens' GetUserDefinedFunction (Core.Maybe Types.CatalogId)
gudffCatalogId = Lens.field @"catalogId"
{-# DEPRECATED gudffCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON GetUserDefinedFunction where
  toJSON GetUserDefinedFunction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("FunctionName" Core..= functionName),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest GetUserDefinedFunction where
  type Rs GetUserDefinedFunction = GetUserDefinedFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetUserDefinedFunction")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserDefinedFunctionResponse'
            Core.<$> (x Core..:? "UserDefinedFunction")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetUserDefinedFunctionResponse' smart constructor.
data GetUserDefinedFunctionResponse = GetUserDefinedFunctionResponse'
  { -- | The requested function definition.
    userDefinedFunction :: Core.Maybe Types.UserDefinedFunction,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetUserDefinedFunctionResponse' value with any optional fields omitted.
mkGetUserDefinedFunctionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetUserDefinedFunctionResponse
mkGetUserDefinedFunctionResponse responseStatus =
  GetUserDefinedFunctionResponse'
    { userDefinedFunction =
        Core.Nothing,
      responseStatus
    }

-- | The requested function definition.
--
-- /Note:/ Consider using 'userDefinedFunction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfrfrsUserDefinedFunction :: Lens.Lens' GetUserDefinedFunctionResponse (Core.Maybe Types.UserDefinedFunction)
gudfrfrsUserDefinedFunction = Lens.field @"userDefinedFunction"
{-# DEPRECATED gudfrfrsUserDefinedFunction "Use generic-lens or generic-optics with 'userDefinedFunction' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfrfrsResponseStatus :: Lens.Lens' GetUserDefinedFunctionResponse Core.Int
gudfrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gudfrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
