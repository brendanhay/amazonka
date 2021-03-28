{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetUserDefinedFunction (..)
    , mkGetUserDefinedFunction
    -- ** Request lenses
    , gudffDatabaseName
    , gudffFunctionName
    , gudffCatalogId

    -- * Destructuring the response
    , GetUserDefinedFunctionResponse (..)
    , mkGetUserDefinedFunctionResponse
    -- ** Response lenses
    , gudfrfrsUserDefinedFunction
    , gudfrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUserDefinedFunction' smart constructor.
data GetUserDefinedFunction = GetUserDefinedFunction'
  { databaseName :: Types.DatabaseName
    -- ^ The name of the catalog database where the function is located.
  , functionName :: Types.FunctionName
    -- ^ The name of the function.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog where the function to be retrieved is located. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserDefinedFunction' value with any optional fields omitted.
mkGetUserDefinedFunction
    :: Types.DatabaseName -- ^ 'databaseName'
    -> Types.FunctionName -- ^ 'functionName'
    -> GetUserDefinedFunction
mkGetUserDefinedFunction databaseName functionName
  = GetUserDefinedFunction'{databaseName, functionName,
                            catalogId = Core.Nothing}

-- | The name of the catalog database where the function is located.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudffDatabaseName :: Lens.Lens' GetUserDefinedFunction Types.DatabaseName
gudffDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gudffDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudffFunctionName :: Lens.Lens' GetUserDefinedFunction Types.FunctionName
gudffFunctionName = Lens.field @"functionName"
{-# INLINEABLE gudffFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The ID of the Data Catalog where the function to be retrieved is located. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudffCatalogId :: Lens.Lens' GetUserDefinedFunction (Core.Maybe Types.CatalogId)
gudffCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gudffCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery GetUserDefinedFunction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetUserDefinedFunction where
        toHeaders GetUserDefinedFunction{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetUserDefinedFunction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetUserDefinedFunction where
        toJSON GetUserDefinedFunction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("FunctionName" Core..= functionName),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest GetUserDefinedFunction where
        type Rs GetUserDefinedFunction = GetUserDefinedFunctionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetUserDefinedFunctionResponse' Core.<$>
                   (x Core..:? "UserDefinedFunction") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetUserDefinedFunctionResponse' smart constructor.
data GetUserDefinedFunctionResponse = GetUserDefinedFunctionResponse'
  { userDefinedFunction :: Core.Maybe Types.UserDefinedFunction
    -- ^ The requested function definition.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetUserDefinedFunctionResponse' value with any optional fields omitted.
mkGetUserDefinedFunctionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetUserDefinedFunctionResponse
mkGetUserDefinedFunctionResponse responseStatus
  = GetUserDefinedFunctionResponse'{userDefinedFunction =
                                      Core.Nothing,
                                    responseStatus}

-- | The requested function definition.
--
-- /Note:/ Consider using 'userDefinedFunction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfrfrsUserDefinedFunction :: Lens.Lens' GetUserDefinedFunctionResponse (Core.Maybe Types.UserDefinedFunction)
gudfrfrsUserDefinedFunction = Lens.field @"userDefinedFunction"
{-# INLINEABLE gudfrfrsUserDefinedFunction #-}
{-# DEPRECATED userDefinedFunction "Use generic-lens or generic-optics with 'userDefinedFunction' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfrfrsResponseStatus :: Lens.Lens' GetUserDefinedFunctionResponse Core.Int
gudfrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gudfrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
