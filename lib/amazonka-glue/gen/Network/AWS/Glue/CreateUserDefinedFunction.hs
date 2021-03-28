{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateUserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new function definition in the Data Catalog.
module Network.AWS.Glue.CreateUserDefinedFunction
    (
    -- * Creating a request
      CreateUserDefinedFunction (..)
    , mkCreateUserDefinedFunction
    -- ** Request lenses
    , cudfDatabaseName
    , cudfFunctionInput
    , cudfCatalogId

    -- * Destructuring the response
    , CreateUserDefinedFunctionResponse (..)
    , mkCreateUserDefinedFunctionResponse
    -- ** Response lenses
    , cudfrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUserDefinedFunction' smart constructor.
data CreateUserDefinedFunction = CreateUserDefinedFunction'
  { databaseName :: Types.DatabaseName
    -- ^ The name of the catalog database in which to create the function.
  , functionInput :: Types.UserDefinedFunctionInput
    -- ^ A @FunctionInput@ object that defines the function to create in the Data Catalog.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog in which to create the function. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserDefinedFunction' value with any optional fields omitted.
mkCreateUserDefinedFunction
    :: Types.DatabaseName -- ^ 'databaseName'
    -> Types.UserDefinedFunctionInput -- ^ 'functionInput'
    -> CreateUserDefinedFunction
mkCreateUserDefinedFunction databaseName functionInput
  = CreateUserDefinedFunction'{databaseName, functionInput,
                               catalogId = Core.Nothing}

-- | The name of the catalog database in which to create the function.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cudfDatabaseName :: Lens.Lens' CreateUserDefinedFunction Types.DatabaseName
cudfDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE cudfDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | A @FunctionInput@ object that defines the function to create in the Data Catalog.
--
-- /Note:/ Consider using 'functionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cudfFunctionInput :: Lens.Lens' CreateUserDefinedFunction Types.UserDefinedFunctionInput
cudfFunctionInput = Lens.field @"functionInput"
{-# INLINEABLE cudfFunctionInput #-}
{-# DEPRECATED functionInput "Use generic-lens or generic-optics with 'functionInput' instead"  #-}

-- | The ID of the Data Catalog in which to create the function. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cudfCatalogId :: Lens.Lens' CreateUserDefinedFunction (Core.Maybe Types.CatalogId)
cudfCatalogId = Lens.field @"catalogId"
{-# INLINEABLE cudfCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery CreateUserDefinedFunction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateUserDefinedFunction where
        toHeaders CreateUserDefinedFunction{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.CreateUserDefinedFunction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateUserDefinedFunction where
        toJSON CreateUserDefinedFunction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("FunctionInput" Core..= functionInput),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest CreateUserDefinedFunction where
        type Rs CreateUserDefinedFunction =
             CreateUserDefinedFunctionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateUserDefinedFunctionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateUserDefinedFunctionResponse' smart constructor.
newtype CreateUserDefinedFunctionResponse = CreateUserDefinedFunctionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserDefinedFunctionResponse' value with any optional fields omitted.
mkCreateUserDefinedFunctionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateUserDefinedFunctionResponse
mkCreateUserDefinedFunctionResponse responseStatus
  = CreateUserDefinedFunctionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cudfrrsResponseStatus :: Lens.Lens' CreateUserDefinedFunctionResponse Core.Int
cudfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cudfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
