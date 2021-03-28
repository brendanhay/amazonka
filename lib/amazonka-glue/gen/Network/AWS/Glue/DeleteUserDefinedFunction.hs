{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteUserDefinedFunction (..)
    , mkDeleteUserDefinedFunction
    -- ** Request lenses
    , dudfDatabaseName
    , dudfFunctionName
    , dudfCatalogId

    -- * Destructuring the response
    , DeleteUserDefinedFunctionResponse (..)
    , mkDeleteUserDefinedFunctionResponse
    -- ** Response lenses
    , dudfrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUserDefinedFunction' smart constructor.
data DeleteUserDefinedFunction = DeleteUserDefinedFunction'
  { databaseName :: Types.DatabaseName
    -- ^ The name of the catalog database where the function is located.
  , functionName :: Types.FunctionName
    -- ^ The name of the function definition to be deleted.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog where the function to be deleted is located. If none is supplied, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserDefinedFunction' value with any optional fields omitted.
mkDeleteUserDefinedFunction
    :: Types.DatabaseName -- ^ 'databaseName'
    -> Types.FunctionName -- ^ 'functionName'
    -> DeleteUserDefinedFunction
mkDeleteUserDefinedFunction databaseName functionName
  = DeleteUserDefinedFunction'{databaseName, functionName,
                               catalogId = Core.Nothing}

-- | The name of the catalog database where the function is located.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfDatabaseName :: Lens.Lens' DeleteUserDefinedFunction Types.DatabaseName
dudfDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE dudfDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the function definition to be deleted.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfFunctionName :: Lens.Lens' DeleteUserDefinedFunction Types.FunctionName
dudfFunctionName = Lens.field @"functionName"
{-# INLINEABLE dudfFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The ID of the Data Catalog where the function to be deleted is located. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfCatalogId :: Lens.Lens' DeleteUserDefinedFunction (Core.Maybe Types.CatalogId)
dudfCatalogId = Lens.field @"catalogId"
{-# INLINEABLE dudfCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery DeleteUserDefinedFunction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUserDefinedFunction where
        toHeaders DeleteUserDefinedFunction{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteUserDefinedFunction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteUserDefinedFunction where
        toJSON DeleteUserDefinedFunction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("FunctionName" Core..= functionName),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest DeleteUserDefinedFunction where
        type Rs DeleteUserDefinedFunction =
             DeleteUserDefinedFunctionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteUserDefinedFunctionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUserDefinedFunctionResponse' smart constructor.
newtype DeleteUserDefinedFunctionResponse = DeleteUserDefinedFunctionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserDefinedFunctionResponse' value with any optional fields omitted.
mkDeleteUserDefinedFunctionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteUserDefinedFunctionResponse
mkDeleteUserDefinedFunctionResponse responseStatus
  = DeleteUserDefinedFunctionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dudfrrsResponseStatus :: Lens.Lens' DeleteUserDefinedFunctionResponse Core.Int
dudfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dudfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
