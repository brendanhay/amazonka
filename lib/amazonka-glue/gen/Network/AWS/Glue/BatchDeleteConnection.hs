{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchDeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of connection definitions from the Data Catalog.
module Network.AWS.Glue.BatchDeleteConnection
    (
    -- * Creating a request
      BatchDeleteConnection (..)
    , mkBatchDeleteConnection
    -- ** Request lenses
    , bdcConnectionNameList
    , bdcCatalogId

    -- * Destructuring the response
    , BatchDeleteConnectionResponse (..)
    , mkBatchDeleteConnectionResponse
    -- ** Response lenses
    , bdcrrsErrors
    , bdcrrsSucceeded
    , bdcrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDeleteConnection' smart constructor.
data BatchDeleteConnection = BatchDeleteConnection'
  { connectionNameList :: [Types.NameString]
    -- ^ A list of names of the connections to delete.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteConnection' value with any optional fields omitted.
mkBatchDeleteConnection
    :: BatchDeleteConnection
mkBatchDeleteConnection
  = BatchDeleteConnection'{connectionNameList = Core.mempty,
                           catalogId = Core.Nothing}

-- | A list of names of the connections to delete.
--
-- /Note:/ Consider using 'connectionNameList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcConnectionNameList :: Lens.Lens' BatchDeleteConnection [Types.NameString]
bdcConnectionNameList = Lens.field @"connectionNameList"
{-# INLINEABLE bdcConnectionNameList #-}
{-# DEPRECATED connectionNameList "Use generic-lens or generic-optics with 'connectionNameList' instead"  #-}

-- | The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcCatalogId :: Lens.Lens' BatchDeleteConnection (Core.Maybe Types.CatalogId)
bdcCatalogId = Lens.field @"catalogId"
{-# INLINEABLE bdcCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery BatchDeleteConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchDeleteConnection where
        toHeaders BatchDeleteConnection{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.BatchDeleteConnection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchDeleteConnection where
        toJSON BatchDeleteConnection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConnectionNameList" Core..= connectionNameList),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest BatchDeleteConnection where
        type Rs BatchDeleteConnection = BatchDeleteConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchDeleteConnectionResponse' Core.<$>
                   (x Core..:? "Errors") Core.<*> x Core..:? "Succeeded" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchDeleteConnectionResponse' smart constructor.
data BatchDeleteConnectionResponse = BatchDeleteConnectionResponse'
  { errors :: Core.Maybe (Core.HashMap Types.NameString Types.ErrorDetail)
    -- ^ A map of the names of connections that were not successfully deleted to error details.
  , succeeded :: Core.Maybe [Types.NameString]
    -- ^ A list of names of the connection definitions that were successfully deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteConnectionResponse' value with any optional fields omitted.
mkBatchDeleteConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchDeleteConnectionResponse
mkBatchDeleteConnectionResponse responseStatus
  = BatchDeleteConnectionResponse'{errors = Core.Nothing,
                                   succeeded = Core.Nothing, responseStatus}

-- | A map of the names of connections that were not successfully deleted to error details.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcrrsErrors :: Lens.Lens' BatchDeleteConnectionResponse (Core.Maybe (Core.HashMap Types.NameString Types.ErrorDetail))
bdcrrsErrors = Lens.field @"errors"
{-# INLINEABLE bdcrrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | A list of names of the connection definitions that were successfully deleted.
--
-- /Note:/ Consider using 'succeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcrrsSucceeded :: Lens.Lens' BatchDeleteConnectionResponse (Core.Maybe [Types.NameString])
bdcrrsSucceeded = Lens.field @"succeeded"
{-# INLINEABLE bdcrrsSucceeded #-}
{-# DEPRECATED succeeded "Use generic-lens or generic-optics with 'succeeded' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcrrsResponseStatus :: Lens.Lens' BatchDeleteConnectionResponse Core.Int
bdcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bdcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
