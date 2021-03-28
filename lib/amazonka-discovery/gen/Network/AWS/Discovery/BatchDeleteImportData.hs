{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.BatchDeleteImportData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more import tasks, each identified by their import ID. Each import task has a number of records that can identify servers or applications. 
--
-- AWS Application Discovery Service has built-in matching logic that will identify when discovered servers match existing entries that you've previously discovered, the information for the already-existing discovered server is updated. When you delete an import task that contains records that were used to match, the information in those matched records that comes from the deleted records will also be deleted.
module Network.AWS.Discovery.BatchDeleteImportData
    (
    -- * Creating a request
      BatchDeleteImportData (..)
    , mkBatchDeleteImportData
    -- ** Request lenses
    , bdidImportTaskIds

    -- * Destructuring the response
    , BatchDeleteImportDataResponse (..)
    , mkBatchDeleteImportDataResponse
    -- ** Response lenses
    , bdidrrsErrors
    , bdidrrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDeleteImportData' smart constructor.
newtype BatchDeleteImportData = BatchDeleteImportData'
  { importTaskIds :: Core.NonEmpty Types.ImportTaskIdentifier
    -- ^ The IDs for the import tasks that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteImportData' value with any optional fields omitted.
mkBatchDeleteImportData
    :: Core.NonEmpty Types.ImportTaskIdentifier -- ^ 'importTaskIds'
    -> BatchDeleteImportData
mkBatchDeleteImportData importTaskIds
  = BatchDeleteImportData'{importTaskIds}

-- | The IDs for the import tasks that you want to delete.
--
-- /Note:/ Consider using 'importTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdidImportTaskIds :: Lens.Lens' BatchDeleteImportData (Core.NonEmpty Types.ImportTaskIdentifier)
bdidImportTaskIds = Lens.field @"importTaskIds"
{-# INLINEABLE bdidImportTaskIds #-}
{-# DEPRECATED importTaskIds "Use generic-lens or generic-optics with 'importTaskIds' instead"  #-}

instance Core.ToQuery BatchDeleteImportData where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchDeleteImportData where
        toHeaders BatchDeleteImportData{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSPoseidonService_V2015_11_01.BatchDeleteImportData")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchDeleteImportData where
        toJSON BatchDeleteImportData{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("importTaskIds" Core..= importTaskIds)])

instance Core.AWSRequest BatchDeleteImportData where
        type Rs BatchDeleteImportData = BatchDeleteImportDataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchDeleteImportDataResponse' Core.<$>
                   (x Core..:? "errors") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchDeleteImportDataResponse' smart constructor.
data BatchDeleteImportDataResponse = BatchDeleteImportDataResponse'
  { errors :: Core.Maybe [Types.BatchDeleteImportDataError]
    -- ^ Error messages returned for each import task that you deleted as a response for this command.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteImportDataResponse' value with any optional fields omitted.
mkBatchDeleteImportDataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchDeleteImportDataResponse
mkBatchDeleteImportDataResponse responseStatus
  = BatchDeleteImportDataResponse'{errors = Core.Nothing,
                                   responseStatus}

-- | Error messages returned for each import task that you deleted as a response for this command.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdidrrsErrors :: Lens.Lens' BatchDeleteImportDataResponse (Core.Maybe [Types.BatchDeleteImportDataError])
bdidrrsErrors = Lens.field @"errors"
{-# INLINEABLE bdidrrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdidrrsResponseStatus :: Lens.Lens' BatchDeleteImportDataResponse Core.Int
bdidrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bdidrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
