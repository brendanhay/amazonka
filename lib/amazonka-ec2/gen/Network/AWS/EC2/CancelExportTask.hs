{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active export task. The request removes all artifacts of the export, including any partially-created Amazon S3 objects. If the export task is complete or is in the process of transferring the final disk image, the command fails and returns an error.
module Network.AWS.EC2.CancelExportTask
    (
    -- * Creating a request
      CancelExportTask (..)
    , mkCancelExportTask
    -- ** Request lenses
    , cetExportTaskId

    -- * Destructuring the response
    , CancelExportTaskResponse (..)
    , mkCancelExportTaskResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask'
  { exportTaskId :: Types.ExportVmTaskId
    -- ^ The ID of the export task. This is the ID returned by @CreateInstanceExportTask@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelExportTask' value with any optional fields omitted.
mkCancelExportTask
    :: Types.ExportVmTaskId -- ^ 'exportTaskId'
    -> CancelExportTask
mkCancelExportTask exportTaskId = CancelExportTask'{exportTaskId}

-- | The ID of the export task. This is the ID returned by @CreateInstanceExportTask@ .
--
-- /Note:/ Consider using 'exportTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetExportTaskId :: Lens.Lens' CancelExportTask Types.ExportVmTaskId
cetExportTaskId = Lens.field @"exportTaskId"
{-# INLINEABLE cetExportTaskId #-}
{-# DEPRECATED exportTaskId "Use generic-lens or generic-optics with 'exportTaskId' instead"  #-}

instance Core.ToQuery CancelExportTask where
        toQuery CancelExportTask{..}
          = Core.toQueryPair "Action" ("CancelExportTask" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ExportTaskId" exportTaskId

instance Core.ToHeaders CancelExportTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CancelExportTask where
        type Rs CancelExportTask = CancelExportTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull CancelExportTaskResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelExportTaskResponse' smart constructor.
data CancelExportTaskResponse = CancelExportTaskResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelExportTaskResponse' value with any optional fields omitted.
mkCancelExportTaskResponse
    :: CancelExportTaskResponse
mkCancelExportTaskResponse = CancelExportTaskResponse'
