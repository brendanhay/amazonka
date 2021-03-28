{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.CancelExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified export task.
--
-- The task must be in the @PENDING@ or @RUNNING@ state.
module Network.AWS.CloudWatchLogs.CancelExportTask
    (
    -- * Creating a request
      CancelExportTask (..)
    , mkCancelExportTask
    -- ** Request lenses
    , cetTaskId

    -- * Destructuring the response
    , CancelExportTaskResponse (..)
    , mkCancelExportTaskResponse
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask'
  { taskId :: Types.ExportTaskId
    -- ^ The ID of the export task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelExportTask' value with any optional fields omitted.
mkCancelExportTask
    :: Types.ExportTaskId -- ^ 'taskId'
    -> CancelExportTask
mkCancelExportTask taskId = CancelExportTask'{taskId}

-- | The ID of the export task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetTaskId :: Lens.Lens' CancelExportTask Types.ExportTaskId
cetTaskId = Lens.field @"taskId"
{-# INLINEABLE cetTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

instance Core.ToQuery CancelExportTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelExportTask where
        toHeaders CancelExportTask{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.CancelExportTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelExportTask where
        toJSON CancelExportTask{..}
          = Core.object
              (Core.catMaybes [Core.Just ("taskId" Core..= taskId)])

instance Core.AWSRequest CancelExportTask where
        type Rs CancelExportTask = CancelExportTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
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
