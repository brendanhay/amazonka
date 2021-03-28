{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelImportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-process import virtual machine or import snapshot task.
module Network.AWS.EC2.CancelImportTask
    (
    -- * Creating a request
      CancelImportTask (..)
    , mkCancelImportTask
    -- ** Request lenses
    , citCancelReason
    , citDryRun
    , citImportTaskId

    -- * Destructuring the response
    , CancelImportTaskResponse (..)
    , mkCancelImportTaskResponse
    -- ** Response lenses
    , citrrsImportTaskId
    , citrrsPreviousState
    , citrrsState
    , citrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelImportTask' smart constructor.
data CancelImportTask = CancelImportTask'
  { cancelReason :: Core.Maybe Core.Text
    -- ^ The reason for canceling the task.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , importTaskId :: Core.Maybe Types.ImportTaskId
    -- ^ The ID of the import image or import snapshot task to be canceled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelImportTask' value with any optional fields omitted.
mkCancelImportTask
    :: CancelImportTask
mkCancelImportTask
  = CancelImportTask'{cancelReason = Core.Nothing,
                      dryRun = Core.Nothing, importTaskId = Core.Nothing}

-- | The reason for canceling the task.
--
-- /Note:/ Consider using 'cancelReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citCancelReason :: Lens.Lens' CancelImportTask (Core.Maybe Core.Text)
citCancelReason = Lens.field @"cancelReason"
{-# INLINEABLE citCancelReason #-}
{-# DEPRECATED cancelReason "Use generic-lens or generic-optics with 'cancelReason' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citDryRun :: Lens.Lens' CancelImportTask (Core.Maybe Core.Bool)
citDryRun = Lens.field @"dryRun"
{-# INLINEABLE citDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the import image or import snapshot task to be canceled.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citImportTaskId :: Lens.Lens' CancelImportTask (Core.Maybe Types.ImportTaskId)
citImportTaskId = Lens.field @"importTaskId"
{-# INLINEABLE citImportTaskId #-}
{-# DEPRECATED importTaskId "Use generic-lens or generic-optics with 'importTaskId' instead"  #-}

instance Core.ToQuery CancelImportTask where
        toQuery CancelImportTask{..}
          = Core.toQueryPair "Action" ("CancelImportTask" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CancelReason")
                cancelReason
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ImportTaskId")
                importTaskId

instance Core.ToHeaders CancelImportTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CancelImportTask where
        type Rs CancelImportTask = CancelImportTaskResponse
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
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CancelImportTaskResponse' Core.<$>
                   (x Core..@? "importTaskId") Core.<*> x Core..@? "previousState"
                     Core.<*> x Core..@? "state"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelImportTaskResponse' smart constructor.
data CancelImportTaskResponse = CancelImportTaskResponse'
  { importTaskId :: Core.Maybe Core.Text
    -- ^ The ID of the task being canceled.
  , previousState :: Core.Maybe Core.Text
    -- ^ The current state of the task being canceled.
  , state :: Core.Maybe Core.Text
    -- ^ The current state of the task being canceled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelImportTaskResponse' value with any optional fields omitted.
mkCancelImportTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelImportTaskResponse
mkCancelImportTaskResponse responseStatus
  = CancelImportTaskResponse'{importTaskId = Core.Nothing,
                              previousState = Core.Nothing, state = Core.Nothing, responseStatus}

-- | The ID of the task being canceled.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrrsImportTaskId :: Lens.Lens' CancelImportTaskResponse (Core.Maybe Core.Text)
citrrsImportTaskId = Lens.field @"importTaskId"
{-# INLINEABLE citrrsImportTaskId #-}
{-# DEPRECATED importTaskId "Use generic-lens or generic-optics with 'importTaskId' instead"  #-}

-- | The current state of the task being canceled.
--
-- /Note:/ Consider using 'previousState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrrsPreviousState :: Lens.Lens' CancelImportTaskResponse (Core.Maybe Core.Text)
citrrsPreviousState = Lens.field @"previousState"
{-# INLINEABLE citrrsPreviousState #-}
{-# DEPRECATED previousState "Use generic-lens or generic-optics with 'previousState' instead"  #-}

-- | The current state of the task being canceled.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrrsState :: Lens.Lens' CancelImportTaskResponse (Core.Maybe Core.Text)
citrrsState = Lens.field @"state"
{-# INLINEABLE citrrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrrsResponseStatus :: Lens.Lens' CancelImportTaskResponse Core.Int
citrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE citrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
