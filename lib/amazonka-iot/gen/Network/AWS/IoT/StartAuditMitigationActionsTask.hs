{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.StartAuditMitigationActionsTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task that applies a set of mitigation actions to the specified target.
module Network.AWS.IoT.StartAuditMitigationActionsTask
    (
    -- * Creating a request
      StartAuditMitigationActionsTask (..)
    , mkStartAuditMitigationActionsTask
    -- ** Request lenses
    , samatTaskId
    , samatTarget
    , samatAuditCheckToActionsMapping
    , samatClientRequestToken

    -- * Destructuring the response
    , StartAuditMitigationActionsTaskResponse (..)
    , mkStartAuditMitigationActionsTaskResponse
    -- ** Response lenses
    , samatrrsTaskId
    , samatrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartAuditMitigationActionsTask' smart constructor.
data StartAuditMitigationActionsTask = StartAuditMitigationActionsTask'
  { taskId :: Types.AuditMitigationActionsTaskId
    -- ^ A unique identifier for the task. You can use this identifier to check the status of the task or to cancel it.
  , target :: Types.AuditMitigationActionsTaskTarget
    -- ^ Specifies the audit findings to which the mitigation actions are applied. You can apply them to a type of audit check, to all findings from an audit, or to a speecific set of findings.
  , auditCheckToActionsMapping :: Core.HashMap Types.AuditCheckName (Core.NonEmpty Types.MitigationActionName)
    -- ^ For an audit check, specifies which mitigation actions to apply. Those actions must be defined in your AWS account.
  , clientRequestToken :: Types.ClientRequestToken
    -- ^ Each audit mitigation task must have a unique client request token. If you try to start a new task with the same token as a task that already exists, an exception occurs. If you omit this value, a unique client request token is generated automatically.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartAuditMitigationActionsTask' value with any optional fields omitted.
mkStartAuditMitigationActionsTask
    :: Types.AuditMitigationActionsTaskId -- ^ 'taskId'
    -> Types.AuditMitigationActionsTaskTarget -- ^ 'target'
    -> Types.ClientRequestToken -- ^ 'clientRequestToken'
    -> StartAuditMitigationActionsTask
mkStartAuditMitigationActionsTask taskId target clientRequestToken
  = StartAuditMitigationActionsTask'{taskId, target,
                                     auditCheckToActionsMapping = Core.mempty, clientRequestToken}

-- | A unique identifier for the task. You can use this identifier to check the status of the task or to cancel it.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatTaskId :: Lens.Lens' StartAuditMitigationActionsTask Types.AuditMitigationActionsTaskId
samatTaskId = Lens.field @"taskId"
{-# INLINEABLE samatTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | Specifies the audit findings to which the mitigation actions are applied. You can apply them to a type of audit check, to all findings from an audit, or to a speecific set of findings.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatTarget :: Lens.Lens' StartAuditMitigationActionsTask Types.AuditMitigationActionsTaskTarget
samatTarget = Lens.field @"target"
{-# INLINEABLE samatTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

-- | For an audit check, specifies which mitigation actions to apply. Those actions must be defined in your AWS account.
--
-- /Note:/ Consider using 'auditCheckToActionsMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatAuditCheckToActionsMapping :: Lens.Lens' StartAuditMitigationActionsTask (Core.HashMap Types.AuditCheckName (Core.NonEmpty Types.MitigationActionName))
samatAuditCheckToActionsMapping = Lens.field @"auditCheckToActionsMapping"
{-# INLINEABLE samatAuditCheckToActionsMapping #-}
{-# DEPRECATED auditCheckToActionsMapping "Use generic-lens or generic-optics with 'auditCheckToActionsMapping' instead"  #-}

-- | Each audit mitigation task must have a unique client request token. If you try to start a new task with the same token as a task that already exists, an exception occurs. If you omit this value, a unique client request token is generated automatically.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatClientRequestToken :: Lens.Lens' StartAuditMitigationActionsTask Types.ClientRequestToken
samatClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE samatClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

instance Core.ToQuery StartAuditMitigationActionsTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartAuditMitigationActionsTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON StartAuditMitigationActionsTask where
        toJSON StartAuditMitigationActionsTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("target" Core..= target),
                  Core.Just
                    ("auditCheckToActionsMapping" Core..= auditCheckToActionsMapping),
                  Core.Just ("clientRequestToken" Core..= clientRequestToken)])

instance Core.AWSRequest StartAuditMitigationActionsTask where
        type Rs StartAuditMitigationActionsTask =
             StartAuditMitigationActionsTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/audit/mitigationactions/tasks/" Core.<> Core.toText taskId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartAuditMitigationActionsTaskResponse' Core.<$>
                   (x Core..:? "taskId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartAuditMitigationActionsTaskResponse' smart constructor.
data StartAuditMitigationActionsTaskResponse = StartAuditMitigationActionsTaskResponse'
  { taskId :: Core.Maybe Types.AuditMitigationActionsTaskId
    -- ^ The unique identifier for the audit mitigation task. This matches the @taskId@ that you specified in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartAuditMitigationActionsTaskResponse' value with any optional fields omitted.
mkStartAuditMitigationActionsTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartAuditMitigationActionsTaskResponse
mkStartAuditMitigationActionsTaskResponse responseStatus
  = StartAuditMitigationActionsTaskResponse'{taskId = Core.Nothing,
                                             responseStatus}

-- | The unique identifier for the audit mitigation task. This matches the @taskId@ that you specified in the request.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatrrsTaskId :: Lens.Lens' StartAuditMitigationActionsTaskResponse (Core.Maybe Types.AuditMitigationActionsTaskId)
samatrrsTaskId = Lens.field @"taskId"
{-# INLINEABLE samatrrsTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatrrsResponseStatus :: Lens.Lens' StartAuditMitigationActionsTaskResponse Core.Int
samatrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE samatrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
