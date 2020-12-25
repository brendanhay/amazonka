{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutApprovalResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the response to a manual approval request to AWS CodePipeline. Valid responses include Approved and Rejected.
module Network.AWS.CodePipeline.PutApprovalResult
  ( -- * Creating a request
    PutApprovalResult (..),
    mkPutApprovalResult,

    -- ** Request lenses
    parPipelineName,
    parStageName,
    parActionName,
    parResult,
    parToken,

    -- * Destructuring the response
    PutApprovalResultResponse (..),
    mkPutApprovalResultResponse,

    -- ** Response lenses
    parrrsApprovedAt,
    parrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PutApprovalResult@ action.
--
-- /See:/ 'mkPutApprovalResult' smart constructor.
data PutApprovalResult = PutApprovalResult'
  { -- | The name of the pipeline that contains the action.
    pipelineName :: Types.PipelineName,
    -- | The name of the stage that contains the action.
    stageName :: Types.StageName,
    -- | The name of the action for which approval is requested.
    actionName :: Types.ActionName,
    -- | Represents information about the result of the approval request.
    result :: Types.ApprovalResult,
    -- | The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action. It is used to validate that the approval request corresponding to this token is still valid.
    token :: Types.ApprovalToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutApprovalResult' value with any optional fields omitted.
mkPutApprovalResult ::
  -- | 'pipelineName'
  Types.PipelineName ->
  -- | 'stageName'
  Types.StageName ->
  -- | 'actionName'
  Types.ActionName ->
  -- | 'result'
  Types.ApprovalResult ->
  -- | 'token'
  Types.ApprovalToken ->
  PutApprovalResult
mkPutApprovalResult pipelineName stageName actionName result token =
  PutApprovalResult'
    { pipelineName,
      stageName,
      actionName,
      result,
      token
    }

-- | The name of the pipeline that contains the action.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parPipelineName :: Lens.Lens' PutApprovalResult Types.PipelineName
parPipelineName = Lens.field @"pipelineName"
{-# DEPRECATED parPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The name of the stage that contains the action.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parStageName :: Lens.Lens' PutApprovalResult Types.StageName
parStageName = Lens.field @"stageName"
{-# DEPRECATED parStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | The name of the action for which approval is requested.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parActionName :: Lens.Lens' PutApprovalResult Types.ActionName
parActionName = Lens.field @"actionName"
{-# DEPRECATED parActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | Represents information about the result of the approval request.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parResult :: Lens.Lens' PutApprovalResult Types.ApprovalResult
parResult = Lens.field @"result"
{-# DEPRECATED parResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action. It is used to validate that the approval request corresponding to this token is still valid.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parToken :: Lens.Lens' PutApprovalResult Types.ApprovalToken
parToken = Lens.field @"token"
{-# DEPRECATED parToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.FromJSON PutApprovalResult where
  toJSON PutApprovalResult {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            Core.Just ("stageName" Core..= stageName),
            Core.Just ("actionName" Core..= actionName),
            Core.Just ("result" Core..= result),
            Core.Just ("token" Core..= token)
          ]
      )

instance Core.AWSRequest PutApprovalResult where
  type Rs PutApprovalResult = PutApprovalResultResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.PutApprovalResult")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutApprovalResultResponse'
            Core.<$> (x Core..:? "approvedAt") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @PutApprovalResult@ action.
--
-- /See:/ 'mkPutApprovalResultResponse' smart constructor.
data PutApprovalResultResponse = PutApprovalResultResponse'
  { -- | The timestamp showing when the approval or rejection was submitted.
    approvedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutApprovalResultResponse' value with any optional fields omitted.
mkPutApprovalResultResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutApprovalResultResponse
mkPutApprovalResultResponse responseStatus =
  PutApprovalResultResponse'
    { approvedAt = Core.Nothing,
      responseStatus
    }

-- | The timestamp showing when the approval or rejection was submitted.
--
-- /Note:/ Consider using 'approvedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrrsApprovedAt :: Lens.Lens' PutApprovalResultResponse (Core.Maybe Core.NominalDiffTime)
parrrsApprovedAt = Lens.field @"approvedAt"
{-# DEPRECATED parrrsApprovedAt "Use generic-lens or generic-optics with 'approvedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrrsResponseStatus :: Lens.Lens' PutApprovalResultResponse Core.Int
parrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED parrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
