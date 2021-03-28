{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutApprovalResult (..)
    , mkPutApprovalResult
    -- ** Request lenses
    , parPipelineName
    , parStageName
    , parActionName
    , parResult
    , parToken

    -- * Destructuring the response
    , PutApprovalResultResponse (..)
    , mkPutApprovalResultResponse
    -- ** Response lenses
    , parrrsApprovedAt
    , parrrsResponseStatus
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PutApprovalResult@ action.
--
-- /See:/ 'mkPutApprovalResult' smart constructor.
data PutApprovalResult = PutApprovalResult'
  { pipelineName :: Types.PipelineName
    -- ^ The name of the pipeline that contains the action. 
  , stageName :: Types.StageName
    -- ^ The name of the stage that contains the action.
  , actionName :: Types.ActionName
    -- ^ The name of the action for which approval is requested.
  , result :: Types.ApprovalResult
    -- ^ Represents information about the result of the approval request.
  , token :: Types.ApprovalToken
    -- ^ The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action. It is used to validate that the approval request corresponding to this token is still valid.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutApprovalResult' value with any optional fields omitted.
mkPutApprovalResult
    :: Types.PipelineName -- ^ 'pipelineName'
    -> Types.StageName -- ^ 'stageName'
    -> Types.ActionName -- ^ 'actionName'
    -> Types.ApprovalResult -- ^ 'result'
    -> Types.ApprovalToken -- ^ 'token'
    -> PutApprovalResult
mkPutApprovalResult pipelineName stageName actionName result token
  = PutApprovalResult'{pipelineName, stageName, actionName, result,
                       token}

-- | The name of the pipeline that contains the action. 
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parPipelineName :: Lens.Lens' PutApprovalResult Types.PipelineName
parPipelineName = Lens.field @"pipelineName"
{-# INLINEABLE parPipelineName #-}
{-# DEPRECATED pipelineName "Use generic-lens or generic-optics with 'pipelineName' instead"  #-}

-- | The name of the stage that contains the action.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parStageName :: Lens.Lens' PutApprovalResult Types.StageName
parStageName = Lens.field @"stageName"
{-# INLINEABLE parStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | The name of the action for which approval is requested.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parActionName :: Lens.Lens' PutApprovalResult Types.ActionName
parActionName = Lens.field @"actionName"
{-# INLINEABLE parActionName #-}
{-# DEPRECATED actionName "Use generic-lens or generic-optics with 'actionName' instead"  #-}

-- | Represents information about the result of the approval request.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parResult :: Lens.Lens' PutApprovalResult Types.ApprovalResult
parResult = Lens.field @"result"
{-# INLINEABLE parResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

-- | The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action. It is used to validate that the approval request corresponding to this token is still valid.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parToken :: Lens.Lens' PutApprovalResult Types.ApprovalToken
parToken = Lens.field @"token"
{-# INLINEABLE parToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

instance Core.ToQuery PutApprovalResult where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutApprovalResult where
        toHeaders PutApprovalResult{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.PutApprovalResult")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutApprovalResult where
        toJSON PutApprovalResult{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineName" Core..= pipelineName),
                  Core.Just ("stageName" Core..= stageName),
                  Core.Just ("actionName" Core..= actionName),
                  Core.Just ("result" Core..= result),
                  Core.Just ("token" Core..= token)])

instance Core.AWSRequest PutApprovalResult where
        type Rs PutApprovalResult = PutApprovalResultResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutApprovalResultResponse' Core.<$>
                   (x Core..:? "approvedAt") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @PutApprovalResult@ action.
--
-- /See:/ 'mkPutApprovalResultResponse' smart constructor.
data PutApprovalResultResponse = PutApprovalResultResponse'
  { approvedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp showing when the approval or rejection was submitted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutApprovalResultResponse' value with any optional fields omitted.
mkPutApprovalResultResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutApprovalResultResponse
mkPutApprovalResultResponse responseStatus
  = PutApprovalResultResponse'{approvedAt = Core.Nothing,
                               responseStatus}

-- | The timestamp showing when the approval or rejection was submitted.
--
-- /Note:/ Consider using 'approvedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrrsApprovedAt :: Lens.Lens' PutApprovalResultResponse (Core.Maybe Core.NominalDiffTime)
parrrsApprovedAt = Lens.field @"approvedAt"
{-# INLINEABLE parrrsApprovedAt #-}
{-# DEPRECATED approvedAt "Use generic-lens or generic-optics with 'approvedAt' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrrsResponseStatus :: Lens.Lens' PutApprovalResultResponse Core.Int
parrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE parrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
