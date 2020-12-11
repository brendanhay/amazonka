{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    parrsApprovedAt,
    parrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @PutApprovalResult@ action.
--
-- /See:/ 'mkPutApprovalResult' smart constructor.
data PutApprovalResult = PutApprovalResult'
  { pipelineName ::
      Lude.Text,
    stageName :: Lude.Text,
    actionName :: Lude.Text,
    result :: ApprovalResult,
    token :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutApprovalResult' with the minimum fields required to make a request.
--
-- * 'actionName' - The name of the action for which approval is requested.
-- * 'pipelineName' - The name of the pipeline that contains the action.
-- * 'result' - Represents information about the result of the approval request.
-- * 'stageName' - The name of the stage that contains the action.
-- * 'token' - The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action. It is used to validate that the approval request corresponding to this token is still valid.
mkPutApprovalResult ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  -- | 'actionName'
  Lude.Text ->
  -- | 'result'
  ApprovalResult ->
  -- | 'token'
  Lude.Text ->
  PutApprovalResult
mkPutApprovalResult
  pPipelineName_
  pStageName_
  pActionName_
  pResult_
  pToken_ =
    PutApprovalResult'
      { pipelineName = pPipelineName_,
        stageName = pStageName_,
        actionName = pActionName_,
        result = pResult_,
        token = pToken_
      }

-- | The name of the pipeline that contains the action.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parPipelineName :: Lens.Lens' PutApprovalResult Lude.Text
parPipelineName = Lens.lens (pipelineName :: PutApprovalResult -> Lude.Text) (\s a -> s {pipelineName = a} :: PutApprovalResult)
{-# DEPRECATED parPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The name of the stage that contains the action.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parStageName :: Lens.Lens' PutApprovalResult Lude.Text
parStageName = Lens.lens (stageName :: PutApprovalResult -> Lude.Text) (\s a -> s {stageName = a} :: PutApprovalResult)
{-# DEPRECATED parStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | The name of the action for which approval is requested.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parActionName :: Lens.Lens' PutApprovalResult Lude.Text
parActionName = Lens.lens (actionName :: PutApprovalResult -> Lude.Text) (\s a -> s {actionName = a} :: PutApprovalResult)
{-# DEPRECATED parActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | Represents information about the result of the approval request.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parResult :: Lens.Lens' PutApprovalResult ApprovalResult
parResult = Lens.lens (result :: PutApprovalResult -> ApprovalResult) (\s a -> s {result = a} :: PutApprovalResult)
{-# DEPRECATED parResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action. It is used to validate that the approval request corresponding to this token is still valid.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parToken :: Lens.Lens' PutApprovalResult Lude.Text
parToken = Lens.lens (token :: PutApprovalResult -> Lude.Text) (\s a -> s {token = a} :: PutApprovalResult)
{-# DEPRECATED parToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Lude.AWSRequest PutApprovalResult where
  type Rs PutApprovalResult = PutApprovalResultResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutApprovalResultResponse'
            Lude.<$> (x Lude..?> "approvedAt") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutApprovalResult where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.PutApprovalResult" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutApprovalResult where
  toJSON PutApprovalResult' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineName" Lude..= pipelineName),
            Lude.Just ("stageName" Lude..= stageName),
            Lude.Just ("actionName" Lude..= actionName),
            Lude.Just ("result" Lude..= result),
            Lude.Just ("token" Lude..= token)
          ]
      )

instance Lude.ToPath PutApprovalResult where
  toPath = Lude.const "/"

instance Lude.ToQuery PutApprovalResult where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @PutApprovalResult@ action.
--
-- /See:/ 'mkPutApprovalResultResponse' smart constructor.
data PutApprovalResultResponse = PutApprovalResultResponse'
  { approvedAt ::
      Lude.Maybe Lude.Timestamp,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutApprovalResultResponse' with the minimum fields required to make a request.
--
-- * 'approvedAt' - The timestamp showing when the approval or rejection was submitted.
-- * 'responseStatus' - The response status code.
mkPutApprovalResultResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutApprovalResultResponse
mkPutApprovalResultResponse pResponseStatus_ =
  PutApprovalResultResponse'
    { approvedAt = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The timestamp showing when the approval or rejection was submitted.
--
-- /Note:/ Consider using 'approvedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsApprovedAt :: Lens.Lens' PutApprovalResultResponse (Lude.Maybe Lude.Timestamp)
parrsApprovedAt = Lens.lens (approvedAt :: PutApprovalResultResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {approvedAt = a} :: PutApprovalResultResponse)
{-# DEPRECATED parrsApprovedAt "Use generic-lens or generic-optics with 'approvedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsResponseStatus :: Lens.Lens' PutApprovalResultResponse Lude.Int
parrsResponseStatus = Lens.lens (responseStatus :: PutApprovalResultResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutApprovalResultResponse)
{-# DEPRECATED parrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
