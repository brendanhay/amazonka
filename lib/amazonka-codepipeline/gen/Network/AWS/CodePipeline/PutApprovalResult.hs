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
    pPipelineName,
    pToken,
    pResult,
    pActionName,
    pStageName,

    -- * Destructuring the response
    PutApprovalResultResponse (..),
    mkPutApprovalResultResponse,

    -- ** Response lenses
    prsApprovedAt,
    prsResponseStatus,
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
  { -- | The name of the pipeline that contains the action.
    pipelineName :: Lude.Text,
    -- | The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action. It is used to validate that the approval request corresponding to this token is still valid.
    token :: Lude.Text,
    -- | Represents information about the result of the approval request.
    result :: ApprovalResult,
    -- | The name of the action for which approval is requested.
    actionName :: Lude.Text,
    -- | The name of the stage that contains the action.
    stageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutApprovalResult' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline that contains the action.
-- * 'token' - The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action. It is used to validate that the approval request corresponding to this token is still valid.
-- * 'result' - Represents information about the result of the approval request.
-- * 'actionName' - The name of the action for which approval is requested.
-- * 'stageName' - The name of the stage that contains the action.
mkPutApprovalResult ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'token'
  Lude.Text ->
  -- | 'result'
  ApprovalResult ->
  -- | 'actionName'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  PutApprovalResult
mkPutApprovalResult
  pPipelineName_
  pToken_
  pResult_
  pActionName_
  pStageName_ =
    PutApprovalResult'
      { pipelineName = pPipelineName_,
        token = pToken_,
        result = pResult_,
        actionName = pActionName_,
        stageName = pStageName_
      }

-- | The name of the pipeline that contains the action.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPipelineName :: Lens.Lens' PutApprovalResult Lude.Text
pPipelineName = Lens.lens (pipelineName :: PutApprovalResult -> Lude.Text) (\s a -> s {pipelineName = a} :: PutApprovalResult)
{-# DEPRECATED pPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action. It is used to validate that the approval request corresponding to this token is still valid.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pToken :: Lens.Lens' PutApprovalResult Lude.Text
pToken = Lens.lens (token :: PutApprovalResult -> Lude.Text) (\s a -> s {token = a} :: PutApprovalResult)
{-# DEPRECATED pToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | Represents information about the result of the approval request.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResult :: Lens.Lens' PutApprovalResult ApprovalResult
pResult = Lens.lens (result :: PutApprovalResult -> ApprovalResult) (\s a -> s {result = a} :: PutApprovalResult)
{-# DEPRECATED pResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The name of the action for which approval is requested.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pActionName :: Lens.Lens' PutApprovalResult Lude.Text
pActionName = Lens.lens (actionName :: PutApprovalResult -> Lude.Text) (\s a -> s {actionName = a} :: PutApprovalResult)
{-# DEPRECATED pActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The name of the stage that contains the action.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStageName :: Lens.Lens' PutApprovalResult Lude.Text
pStageName = Lens.lens (stageName :: PutApprovalResult -> Lude.Text) (\s a -> s {stageName = a} :: PutApprovalResult)
{-# DEPRECATED pStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

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
            Lude.Just ("token" Lude..= token),
            Lude.Just ("result" Lude..= result),
            Lude.Just ("actionName" Lude..= actionName),
            Lude.Just ("stageName" Lude..= stageName)
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
  { -- | The timestamp showing when the approval or rejection was submitted.
    approvedAt :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
prsApprovedAt :: Lens.Lens' PutApprovalResultResponse (Lude.Maybe Lude.Timestamp)
prsApprovedAt = Lens.lens (approvedAt :: PutApprovalResultResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {approvedAt = a} :: PutApprovalResultResponse)
{-# DEPRECATED prsApprovedAt "Use generic-lens or generic-optics with 'approvedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsResponseStatus :: Lens.Lens' PutApprovalResultResponse Lude.Int
prsResponseStatus = Lens.lens (responseStatus :: PutApprovalResultResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutApprovalResultResponse)
{-# DEPRECATED prsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
