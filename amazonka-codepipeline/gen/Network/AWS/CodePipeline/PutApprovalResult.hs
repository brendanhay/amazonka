{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutApprovalResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the response to a manual approval request to AWS CodePipeline.
-- Valid responses include Approved and Rejected.
module Network.AWS.CodePipeline.PutApprovalResult
  ( -- * Creating a Request
    PutApprovalResult (..),
    newPutApprovalResult,

    -- * Request Lenses
    putApprovalResult_pipelineName,
    putApprovalResult_stageName,
    putApprovalResult_actionName,
    putApprovalResult_result,
    putApprovalResult_token,

    -- * Destructuring the Response
    PutApprovalResultResponse (..),
    newPutApprovalResultResponse,

    -- * Response Lenses
    putApprovalResultResponse_approvedAt,
    putApprovalResultResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PutApprovalResult@ action.
--
-- /See:/ 'newPutApprovalResult' smart constructor.
data PutApprovalResult = PutApprovalResult'
  { -- | The name of the pipeline that contains the action.
    pipelineName :: Core.Text,
    -- | The name of the stage that contains the action.
    stageName :: Core.Text,
    -- | The name of the action for which approval is requested.
    actionName :: Core.Text,
    -- | Represents information about the result of the approval request.
    result :: ApprovalResult,
    -- | The system-generated token used to identify a unique approval request.
    -- The token for each open approval request can be obtained using the
    -- GetPipelineState action. It is used to validate that the approval
    -- request corresponding to this token is still valid.
    token :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutApprovalResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'putApprovalResult_pipelineName' - The name of the pipeline that contains the action.
--
-- 'stageName', 'putApprovalResult_stageName' - The name of the stage that contains the action.
--
-- 'actionName', 'putApprovalResult_actionName' - The name of the action for which approval is requested.
--
-- 'result', 'putApprovalResult_result' - Represents information about the result of the approval request.
--
-- 'token', 'putApprovalResult_token' - The system-generated token used to identify a unique approval request.
-- The token for each open approval request can be obtained using the
-- GetPipelineState action. It is used to validate that the approval
-- request corresponding to this token is still valid.
newPutApprovalResult ::
  -- | 'pipelineName'
  Core.Text ->
  -- | 'stageName'
  Core.Text ->
  -- | 'actionName'
  Core.Text ->
  -- | 'result'
  ApprovalResult ->
  -- | 'token'
  Core.Text ->
  PutApprovalResult
newPutApprovalResult
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
putApprovalResult_pipelineName :: Lens.Lens' PutApprovalResult Core.Text
putApprovalResult_pipelineName = Lens.lens (\PutApprovalResult' {pipelineName} -> pipelineName) (\s@PutApprovalResult' {} a -> s {pipelineName = a} :: PutApprovalResult)

-- | The name of the stage that contains the action.
putApprovalResult_stageName :: Lens.Lens' PutApprovalResult Core.Text
putApprovalResult_stageName = Lens.lens (\PutApprovalResult' {stageName} -> stageName) (\s@PutApprovalResult' {} a -> s {stageName = a} :: PutApprovalResult)

-- | The name of the action for which approval is requested.
putApprovalResult_actionName :: Lens.Lens' PutApprovalResult Core.Text
putApprovalResult_actionName = Lens.lens (\PutApprovalResult' {actionName} -> actionName) (\s@PutApprovalResult' {} a -> s {actionName = a} :: PutApprovalResult)

-- | Represents information about the result of the approval request.
putApprovalResult_result :: Lens.Lens' PutApprovalResult ApprovalResult
putApprovalResult_result = Lens.lens (\PutApprovalResult' {result} -> result) (\s@PutApprovalResult' {} a -> s {result = a} :: PutApprovalResult)

-- | The system-generated token used to identify a unique approval request.
-- The token for each open approval request can be obtained using the
-- GetPipelineState action. It is used to validate that the approval
-- request corresponding to this token is still valid.
putApprovalResult_token :: Lens.Lens' PutApprovalResult Core.Text
putApprovalResult_token = Lens.lens (\PutApprovalResult' {token} -> token) (\s@PutApprovalResult' {} a -> s {token = a} :: PutApprovalResult)

instance Core.AWSRequest PutApprovalResult where
  type
    AWSResponse PutApprovalResult =
      PutApprovalResultResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutApprovalResultResponse'
            Core.<$> (x Core..?> "approvedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutApprovalResult

instance Core.NFData PutApprovalResult

instance Core.ToHeaders PutApprovalResult where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.PutApprovalResult" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutApprovalResult where
  toJSON PutApprovalResult' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            Core.Just ("stageName" Core..= stageName),
            Core.Just ("actionName" Core..= actionName),
            Core.Just ("result" Core..= result),
            Core.Just ("token" Core..= token)
          ]
      )

instance Core.ToPath PutApprovalResult where
  toPath = Core.const "/"

instance Core.ToQuery PutApprovalResult where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @PutApprovalResult@ action.
--
-- /See:/ 'newPutApprovalResultResponse' smart constructor.
data PutApprovalResultResponse = PutApprovalResultResponse'
  { -- | The timestamp showing when the approval or rejection was submitted.
    approvedAt :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutApprovalResultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvedAt', 'putApprovalResultResponse_approvedAt' - The timestamp showing when the approval or rejection was submitted.
--
-- 'httpStatus', 'putApprovalResultResponse_httpStatus' - The response's http status code.
newPutApprovalResultResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutApprovalResultResponse
newPutApprovalResultResponse pHttpStatus_ =
  PutApprovalResultResponse'
    { approvedAt =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp showing when the approval or rejection was submitted.
putApprovalResultResponse_approvedAt :: Lens.Lens' PutApprovalResultResponse (Core.Maybe Core.UTCTime)
putApprovalResultResponse_approvedAt = Lens.lens (\PutApprovalResultResponse' {approvedAt} -> approvedAt) (\s@PutApprovalResultResponse' {} a -> s {approvedAt = a} :: PutApprovalResultResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
putApprovalResultResponse_httpStatus :: Lens.Lens' PutApprovalResultResponse Core.Int
putApprovalResultResponse_httpStatus = Lens.lens (\PutApprovalResultResponse' {httpStatus} -> httpStatus) (\s@PutApprovalResultResponse' {} a -> s {httpStatus = a} :: PutApprovalResultResponse)

instance Core.NFData PutApprovalResultResponse
