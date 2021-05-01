{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PutApprovalResult@ action.
--
-- /See:/ 'newPutApprovalResult' smart constructor.
data PutApprovalResult = PutApprovalResult'
  { -- | The name of the pipeline that contains the action.
    pipelineName :: Prelude.Text,
    -- | The name of the stage that contains the action.
    stageName :: Prelude.Text,
    -- | The name of the action for which approval is requested.
    actionName :: Prelude.Text,
    -- | Represents information about the result of the approval request.
    result :: ApprovalResult,
    -- | The system-generated token used to identify a unique approval request.
    -- The token for each open approval request can be obtained using the
    -- GetPipelineState action. It is used to validate that the approval
    -- request corresponding to this token is still valid.
    token :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  -- | 'actionName'
  Prelude.Text ->
  -- | 'result'
  ApprovalResult ->
  -- | 'token'
  Prelude.Text ->
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
putApprovalResult_pipelineName :: Lens.Lens' PutApprovalResult Prelude.Text
putApprovalResult_pipelineName = Lens.lens (\PutApprovalResult' {pipelineName} -> pipelineName) (\s@PutApprovalResult' {} a -> s {pipelineName = a} :: PutApprovalResult)

-- | The name of the stage that contains the action.
putApprovalResult_stageName :: Lens.Lens' PutApprovalResult Prelude.Text
putApprovalResult_stageName = Lens.lens (\PutApprovalResult' {stageName} -> stageName) (\s@PutApprovalResult' {} a -> s {stageName = a} :: PutApprovalResult)

-- | The name of the action for which approval is requested.
putApprovalResult_actionName :: Lens.Lens' PutApprovalResult Prelude.Text
putApprovalResult_actionName = Lens.lens (\PutApprovalResult' {actionName} -> actionName) (\s@PutApprovalResult' {} a -> s {actionName = a} :: PutApprovalResult)

-- | Represents information about the result of the approval request.
putApprovalResult_result :: Lens.Lens' PutApprovalResult ApprovalResult
putApprovalResult_result = Lens.lens (\PutApprovalResult' {result} -> result) (\s@PutApprovalResult' {} a -> s {result = a} :: PutApprovalResult)

-- | The system-generated token used to identify a unique approval request.
-- The token for each open approval request can be obtained using the
-- GetPipelineState action. It is used to validate that the approval
-- request corresponding to this token is still valid.
putApprovalResult_token :: Lens.Lens' PutApprovalResult Prelude.Text
putApprovalResult_token = Lens.lens (\PutApprovalResult' {token} -> token) (\s@PutApprovalResult' {} a -> s {token = a} :: PutApprovalResult)

instance Prelude.AWSRequest PutApprovalResult where
  type Rs PutApprovalResult = PutApprovalResultResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutApprovalResultResponse'
            Prelude.<$> (x Prelude..?> "approvedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutApprovalResult

instance Prelude.NFData PutApprovalResult

instance Prelude.ToHeaders PutApprovalResult where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.PutApprovalResult" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutApprovalResult where
  toJSON PutApprovalResult' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pipelineName" Prelude..= pipelineName),
            Prelude.Just ("stageName" Prelude..= stageName),
            Prelude.Just ("actionName" Prelude..= actionName),
            Prelude.Just ("result" Prelude..= result),
            Prelude.Just ("token" Prelude..= token)
          ]
      )

instance Prelude.ToPath PutApprovalResult where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutApprovalResult where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @PutApprovalResult@ action.
--
-- /See:/ 'newPutApprovalResultResponse' smart constructor.
data PutApprovalResultResponse = PutApprovalResultResponse'
  { -- | The timestamp showing when the approval or rejection was submitted.
    approvedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  PutApprovalResultResponse
newPutApprovalResultResponse pHttpStatus_ =
  PutApprovalResultResponse'
    { approvedAt =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp showing when the approval or rejection was submitted.
putApprovalResultResponse_approvedAt :: Lens.Lens' PutApprovalResultResponse (Prelude.Maybe Prelude.UTCTime)
putApprovalResultResponse_approvedAt = Lens.lens (\PutApprovalResultResponse' {approvedAt} -> approvedAt) (\s@PutApprovalResultResponse' {} a -> s {approvedAt = a} :: PutApprovalResultResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
putApprovalResultResponse_httpStatus :: Lens.Lens' PutApprovalResultResponse Prelude.Int
putApprovalResultResponse_httpStatus = Lens.lens (\PutApprovalResultResponse' {httpStatus} -> httpStatus) (\s@PutApprovalResultResponse' {} a -> s {httpStatus = a} :: PutApprovalResultResponse)

instance Prelude.NFData PutApprovalResultResponse
