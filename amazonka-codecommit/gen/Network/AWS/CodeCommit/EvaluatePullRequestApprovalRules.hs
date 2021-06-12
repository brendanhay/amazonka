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
-- Module      : Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Evaluates whether a pull request has met all the conditions specified in
-- its associated approval rules.
module Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules
  ( -- * Creating a Request
    EvaluatePullRequestApprovalRules (..),
    newEvaluatePullRequestApprovalRules,

    -- * Request Lenses
    evaluatePullRequestApprovalRules_pullRequestId,
    evaluatePullRequestApprovalRules_revisionId,

    -- * Destructuring the Response
    EvaluatePullRequestApprovalRulesResponse (..),
    newEvaluatePullRequestApprovalRulesResponse,

    -- * Response Lenses
    evaluatePullRequestApprovalRulesResponse_httpStatus,
    evaluatePullRequestApprovalRulesResponse_evaluation,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEvaluatePullRequestApprovalRules' smart constructor.
data EvaluatePullRequestApprovalRules = EvaluatePullRequestApprovalRules'
  { -- | The system-generated ID of the pull request you want to evaluate.
    pullRequestId :: Core.Text,
    -- | The system-generated ID for the pull request revision. To retrieve the
    -- most recent revision ID for a pull request, use GetPullRequest.
    revisionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EvaluatePullRequestApprovalRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'evaluatePullRequestApprovalRules_pullRequestId' - The system-generated ID of the pull request you want to evaluate.
--
-- 'revisionId', 'evaluatePullRequestApprovalRules_revisionId' - The system-generated ID for the pull request revision. To retrieve the
-- most recent revision ID for a pull request, use GetPullRequest.
newEvaluatePullRequestApprovalRules ::
  -- | 'pullRequestId'
  Core.Text ->
  -- | 'revisionId'
  Core.Text ->
  EvaluatePullRequestApprovalRules
newEvaluatePullRequestApprovalRules
  pPullRequestId_
  pRevisionId_ =
    EvaluatePullRequestApprovalRules'
      { pullRequestId =
          pPullRequestId_,
        revisionId = pRevisionId_
      }

-- | The system-generated ID of the pull request you want to evaluate.
evaluatePullRequestApprovalRules_pullRequestId :: Lens.Lens' EvaluatePullRequestApprovalRules Core.Text
evaluatePullRequestApprovalRules_pullRequestId = Lens.lens (\EvaluatePullRequestApprovalRules' {pullRequestId} -> pullRequestId) (\s@EvaluatePullRequestApprovalRules' {} a -> s {pullRequestId = a} :: EvaluatePullRequestApprovalRules)

-- | The system-generated ID for the pull request revision. To retrieve the
-- most recent revision ID for a pull request, use GetPullRequest.
evaluatePullRequestApprovalRules_revisionId :: Lens.Lens' EvaluatePullRequestApprovalRules Core.Text
evaluatePullRequestApprovalRules_revisionId = Lens.lens (\EvaluatePullRequestApprovalRules' {revisionId} -> revisionId) (\s@EvaluatePullRequestApprovalRules' {} a -> s {revisionId = a} :: EvaluatePullRequestApprovalRules)

instance
  Core.AWSRequest
    EvaluatePullRequestApprovalRules
  where
  type
    AWSResponse EvaluatePullRequestApprovalRules =
      EvaluatePullRequestApprovalRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          EvaluatePullRequestApprovalRulesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "evaluation")
      )

instance
  Core.Hashable
    EvaluatePullRequestApprovalRules

instance Core.NFData EvaluatePullRequestApprovalRules

instance
  Core.ToHeaders
    EvaluatePullRequestApprovalRules
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.EvaluatePullRequestApprovalRules" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EvaluatePullRequestApprovalRules where
  toJSON EvaluatePullRequestApprovalRules' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("revisionId" Core..= revisionId)
          ]
      )

instance Core.ToPath EvaluatePullRequestApprovalRules where
  toPath = Core.const "/"

instance
  Core.ToQuery
    EvaluatePullRequestApprovalRules
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEvaluatePullRequestApprovalRulesResponse' smart constructor.
data EvaluatePullRequestApprovalRulesResponse = EvaluatePullRequestApprovalRulesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The result of the evaluation, including the names of the rules whose
    -- conditions have been met (if any), the names of the rules whose
    -- conditions have not been met (if any), whether the pull request is in
    -- the approved state, and whether the pull request approval rule has been
    -- set aside by an override.
    evaluation :: Evaluation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EvaluatePullRequestApprovalRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'evaluatePullRequestApprovalRulesResponse_httpStatus' - The response's http status code.
--
-- 'evaluation', 'evaluatePullRequestApprovalRulesResponse_evaluation' - The result of the evaluation, including the names of the rules whose
-- conditions have been met (if any), the names of the rules whose
-- conditions have not been met (if any), whether the pull request is in
-- the approved state, and whether the pull request approval rule has been
-- set aside by an override.
newEvaluatePullRequestApprovalRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'evaluation'
  Evaluation ->
  EvaluatePullRequestApprovalRulesResponse
newEvaluatePullRequestApprovalRulesResponse
  pHttpStatus_
  pEvaluation_ =
    EvaluatePullRequestApprovalRulesResponse'
      { httpStatus =
          pHttpStatus_,
        evaluation = pEvaluation_
      }

-- | The response's http status code.
evaluatePullRequestApprovalRulesResponse_httpStatus :: Lens.Lens' EvaluatePullRequestApprovalRulesResponse Core.Int
evaluatePullRequestApprovalRulesResponse_httpStatus = Lens.lens (\EvaluatePullRequestApprovalRulesResponse' {httpStatus} -> httpStatus) (\s@EvaluatePullRequestApprovalRulesResponse' {} a -> s {httpStatus = a} :: EvaluatePullRequestApprovalRulesResponse)

-- | The result of the evaluation, including the names of the rules whose
-- conditions have been met (if any), the names of the rules whose
-- conditions have not been met (if any), whether the pull request is in
-- the approved state, and whether the pull request approval rule has been
-- set aside by an override.
evaluatePullRequestApprovalRulesResponse_evaluation :: Lens.Lens' EvaluatePullRequestApprovalRulesResponse Evaluation
evaluatePullRequestApprovalRulesResponse_evaluation = Lens.lens (\EvaluatePullRequestApprovalRulesResponse' {evaluation} -> evaluation) (\s@EvaluatePullRequestApprovalRulesResponse' {} a -> s {evaluation = a} :: EvaluatePullRequestApprovalRulesResponse)

instance
  Core.NFData
    EvaluatePullRequestApprovalRulesResponse
