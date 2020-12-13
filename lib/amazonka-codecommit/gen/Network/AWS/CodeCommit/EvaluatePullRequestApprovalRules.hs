{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Evaluates whether a pull request has met all the conditions specified in its associated approval rules.
module Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules
  ( -- * Creating a request
    EvaluatePullRequestApprovalRules (..),
    mkEvaluatePullRequestApprovalRules,

    -- ** Request lenses
    eprarPullRequestId,
    eprarRevisionId,

    -- * Destructuring the response
    EvaluatePullRequestApprovalRulesResponse (..),
    mkEvaluatePullRequestApprovalRulesResponse,

    -- ** Response lenses
    eprarrsEvaluation,
    eprarrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEvaluatePullRequestApprovalRules' smart constructor.
data EvaluatePullRequestApprovalRules = EvaluatePullRequestApprovalRules'
  { -- | The system-generated ID of the pull request you want to evaluate.
    pullRequestId :: Lude.Text,
    -- | The system-generated ID for the pull request revision. To retrieve the most recent revision ID for a pull request, use 'GetPullRequest' .
    revisionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluatePullRequestApprovalRules' with the minimum fields required to make a request.
--
-- * 'pullRequestId' - The system-generated ID of the pull request you want to evaluate.
-- * 'revisionId' - The system-generated ID for the pull request revision. To retrieve the most recent revision ID for a pull request, use 'GetPullRequest' .
mkEvaluatePullRequestApprovalRules ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'revisionId'
  Lude.Text ->
  EvaluatePullRequestApprovalRules
mkEvaluatePullRequestApprovalRules pPullRequestId_ pRevisionId_ =
  EvaluatePullRequestApprovalRules'
    { pullRequestId =
        pPullRequestId_,
      revisionId = pRevisionId_
    }

-- | The system-generated ID of the pull request you want to evaluate.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprarPullRequestId :: Lens.Lens' EvaluatePullRequestApprovalRules Lude.Text
eprarPullRequestId = Lens.lens (pullRequestId :: EvaluatePullRequestApprovalRules -> Lude.Text) (\s a -> s {pullRequestId = a} :: EvaluatePullRequestApprovalRules)
{-# DEPRECATED eprarPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The system-generated ID for the pull request revision. To retrieve the most recent revision ID for a pull request, use 'GetPullRequest' .
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprarRevisionId :: Lens.Lens' EvaluatePullRequestApprovalRules Lude.Text
eprarRevisionId = Lens.lens (revisionId :: EvaluatePullRequestApprovalRules -> Lude.Text) (\s a -> s {revisionId = a} :: EvaluatePullRequestApprovalRules)
{-# DEPRECATED eprarRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.AWSRequest EvaluatePullRequestApprovalRules where
  type
    Rs EvaluatePullRequestApprovalRules =
      EvaluatePullRequestApprovalRulesResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          EvaluatePullRequestApprovalRulesResponse'
            Lude.<$> (x Lude..:> "evaluation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EvaluatePullRequestApprovalRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.EvaluatePullRequestApprovalRules" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EvaluatePullRequestApprovalRules where
  toJSON EvaluatePullRequestApprovalRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("revisionId" Lude..= revisionId)
          ]
      )

instance Lude.ToPath EvaluatePullRequestApprovalRules where
  toPath = Lude.const "/"

instance Lude.ToQuery EvaluatePullRequestApprovalRules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEvaluatePullRequestApprovalRulesResponse' smart constructor.
data EvaluatePullRequestApprovalRulesResponse = EvaluatePullRequestApprovalRulesResponse'
  { -- | The result of the evaluation, including the names of the rules whose conditions have been met (if any), the names of the rules whose conditions have not been met (if any), whether the pull request is in the approved state, and whether the pull request approval rule has been set aside by an override.
    evaluation :: Evaluation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluatePullRequestApprovalRulesResponse' with the minimum fields required to make a request.
--
-- * 'evaluation' - The result of the evaluation, including the names of the rules whose conditions have been met (if any), the names of the rules whose conditions have not been met (if any), whether the pull request is in the approved state, and whether the pull request approval rule has been set aside by an override.
-- * 'responseStatus' - The response status code.
mkEvaluatePullRequestApprovalRulesResponse ::
  -- | 'evaluation'
  Evaluation ->
  -- | 'responseStatus'
  Lude.Int ->
  EvaluatePullRequestApprovalRulesResponse
mkEvaluatePullRequestApprovalRulesResponse
  pEvaluation_
  pResponseStatus_ =
    EvaluatePullRequestApprovalRulesResponse'
      { evaluation =
          pEvaluation_,
        responseStatus = pResponseStatus_
      }

-- | The result of the evaluation, including the names of the rules whose conditions have been met (if any), the names of the rules whose conditions have not been met (if any), whether the pull request is in the approved state, and whether the pull request approval rule has been set aside by an override.
--
-- /Note:/ Consider using 'evaluation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprarrsEvaluation :: Lens.Lens' EvaluatePullRequestApprovalRulesResponse Evaluation
eprarrsEvaluation = Lens.lens (evaluation :: EvaluatePullRequestApprovalRulesResponse -> Evaluation) (\s a -> s {evaluation = a} :: EvaluatePullRequestApprovalRulesResponse)
{-# DEPRECATED eprarrsEvaluation "Use generic-lens or generic-optics with 'evaluation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprarrsResponseStatus :: Lens.Lens' EvaluatePullRequestApprovalRulesResponse Lude.Int
eprarrsResponseStatus = Lens.lens (responseStatus :: EvaluatePullRequestApprovalRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EvaluatePullRequestApprovalRulesResponse)
{-# DEPRECATED eprarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
