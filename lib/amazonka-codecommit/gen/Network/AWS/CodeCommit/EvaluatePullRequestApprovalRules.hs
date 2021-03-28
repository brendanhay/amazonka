{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      EvaluatePullRequestApprovalRules (..)
    , mkEvaluatePullRequestApprovalRules
    -- ** Request lenses
    , eprarPullRequestId
    , eprarRevisionId

    -- * Destructuring the response
    , EvaluatePullRequestApprovalRulesResponse (..)
    , mkEvaluatePullRequestApprovalRulesResponse
    -- ** Response lenses
    , eprarrrsEvaluation
    , eprarrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEvaluatePullRequestApprovalRules' smart constructor.
data EvaluatePullRequestApprovalRules = EvaluatePullRequestApprovalRules'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID of the pull request you want to evaluate.
  , revisionId :: Types.RevisionId
    -- ^ The system-generated ID for the pull request revision. To retrieve the most recent revision ID for a pull request, use 'GetPullRequest' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EvaluatePullRequestApprovalRules' value with any optional fields omitted.
mkEvaluatePullRequestApprovalRules
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> Types.RevisionId -- ^ 'revisionId'
    -> EvaluatePullRequestApprovalRules
mkEvaluatePullRequestApprovalRules pullRequestId revisionId
  = EvaluatePullRequestApprovalRules'{pullRequestId, revisionId}

-- | The system-generated ID of the pull request you want to evaluate.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprarPullRequestId :: Lens.Lens' EvaluatePullRequestApprovalRules Types.PullRequestId
eprarPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE eprarPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The system-generated ID for the pull request revision. To retrieve the most recent revision ID for a pull request, use 'GetPullRequest' .
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprarRevisionId :: Lens.Lens' EvaluatePullRequestApprovalRules Types.RevisionId
eprarRevisionId = Lens.field @"revisionId"
{-# INLINEABLE eprarRevisionId #-}
{-# DEPRECATED revisionId "Use generic-lens or generic-optics with 'revisionId' instead"  #-}

instance Core.ToQuery EvaluatePullRequestApprovalRules where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EvaluatePullRequestApprovalRules where
        toHeaders EvaluatePullRequestApprovalRules{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.EvaluatePullRequestApprovalRules")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EvaluatePullRequestApprovalRules where
        toJSON EvaluatePullRequestApprovalRules{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  Core.Just ("revisionId" Core..= revisionId)])

instance Core.AWSRequest EvaluatePullRequestApprovalRules where
        type Rs EvaluatePullRequestApprovalRules =
             EvaluatePullRequestApprovalRulesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 EvaluatePullRequestApprovalRulesResponse' Core.<$>
                   (x Core..: "evaluation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEvaluatePullRequestApprovalRulesResponse' smart constructor.
data EvaluatePullRequestApprovalRulesResponse = EvaluatePullRequestApprovalRulesResponse'
  { evaluation :: Types.Evaluation
    -- ^ The result of the evaluation, including the names of the rules whose conditions have been met (if any), the names of the rules whose conditions have not been met (if any), whether the pull request is in the approved state, and whether the pull request approval rule has been set aside by an override. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EvaluatePullRequestApprovalRulesResponse' value with any optional fields omitted.
mkEvaluatePullRequestApprovalRulesResponse
    :: Types.Evaluation -- ^ 'evaluation'
    -> Core.Int -- ^ 'responseStatus'
    -> EvaluatePullRequestApprovalRulesResponse
mkEvaluatePullRequestApprovalRulesResponse evaluation
  responseStatus
  = EvaluatePullRequestApprovalRulesResponse'{evaluation,
                                              responseStatus}

-- | The result of the evaluation, including the names of the rules whose conditions have been met (if any), the names of the rules whose conditions have not been met (if any), whether the pull request is in the approved state, and whether the pull request approval rule has been set aside by an override. 
--
-- /Note:/ Consider using 'evaluation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprarrrsEvaluation :: Lens.Lens' EvaluatePullRequestApprovalRulesResponse Types.Evaluation
eprarrrsEvaluation = Lens.field @"evaluation"
{-# INLINEABLE eprarrrsEvaluation #-}
{-# DEPRECATED evaluation "Use generic-lens or generic-optics with 'evaluation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprarrrsResponseStatus :: Lens.Lens' EvaluatePullRequestApprovalRulesResponse Core.Int
eprarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eprarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
