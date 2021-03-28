{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreatePullRequestApprovalRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an approval rule for a pull request.
module Network.AWS.CodeCommit.CreatePullRequestApprovalRule
    (
    -- * Creating a request
      CreatePullRequestApprovalRule (..)
    , mkCreatePullRequestApprovalRule
    -- ** Request lenses
    , cprarPullRequestId
    , cprarApprovalRuleName
    , cprarApprovalRuleContent

    -- * Destructuring the response
    , CreatePullRequestApprovalRuleResponse (..)
    , mkCreatePullRequestApprovalRuleResponse
    -- ** Response lenses
    , cprarrrsApprovalRule
    , cprarrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePullRequestApprovalRule' smart constructor.
data CreatePullRequestApprovalRule = CreatePullRequestApprovalRule'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID of the pull request for which you want to create the approval rule.
  , approvalRuleName :: Types.ApprovalRuleName
    -- ^ The name for the approval rule.
  , approvalRuleContent :: Types.ApprovalRuleContent
    -- ^ The content of the approval rule, including the number of approvals needed and the structure of an approval pool defined for approvals, if any. For more information about approval pools, see the AWS CodeCommit User Guide.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePullRequestApprovalRule' value with any optional fields omitted.
mkCreatePullRequestApprovalRule
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> Types.ApprovalRuleName -- ^ 'approvalRuleName'
    -> Types.ApprovalRuleContent -- ^ 'approvalRuleContent'
    -> CreatePullRequestApprovalRule
mkCreatePullRequestApprovalRule pullRequestId approvalRuleName
  approvalRuleContent
  = CreatePullRequestApprovalRule'{pullRequestId, approvalRuleName,
                                   approvalRuleContent}

-- | The system-generated ID of the pull request for which you want to create the approval rule.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarPullRequestId :: Lens.Lens' CreatePullRequestApprovalRule Types.PullRequestId
cprarPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE cprarPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The name for the approval rule.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarApprovalRuleName :: Lens.Lens' CreatePullRequestApprovalRule Types.ApprovalRuleName
cprarApprovalRuleName = Lens.field @"approvalRuleName"
{-# INLINEABLE cprarApprovalRuleName #-}
{-# DEPRECATED approvalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead"  #-}

-- | The content of the approval rule, including the number of approvals needed and the structure of an approval pool defined for approvals, if any. For more information about approval pools, see the AWS CodeCommit User Guide.
--
-- /Note:/ Consider using 'approvalRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarApprovalRuleContent :: Lens.Lens' CreatePullRequestApprovalRule Types.ApprovalRuleContent
cprarApprovalRuleContent = Lens.field @"approvalRuleContent"
{-# INLINEABLE cprarApprovalRuleContent #-}
{-# DEPRECATED approvalRuleContent "Use generic-lens or generic-optics with 'approvalRuleContent' instead"  #-}

instance Core.ToQuery CreatePullRequestApprovalRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePullRequestApprovalRule where
        toHeaders CreatePullRequestApprovalRule{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.CreatePullRequestApprovalRule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePullRequestApprovalRule where
        toJSON CreatePullRequestApprovalRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  Core.Just ("approvalRuleName" Core..= approvalRuleName),
                  Core.Just ("approvalRuleContent" Core..= approvalRuleContent)])

instance Core.AWSRequest CreatePullRequestApprovalRule where
        type Rs CreatePullRequestApprovalRule =
             CreatePullRequestApprovalRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePullRequestApprovalRuleResponse' Core.<$>
                   (x Core..: "approvalRule") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePullRequestApprovalRuleResponse' smart constructor.
data CreatePullRequestApprovalRuleResponse = CreatePullRequestApprovalRuleResponse'
  { approvalRule :: Types.ApprovalRule
    -- ^ Information about the created approval rule.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreatePullRequestApprovalRuleResponse' value with any optional fields omitted.
mkCreatePullRequestApprovalRuleResponse
    :: Types.ApprovalRule -- ^ 'approvalRule'
    -> Core.Int -- ^ 'responseStatus'
    -> CreatePullRequestApprovalRuleResponse
mkCreatePullRequestApprovalRuleResponse approvalRule responseStatus
  = CreatePullRequestApprovalRuleResponse'{approvalRule,
                                           responseStatus}

-- | Information about the created approval rule.
--
-- /Note:/ Consider using 'approvalRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarrrsApprovalRule :: Lens.Lens' CreatePullRequestApprovalRuleResponse Types.ApprovalRule
cprarrrsApprovalRule = Lens.field @"approvalRule"
{-# INLINEABLE cprarrrsApprovalRule #-}
{-# DEPRECATED approvalRule "Use generic-lens or generic-optics with 'approvalRule' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarrrsResponseStatus :: Lens.Lens' CreatePullRequestApprovalRuleResponse Core.Int
cprarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
