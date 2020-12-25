{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateApprovalRuleTemplateContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the content of an approval rule template. You can change the number of required approvals, the membership of the approval rule, and whether an approval pool is defined.
module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateContent
  ( -- * Creating a request
    UpdateApprovalRuleTemplateContent (..),
    mkUpdateApprovalRuleTemplateContent,

    -- ** Request lenses
    uartcApprovalRuleTemplateName,
    uartcNewRuleContent,
    uartcExistingRuleContentSha256,

    -- * Destructuring the response
    UpdateApprovalRuleTemplateContentResponse (..),
    mkUpdateApprovalRuleTemplateContentResponse,

    -- ** Response lenses
    uartcrrsApprovalRuleTemplate,
    uartcrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApprovalRuleTemplateContent' smart constructor.
data UpdateApprovalRuleTemplateContent = UpdateApprovalRuleTemplateContent'
  { -- | The name of the approval rule template where you want to update the content of the rule.
    approvalRuleTemplateName :: Types.ApprovalRuleTemplateName,
    -- | The content that replaces the existing content of the rule. Content statements must be complete. You cannot provide only the changes.
    newRuleContent :: Types.NewRuleContent,
    -- | The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
    existingRuleContentSha256 :: Core.Maybe Types.RuleContentSha256
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApprovalRuleTemplateContent' value with any optional fields omitted.
mkUpdateApprovalRuleTemplateContent ::
  -- | 'approvalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  -- | 'newRuleContent'
  Types.NewRuleContent ->
  UpdateApprovalRuleTemplateContent
mkUpdateApprovalRuleTemplateContent
  approvalRuleTemplateName
  newRuleContent =
    UpdateApprovalRuleTemplateContent'
      { approvalRuleTemplateName,
        newRuleContent,
        existingRuleContentSha256 = Core.Nothing
      }

-- | The name of the approval rule template where you want to update the content of the rule.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartcApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateContent Types.ApprovalRuleTemplateName
uartcApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# DEPRECATED uartcApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | The content that replaces the existing content of the rule. Content statements must be complete. You cannot provide only the changes.
--
-- /Note:/ Consider using 'newRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartcNewRuleContent :: Lens.Lens' UpdateApprovalRuleTemplateContent Types.NewRuleContent
uartcNewRuleContent = Lens.field @"newRuleContent"
{-# DEPRECATED uartcNewRuleContent "Use generic-lens or generic-optics with 'newRuleContent' instead." #-}

-- | The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
--
-- /Note:/ Consider using 'existingRuleContentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartcExistingRuleContentSha256 :: Lens.Lens' UpdateApprovalRuleTemplateContent (Core.Maybe Types.RuleContentSha256)
uartcExistingRuleContentSha256 = Lens.field @"existingRuleContentSha256"
{-# DEPRECATED uartcExistingRuleContentSha256 "Use generic-lens or generic-optics with 'existingRuleContentSha256' instead." #-}

instance Core.FromJSON UpdateApprovalRuleTemplateContent where
  toJSON UpdateApprovalRuleTemplateContent {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("approvalRuleTemplateName" Core..= approvalRuleTemplateName),
            Core.Just ("newRuleContent" Core..= newRuleContent),
            ("existingRuleContentSha256" Core..=)
              Core.<$> existingRuleContentSha256
          ]
      )

instance Core.AWSRequest UpdateApprovalRuleTemplateContent where
  type
    Rs UpdateApprovalRuleTemplateContent =
      UpdateApprovalRuleTemplateContentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.UpdateApprovalRuleTemplateContent"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateContentResponse'
            Core.<$> (x Core..: "approvalRuleTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateApprovalRuleTemplateContentResponse' smart constructor.
data UpdateApprovalRuleTemplateContentResponse = UpdateApprovalRuleTemplateContentResponse'
  { approvalRuleTemplate :: Types.ApprovalRuleTemplate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateApprovalRuleTemplateContentResponse' value with any optional fields omitted.
mkUpdateApprovalRuleTemplateContentResponse ::
  -- | 'approvalRuleTemplate'
  Types.ApprovalRuleTemplate ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateApprovalRuleTemplateContentResponse
mkUpdateApprovalRuleTemplateContentResponse
  approvalRuleTemplate
  responseStatus =
    UpdateApprovalRuleTemplateContentResponse'
      { approvalRuleTemplate,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartcrrsApprovalRuleTemplate :: Lens.Lens' UpdateApprovalRuleTemplateContentResponse Types.ApprovalRuleTemplate
uartcrrsApprovalRuleTemplate = Lens.field @"approvalRuleTemplate"
{-# DEPRECATED uartcrrsApprovalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartcrrsResponseStatus :: Lens.Lens' UpdateApprovalRuleTemplateContentResponse Core.Int
uartcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uartcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
