{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreateApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a template for approval rules that can then be associated with one or more repositories in your AWS account. When you associate a template with a repository, AWS CodeCommit creates an approval rule that matches the conditions of the template for all pull requests that meet the conditions of the template. For more information, see 'AssociateApprovalRuleTemplateWithRepository' .
module Network.AWS.CodeCommit.CreateApprovalRuleTemplate
  ( -- * Creating a request
    CreateApprovalRuleTemplate (..),
    mkCreateApprovalRuleTemplate,

    -- ** Request lenses
    cartApprovalRuleTemplateName,
    cartApprovalRuleTemplateContent,
    cartApprovalRuleTemplateDescription,

    -- * Destructuring the response
    CreateApprovalRuleTemplateResponse (..),
    mkCreateApprovalRuleTemplateResponse,

    -- ** Response lenses
    cartrrsApprovalRuleTemplate,
    cartrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateApprovalRuleTemplate' smart constructor.
data CreateApprovalRuleTemplate = CreateApprovalRuleTemplate'
  { -- | The name of the approval rule template. Provide descriptive names, because this name is applied to the approval rules created automatically in associated repositories.
    approvalRuleTemplateName :: Types.ApprovalRuleTemplateName,
    -- | The content of the approval rule that is created on pull requests in associated repositories. If you specify one or more destination references (branches), approval rules are created in an associated repository only if their destination references (branches) match those specified in the template.
    approvalRuleTemplateContent :: Types.ApprovalRuleTemplateContent,
    -- | The description of the approval rule template. Consider providing a description that explains what this template does and when it might be appropriate to associate it with repositories.
    approvalRuleTemplateDescription :: Core.Maybe Types.ApprovalRuleTemplateDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApprovalRuleTemplate' value with any optional fields omitted.
mkCreateApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  -- | 'approvalRuleTemplateContent'
  Types.ApprovalRuleTemplateContent ->
  CreateApprovalRuleTemplate
mkCreateApprovalRuleTemplate
  approvalRuleTemplateName
  approvalRuleTemplateContent =
    CreateApprovalRuleTemplate'
      { approvalRuleTemplateName,
        approvalRuleTemplateContent,
        approvalRuleTemplateDescription = Core.Nothing
      }

-- | The name of the approval rule template. Provide descriptive names, because this name is applied to the approval rules created automatically in associated repositories.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cartApprovalRuleTemplateName :: Lens.Lens' CreateApprovalRuleTemplate Types.ApprovalRuleTemplateName
cartApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# DEPRECATED cartApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | The content of the approval rule that is created on pull requests in associated repositories. If you specify one or more destination references (branches), approval rules are created in an associated repository only if their destination references (branches) match those specified in the template.
--
-- /Note:/ Consider using 'approvalRuleTemplateContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cartApprovalRuleTemplateContent :: Lens.Lens' CreateApprovalRuleTemplate Types.ApprovalRuleTemplateContent
cartApprovalRuleTemplateContent = Lens.field @"approvalRuleTemplateContent"
{-# DEPRECATED cartApprovalRuleTemplateContent "Use generic-lens or generic-optics with 'approvalRuleTemplateContent' instead." #-}

-- | The description of the approval rule template. Consider providing a description that explains what this template does and when it might be appropriate to associate it with repositories.
--
-- /Note:/ Consider using 'approvalRuleTemplateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cartApprovalRuleTemplateDescription :: Lens.Lens' CreateApprovalRuleTemplate (Core.Maybe Types.ApprovalRuleTemplateDescription)
cartApprovalRuleTemplateDescription = Lens.field @"approvalRuleTemplateDescription"
{-# DEPRECATED cartApprovalRuleTemplateDescription "Use generic-lens or generic-optics with 'approvalRuleTemplateDescription' instead." #-}

instance Core.FromJSON CreateApprovalRuleTemplate where
  toJSON CreateApprovalRuleTemplate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("approvalRuleTemplateName" Core..= approvalRuleTemplateName),
            Core.Just
              ( "approvalRuleTemplateContent"
                  Core..= approvalRuleTemplateContent
              ),
            ("approvalRuleTemplateDescription" Core..=)
              Core.<$> approvalRuleTemplateDescription
          ]
      )

instance Core.AWSRequest CreateApprovalRuleTemplate where
  type
    Rs CreateApprovalRuleTemplate =
      CreateApprovalRuleTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.CreateApprovalRuleTemplate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApprovalRuleTemplateResponse'
            Core.<$> (x Core..: "approvalRuleTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateApprovalRuleTemplateResponse' smart constructor.
data CreateApprovalRuleTemplateResponse = CreateApprovalRuleTemplateResponse'
  { -- | The content and structure of the created approval rule template.
    approvalRuleTemplate :: Types.ApprovalRuleTemplate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateApprovalRuleTemplateResponse' value with any optional fields omitted.
mkCreateApprovalRuleTemplateResponse ::
  -- | 'approvalRuleTemplate'
  Types.ApprovalRuleTemplate ->
  -- | 'responseStatus'
  Core.Int ->
  CreateApprovalRuleTemplateResponse
mkCreateApprovalRuleTemplateResponse
  approvalRuleTemplate
  responseStatus =
    CreateApprovalRuleTemplateResponse'
      { approvalRuleTemplate,
        responseStatus
      }

-- | The content and structure of the created approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cartrrsApprovalRuleTemplate :: Lens.Lens' CreateApprovalRuleTemplateResponse Types.ApprovalRuleTemplate
cartrrsApprovalRuleTemplate = Lens.field @"approvalRuleTemplate"
{-# DEPRECATED cartrrsApprovalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cartrrsResponseStatus :: Lens.Lens' CreateApprovalRuleTemplateResponse Core.Int
cartrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cartrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
