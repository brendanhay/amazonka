{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description for a specified approval rule template.
module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription
  ( -- * Creating a request
    UpdateApprovalRuleTemplateDescription (..),
    mkUpdateApprovalRuleTemplateDescription,

    -- ** Request lenses
    uartdApprovalRuleTemplateName,
    uartdApprovalRuleTemplateDescription,

    -- * Destructuring the response
    UpdateApprovalRuleTemplateDescriptionResponse (..),
    mkUpdateApprovalRuleTemplateDescriptionResponse,

    -- ** Response lenses
    uartdrrsApprovalRuleTemplate,
    uartdrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApprovalRuleTemplateDescription' smart constructor.
data UpdateApprovalRuleTemplateDescription = UpdateApprovalRuleTemplateDescription'
  { -- | The name of the template for which you want to update the description.
    approvalRuleTemplateName :: Types.ApprovalRuleTemplateName,
    -- | The updated description of the approval rule template.
    approvalRuleTemplateDescription :: Types.ApprovalRuleTemplateDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApprovalRuleTemplateDescription' value with any optional fields omitted.
mkUpdateApprovalRuleTemplateDescription ::
  -- | 'approvalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  -- | 'approvalRuleTemplateDescription'
  Types.ApprovalRuleTemplateDescription ->
  UpdateApprovalRuleTemplateDescription
mkUpdateApprovalRuleTemplateDescription
  approvalRuleTemplateName
  approvalRuleTemplateDescription =
    UpdateApprovalRuleTemplateDescription'
      { approvalRuleTemplateName,
        approvalRuleTemplateDescription
      }

-- | The name of the template for which you want to update the description.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartdApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateDescription Types.ApprovalRuleTemplateName
uartdApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# DEPRECATED uartdApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | The updated description of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartdApprovalRuleTemplateDescription :: Lens.Lens' UpdateApprovalRuleTemplateDescription Types.ApprovalRuleTemplateDescription
uartdApprovalRuleTemplateDescription = Lens.field @"approvalRuleTemplateDescription"
{-# DEPRECATED uartdApprovalRuleTemplateDescription "Use generic-lens or generic-optics with 'approvalRuleTemplateDescription' instead." #-}

instance Core.FromJSON UpdateApprovalRuleTemplateDescription where
  toJSON UpdateApprovalRuleTemplateDescription {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("approvalRuleTemplateName" Core..= approvalRuleTemplateName),
            Core.Just
              ( "approvalRuleTemplateDescription"
                  Core..= approvalRuleTemplateDescription
              )
          ]
      )

instance Core.AWSRequest UpdateApprovalRuleTemplateDescription where
  type
    Rs UpdateApprovalRuleTemplateDescription =
      UpdateApprovalRuleTemplateDescriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.UpdateApprovalRuleTemplateDescription"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateDescriptionResponse'
            Core.<$> (x Core..: "approvalRuleTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateApprovalRuleTemplateDescriptionResponse' smart constructor.
data UpdateApprovalRuleTemplateDescriptionResponse = UpdateApprovalRuleTemplateDescriptionResponse'
  { -- | The structure and content of the updated approval rule template.
    approvalRuleTemplate :: Types.ApprovalRuleTemplate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateApprovalRuleTemplateDescriptionResponse' value with any optional fields omitted.
mkUpdateApprovalRuleTemplateDescriptionResponse ::
  -- | 'approvalRuleTemplate'
  Types.ApprovalRuleTemplate ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateApprovalRuleTemplateDescriptionResponse
mkUpdateApprovalRuleTemplateDescriptionResponse
  approvalRuleTemplate
  responseStatus =
    UpdateApprovalRuleTemplateDescriptionResponse'
      { approvalRuleTemplate,
        responseStatus
      }

-- | The structure and content of the updated approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartdrrsApprovalRuleTemplate :: Lens.Lens' UpdateApprovalRuleTemplateDescriptionResponse Types.ApprovalRuleTemplate
uartdrrsApprovalRuleTemplate = Lens.field @"approvalRuleTemplate"
{-# DEPRECATED uartdrrsApprovalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartdrrsResponseStatus :: Lens.Lens' UpdateApprovalRuleTemplateDescriptionResponse Core.Int
uartdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uartdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
