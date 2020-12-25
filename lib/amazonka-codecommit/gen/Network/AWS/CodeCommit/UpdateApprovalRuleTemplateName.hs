{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of a specified approval rule template.
module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
  ( -- * Creating a request
    UpdateApprovalRuleTemplateName (..),
    mkUpdateApprovalRuleTemplateName,

    -- ** Request lenses
    uartnOldApprovalRuleTemplateName,
    uartnNewApprovalRuleTemplateName,

    -- * Destructuring the response
    UpdateApprovalRuleTemplateNameResponse (..),
    mkUpdateApprovalRuleTemplateNameResponse,

    -- ** Response lenses
    uartnrrsApprovalRuleTemplate,
    uartnrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApprovalRuleTemplateName' smart constructor.
data UpdateApprovalRuleTemplateName = UpdateApprovalRuleTemplateName'
  { -- | The current name of the approval rule template.
    oldApprovalRuleTemplateName :: Types.ApprovalRuleTemplateName,
    -- | The new name you want to apply to the approval rule template.
    newApprovalRuleTemplateName :: Types.ApprovalRuleTemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApprovalRuleTemplateName' value with any optional fields omitted.
mkUpdateApprovalRuleTemplateName ::
  -- | 'oldApprovalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  -- | 'newApprovalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  UpdateApprovalRuleTemplateName
mkUpdateApprovalRuleTemplateName
  oldApprovalRuleTemplateName
  newApprovalRuleTemplateName =
    UpdateApprovalRuleTemplateName'
      { oldApprovalRuleTemplateName,
        newApprovalRuleTemplateName
      }

-- | The current name of the approval rule template.
--
-- /Note:/ Consider using 'oldApprovalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnOldApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateName Types.ApprovalRuleTemplateName
uartnOldApprovalRuleTemplateName = Lens.field @"oldApprovalRuleTemplateName"
{-# DEPRECATED uartnOldApprovalRuleTemplateName "Use generic-lens or generic-optics with 'oldApprovalRuleTemplateName' instead." #-}

-- | The new name you want to apply to the approval rule template.
--
-- /Note:/ Consider using 'newApprovalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnNewApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateName Types.ApprovalRuleTemplateName
uartnNewApprovalRuleTemplateName = Lens.field @"newApprovalRuleTemplateName"
{-# DEPRECATED uartnNewApprovalRuleTemplateName "Use generic-lens or generic-optics with 'newApprovalRuleTemplateName' instead." #-}

instance Core.FromJSON UpdateApprovalRuleTemplateName where
  toJSON UpdateApprovalRuleTemplateName {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "oldApprovalRuleTemplateName"
                  Core..= oldApprovalRuleTemplateName
              ),
            Core.Just
              ( "newApprovalRuleTemplateName"
                  Core..= newApprovalRuleTemplateName
              )
          ]
      )

instance Core.AWSRequest UpdateApprovalRuleTemplateName where
  type
    Rs UpdateApprovalRuleTemplateName =
      UpdateApprovalRuleTemplateNameResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.UpdateApprovalRuleTemplateName"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateNameResponse'
            Core.<$> (x Core..: "approvalRuleTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateApprovalRuleTemplateNameResponse' smart constructor.
data UpdateApprovalRuleTemplateNameResponse = UpdateApprovalRuleTemplateNameResponse'
  { -- | The structure and content of the updated approval rule template.
    approvalRuleTemplate :: Types.ApprovalRuleTemplate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateApprovalRuleTemplateNameResponse' value with any optional fields omitted.
mkUpdateApprovalRuleTemplateNameResponse ::
  -- | 'approvalRuleTemplate'
  Types.ApprovalRuleTemplate ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateApprovalRuleTemplateNameResponse
mkUpdateApprovalRuleTemplateNameResponse
  approvalRuleTemplate
  responseStatus =
    UpdateApprovalRuleTemplateNameResponse'
      { approvalRuleTemplate,
        responseStatus
      }

-- | The structure and content of the updated approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnrrsApprovalRuleTemplate :: Lens.Lens' UpdateApprovalRuleTemplateNameResponse Types.ApprovalRuleTemplate
uartnrrsApprovalRuleTemplate = Lens.field @"approvalRuleTemplate"
{-# DEPRECATED uartnrrsApprovalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnrrsResponseStatus :: Lens.Lens' UpdateApprovalRuleTemplateNameResponse Core.Int
uartnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uartnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
