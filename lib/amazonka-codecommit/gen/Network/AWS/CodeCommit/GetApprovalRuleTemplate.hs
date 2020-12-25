{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified approval rule template.
module Network.AWS.CodeCommit.GetApprovalRuleTemplate
  ( -- * Creating a request
    GetApprovalRuleTemplate (..),
    mkGetApprovalRuleTemplate,

    -- ** Request lenses
    gartApprovalRuleTemplateName,

    -- * Destructuring the response
    GetApprovalRuleTemplateResponse (..),
    mkGetApprovalRuleTemplateResponse,

    -- ** Response lenses
    gartrrsApprovalRuleTemplate,
    gartrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetApprovalRuleTemplate' smart constructor.
newtype GetApprovalRuleTemplate = GetApprovalRuleTemplate'
  { -- | The name of the approval rule template for which you want to get information.
    approvalRuleTemplateName :: Types.ApprovalRuleTemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApprovalRuleTemplate' value with any optional fields omitted.
mkGetApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  GetApprovalRuleTemplate
mkGetApprovalRuleTemplate approvalRuleTemplateName =
  GetApprovalRuleTemplate' {approvalRuleTemplateName}

-- | The name of the approval rule template for which you want to get information.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gartApprovalRuleTemplateName :: Lens.Lens' GetApprovalRuleTemplate Types.ApprovalRuleTemplateName
gartApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# DEPRECATED gartApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

instance Core.FromJSON GetApprovalRuleTemplate where
  toJSON GetApprovalRuleTemplate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("approvalRuleTemplateName" Core..= approvalRuleTemplateName)
          ]
      )

instance Core.AWSRequest GetApprovalRuleTemplate where
  type Rs GetApprovalRuleTemplate = GetApprovalRuleTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.GetApprovalRuleTemplate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApprovalRuleTemplateResponse'
            Core.<$> (x Core..: "approvalRuleTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetApprovalRuleTemplateResponse' smart constructor.
data GetApprovalRuleTemplateResponse = GetApprovalRuleTemplateResponse'
  { -- | The content and structure of the approval rule template.
    approvalRuleTemplate :: Types.ApprovalRuleTemplate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetApprovalRuleTemplateResponse' value with any optional fields omitted.
mkGetApprovalRuleTemplateResponse ::
  -- | 'approvalRuleTemplate'
  Types.ApprovalRuleTemplate ->
  -- | 'responseStatus'
  Core.Int ->
  GetApprovalRuleTemplateResponse
mkGetApprovalRuleTemplateResponse
  approvalRuleTemplate
  responseStatus =
    GetApprovalRuleTemplateResponse'
      { approvalRuleTemplate,
        responseStatus
      }

-- | The content and structure of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gartrrsApprovalRuleTemplate :: Lens.Lens' GetApprovalRuleTemplateResponse Types.ApprovalRuleTemplate
gartrrsApprovalRuleTemplate = Lens.field @"approvalRuleTemplate"
{-# DEPRECATED gartrrsApprovalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gartrrsResponseStatus :: Lens.Lens' GetApprovalRuleTemplateResponse Core.Int
gartrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gartrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
