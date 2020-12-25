{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate
  ( OriginApprovalRuleTemplate (..),

    -- * Smart constructor
    mkOriginApprovalRuleTemplate,

    -- * Lenses
    oartApprovalRuleTemplateId,
    oartApprovalRuleTemplateName,
  )
where

import qualified Network.AWS.CodeCommit.Types.ApprovalRuleTemplateId as Types
import qualified Network.AWS.CodeCommit.Types.ApprovalRuleTemplateName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about the template that created the approval rule for a pull request.
--
-- /See:/ 'mkOriginApprovalRuleTemplate' smart constructor.
data OriginApprovalRuleTemplate = OriginApprovalRuleTemplate'
  { -- | The ID of the template that created the approval rule.
    approvalRuleTemplateId :: Core.Maybe Types.ApprovalRuleTemplateId,
    -- | The name of the template that created the approval rule.
    approvalRuleTemplateName :: Core.Maybe Types.ApprovalRuleTemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginApprovalRuleTemplate' value with any optional fields omitted.
mkOriginApprovalRuleTemplate ::
  OriginApprovalRuleTemplate
mkOriginApprovalRuleTemplate =
  OriginApprovalRuleTemplate'
    { approvalRuleTemplateId =
        Core.Nothing,
      approvalRuleTemplateName = Core.Nothing
    }

-- | The ID of the template that created the approval rule.
--
-- /Note:/ Consider using 'approvalRuleTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oartApprovalRuleTemplateId :: Lens.Lens' OriginApprovalRuleTemplate (Core.Maybe Types.ApprovalRuleTemplateId)
oartApprovalRuleTemplateId = Lens.field @"approvalRuleTemplateId"
{-# DEPRECATED oartApprovalRuleTemplateId "Use generic-lens or generic-optics with 'approvalRuleTemplateId' instead." #-}

-- | The name of the template that created the approval rule.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oartApprovalRuleTemplateName :: Lens.Lens' OriginApprovalRuleTemplate (Core.Maybe Types.ApprovalRuleTemplateName)
oartApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# DEPRECATED oartApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

instance Core.FromJSON OriginApprovalRuleTemplate where
  parseJSON =
    Core.withObject "OriginApprovalRuleTemplate" Core.$
      \x ->
        OriginApprovalRuleTemplate'
          Core.<$> (x Core..:? "approvalRuleTemplateId")
          Core.<*> (x Core..:? "approvalRuleTemplateName")
