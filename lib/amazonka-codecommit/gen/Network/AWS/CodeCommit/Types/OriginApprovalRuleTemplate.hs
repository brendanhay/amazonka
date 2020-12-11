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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about the template that created the approval rule for a pull request.
--
-- /See:/ 'mkOriginApprovalRuleTemplate' smart constructor.
data OriginApprovalRuleTemplate = OriginApprovalRuleTemplate'
  { approvalRuleTemplateId ::
      Lude.Maybe Lude.Text,
    approvalRuleTemplateName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginApprovalRuleTemplate' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplateId' - The ID of the template that created the approval rule.
-- * 'approvalRuleTemplateName' - The name of the template that created the approval rule.
mkOriginApprovalRuleTemplate ::
  OriginApprovalRuleTemplate
mkOriginApprovalRuleTemplate =
  OriginApprovalRuleTemplate'
    { approvalRuleTemplateId =
        Lude.Nothing,
      approvalRuleTemplateName = Lude.Nothing
    }

-- | The ID of the template that created the approval rule.
--
-- /Note:/ Consider using 'approvalRuleTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oartApprovalRuleTemplateId :: Lens.Lens' OriginApprovalRuleTemplate (Lude.Maybe Lude.Text)
oartApprovalRuleTemplateId = Lens.lens (approvalRuleTemplateId :: OriginApprovalRuleTemplate -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleTemplateId = a} :: OriginApprovalRuleTemplate)
{-# DEPRECATED oartApprovalRuleTemplateId "Use generic-lens or generic-optics with 'approvalRuleTemplateId' instead." #-}

-- | The name of the template that created the approval rule.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oartApprovalRuleTemplateName :: Lens.Lens' OriginApprovalRuleTemplate (Lude.Maybe Lude.Text)
oartApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: OriginApprovalRuleTemplate -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: OriginApprovalRuleTemplate)
{-# DEPRECATED oartApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

instance Lude.FromJSON OriginApprovalRuleTemplate where
  parseJSON =
    Lude.withObject
      "OriginApprovalRuleTemplate"
      ( \x ->
          OriginApprovalRuleTemplate'
            Lude.<$> (x Lude..:? "approvalRuleTemplateId")
            Lude.<*> (x Lude..:? "approvalRuleTemplateName")
      )
