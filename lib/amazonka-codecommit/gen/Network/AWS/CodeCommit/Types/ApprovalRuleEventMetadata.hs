-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
  ( ApprovalRuleEventMetadata (..),

    -- * Smart constructor
    mkApprovalRuleEventMetadata,

    -- * Lenses
    aremApprovalRuleName,
    aremApprovalRuleId,
    aremApprovalRuleContent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about an event for an approval rule.
--
-- /See:/ 'mkApprovalRuleEventMetadata' smart constructor.
data ApprovalRuleEventMetadata = ApprovalRuleEventMetadata'
  { approvalRuleName ::
      Lude.Maybe Lude.Text,
    approvalRuleId :: Lude.Maybe Lude.Text,
    approvalRuleContent ::
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

-- | Creates a value of 'ApprovalRuleEventMetadata' with the minimum fields required to make a request.
--
-- * 'approvalRuleContent' - The content of the approval rule.
-- * 'approvalRuleId' - The system-generated ID of the approval rule.
-- * 'approvalRuleName' - The name of the approval rule.
mkApprovalRuleEventMetadata ::
  ApprovalRuleEventMetadata
mkApprovalRuleEventMetadata =
  ApprovalRuleEventMetadata'
    { approvalRuleName = Lude.Nothing,
      approvalRuleId = Lude.Nothing,
      approvalRuleContent = Lude.Nothing
    }

-- | The name of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aremApprovalRuleName :: Lens.Lens' ApprovalRuleEventMetadata (Lude.Maybe Lude.Text)
aremApprovalRuleName = Lens.lens (approvalRuleName :: ApprovalRuleEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleName = a} :: ApprovalRuleEventMetadata)
{-# DEPRECATED aremApprovalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead." #-}

-- | The system-generated ID of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aremApprovalRuleId :: Lens.Lens' ApprovalRuleEventMetadata (Lude.Maybe Lude.Text)
aremApprovalRuleId = Lens.lens (approvalRuleId :: ApprovalRuleEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleId = a} :: ApprovalRuleEventMetadata)
{-# DEPRECATED aremApprovalRuleId "Use generic-lens or generic-optics with 'approvalRuleId' instead." #-}

-- | The content of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aremApprovalRuleContent :: Lens.Lens' ApprovalRuleEventMetadata (Lude.Maybe Lude.Text)
aremApprovalRuleContent = Lens.lens (approvalRuleContent :: ApprovalRuleEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleContent = a} :: ApprovalRuleEventMetadata)
{-# DEPRECATED aremApprovalRuleContent "Use generic-lens or generic-optics with 'approvalRuleContent' instead." #-}

instance Lude.FromJSON ApprovalRuleEventMetadata where
  parseJSON =
    Lude.withObject
      "ApprovalRuleEventMetadata"
      ( \x ->
          ApprovalRuleEventMetadata'
            Lude.<$> (x Lude..:? "approvalRuleName")
            Lude.<*> (x Lude..:? "approvalRuleId")
            Lude.<*> (x Lude..:? "approvalRuleContent")
      )
