-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRule
  ( ApprovalRule (..),

    -- * Smart constructor
    mkApprovalRule,

    -- * Lenses
    arRuleContentSha256,
    arLastModifiedDate,
    arApprovalRuleName,
    arApprovalRuleId,
    arLastModifiedUser,
    arOriginApprovalRuleTemplate,
    arCreationDate,
    arApprovalRuleContent,
  )
where

import Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about an approval rule.
--
-- /See:/ 'mkApprovalRule' smart constructor.
data ApprovalRule = ApprovalRule'
  { ruleContentSha256 ::
      Lude.Maybe Lude.Text,
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    approvalRuleName :: Lude.Maybe Lude.Text,
    approvalRuleId :: Lude.Maybe Lude.Text,
    lastModifiedUser :: Lude.Maybe Lude.Text,
    originApprovalRuleTemplate ::
      Lude.Maybe OriginApprovalRuleTemplate,
    creationDate :: Lude.Maybe Lude.Timestamp,
    approvalRuleContent :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApprovalRule' with the minimum fields required to make a request.
--
-- * 'approvalRuleContent' - The content of the approval rule.
-- * 'approvalRuleId' - The system-generated ID of the approval rule.
-- * 'approvalRuleName' - The name of the approval rule.
-- * 'creationDate' - The date the approval rule was created, in timestamp format.
-- * 'lastModifiedDate' - The date the approval rule was most recently changed, in timestamp format.
-- * 'lastModifiedUser' - The Amazon Resource Name (ARN) of the user who made the most recent changes to the approval rule.
-- * 'originApprovalRuleTemplate' - The approval rule template used to create the rule.
-- * 'ruleContentSha256' - The SHA-256 hash signature for the content of the approval rule.
mkApprovalRule ::
  ApprovalRule
mkApprovalRule =
  ApprovalRule'
    { ruleContentSha256 = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      approvalRuleName = Lude.Nothing,
      approvalRuleId = Lude.Nothing,
      lastModifiedUser = Lude.Nothing,
      originApprovalRuleTemplate = Lude.Nothing,
      creationDate = Lude.Nothing,
      approvalRuleContent = Lude.Nothing
    }

-- | The SHA-256 hash signature for the content of the approval rule.
--
-- /Note:/ Consider using 'ruleContentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRuleContentSha256 :: Lens.Lens' ApprovalRule (Lude.Maybe Lude.Text)
arRuleContentSha256 = Lens.lens (ruleContentSha256 :: ApprovalRule -> Lude.Maybe Lude.Text) (\s a -> s {ruleContentSha256 = a} :: ApprovalRule)
{-# DEPRECATED arRuleContentSha256 "Use generic-lens or generic-optics with 'ruleContentSha256' instead." #-}

-- | The date the approval rule was most recently changed, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arLastModifiedDate :: Lens.Lens' ApprovalRule (Lude.Maybe Lude.Timestamp)
arLastModifiedDate = Lens.lens (lastModifiedDate :: ApprovalRule -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: ApprovalRule)
{-# DEPRECATED arLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The name of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arApprovalRuleName :: Lens.Lens' ApprovalRule (Lude.Maybe Lude.Text)
arApprovalRuleName = Lens.lens (approvalRuleName :: ApprovalRule -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleName = a} :: ApprovalRule)
{-# DEPRECATED arApprovalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead." #-}

-- | The system-generated ID of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arApprovalRuleId :: Lens.Lens' ApprovalRule (Lude.Maybe Lude.Text)
arApprovalRuleId = Lens.lens (approvalRuleId :: ApprovalRule -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleId = a} :: ApprovalRule)
{-# DEPRECATED arApprovalRuleId "Use generic-lens or generic-optics with 'approvalRuleId' instead." #-}

-- | The Amazon Resource Name (ARN) of the user who made the most recent changes to the approval rule.
--
-- /Note:/ Consider using 'lastModifiedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arLastModifiedUser :: Lens.Lens' ApprovalRule (Lude.Maybe Lude.Text)
arLastModifiedUser = Lens.lens (lastModifiedUser :: ApprovalRule -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedUser = a} :: ApprovalRule)
{-# DEPRECATED arLastModifiedUser "Use generic-lens or generic-optics with 'lastModifiedUser' instead." #-}

-- | The approval rule template used to create the rule.
--
-- /Note:/ Consider using 'originApprovalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arOriginApprovalRuleTemplate :: Lens.Lens' ApprovalRule (Lude.Maybe OriginApprovalRuleTemplate)
arOriginApprovalRuleTemplate = Lens.lens (originApprovalRuleTemplate :: ApprovalRule -> Lude.Maybe OriginApprovalRuleTemplate) (\s a -> s {originApprovalRuleTemplate = a} :: ApprovalRule)
{-# DEPRECATED arOriginApprovalRuleTemplate "Use generic-lens or generic-optics with 'originApprovalRuleTemplate' instead." #-}

-- | The date the approval rule was created, in timestamp format.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCreationDate :: Lens.Lens' ApprovalRule (Lude.Maybe Lude.Timestamp)
arCreationDate = Lens.lens (creationDate :: ApprovalRule -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: ApprovalRule)
{-# DEPRECATED arCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The content of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arApprovalRuleContent :: Lens.Lens' ApprovalRule (Lude.Maybe Lude.Text)
arApprovalRuleContent = Lens.lens (approvalRuleContent :: ApprovalRule -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleContent = a} :: ApprovalRule)
{-# DEPRECATED arApprovalRuleContent "Use generic-lens or generic-optics with 'approvalRuleContent' instead." #-}

instance Lude.FromJSON ApprovalRule where
  parseJSON =
    Lude.withObject
      "ApprovalRule"
      ( \x ->
          ApprovalRule'
            Lude.<$> (x Lude..:? "ruleContentSha256")
            Lude.<*> (x Lude..:? "lastModifiedDate")
            Lude.<*> (x Lude..:? "approvalRuleName")
            Lude.<*> (x Lude..:? "approvalRuleId")
            Lude.<*> (x Lude..:? "lastModifiedUser")
            Lude.<*> (x Lude..:? "originApprovalRuleTemplate")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "approvalRuleContent")
      )
