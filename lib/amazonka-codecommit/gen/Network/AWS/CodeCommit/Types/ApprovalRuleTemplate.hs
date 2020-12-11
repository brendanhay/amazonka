-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRuleTemplate
  ( ApprovalRuleTemplate (..),

    -- * Smart constructor
    mkApprovalRuleTemplate,

    -- * Lenses
    artRuleContentSha256,
    artApprovalRuleTemplateId,
    artLastModifiedDate,
    artApprovalRuleTemplateDescription,
    artApprovalRuleTemplateContent,
    artLastModifiedUser,
    artCreationDate,
    artApprovalRuleTemplateName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about an approval rule template.
--
-- /See:/ 'mkApprovalRuleTemplate' smart constructor.
data ApprovalRuleTemplate = ApprovalRuleTemplate'
  { ruleContentSha256 ::
      Lude.Maybe Lude.Text,
    approvalRuleTemplateId :: Lude.Maybe Lude.Text,
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    approvalRuleTemplateDescription ::
      Lude.Maybe Lude.Text,
    approvalRuleTemplateContent ::
      Lude.Maybe Lude.Text,
    lastModifiedUser :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    approvalRuleTemplateName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApprovalRuleTemplate' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplateContent' - The content of the approval rule template.
-- * 'approvalRuleTemplateDescription' - The description of the approval rule template.
-- * 'approvalRuleTemplateId' - The system-generated ID of the approval rule template.
-- * 'approvalRuleTemplateName' - The name of the approval rule template.
-- * 'creationDate' - The date the approval rule template was created, in timestamp format.
-- * 'lastModifiedDate' - The date the approval rule template was most recently changed, in timestamp format.
-- * 'lastModifiedUser' - The Amazon Resource Name (ARN) of the user who made the most recent changes to the approval rule template.
-- * 'ruleContentSha256' - The SHA-256 hash signature for the content of the approval rule template.
mkApprovalRuleTemplate ::
  ApprovalRuleTemplate
mkApprovalRuleTemplate =
  ApprovalRuleTemplate'
    { ruleContentSha256 = Lude.Nothing,
      approvalRuleTemplateId = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      approvalRuleTemplateDescription = Lude.Nothing,
      approvalRuleTemplateContent = Lude.Nothing,
      lastModifiedUser = Lude.Nothing,
      creationDate = Lude.Nothing,
      approvalRuleTemplateName = Lude.Nothing
    }

-- | The SHA-256 hash signature for the content of the approval rule template.
--
-- /Note:/ Consider using 'ruleContentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artRuleContentSha256 :: Lens.Lens' ApprovalRuleTemplate (Lude.Maybe Lude.Text)
artRuleContentSha256 = Lens.lens (ruleContentSha256 :: ApprovalRuleTemplate -> Lude.Maybe Lude.Text) (\s a -> s {ruleContentSha256 = a} :: ApprovalRuleTemplate)
{-# DEPRECATED artRuleContentSha256 "Use generic-lens or generic-optics with 'ruleContentSha256' instead." #-}

-- | The system-generated ID of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artApprovalRuleTemplateId :: Lens.Lens' ApprovalRuleTemplate (Lude.Maybe Lude.Text)
artApprovalRuleTemplateId = Lens.lens (approvalRuleTemplateId :: ApprovalRuleTemplate -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleTemplateId = a} :: ApprovalRuleTemplate)
{-# DEPRECATED artApprovalRuleTemplateId "Use generic-lens or generic-optics with 'approvalRuleTemplateId' instead." #-}

-- | The date the approval rule template was most recently changed, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artLastModifiedDate :: Lens.Lens' ApprovalRuleTemplate (Lude.Maybe Lude.Timestamp)
artLastModifiedDate = Lens.lens (lastModifiedDate :: ApprovalRuleTemplate -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: ApprovalRuleTemplate)
{-# DEPRECATED artLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The description of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artApprovalRuleTemplateDescription :: Lens.Lens' ApprovalRuleTemplate (Lude.Maybe Lude.Text)
artApprovalRuleTemplateDescription = Lens.lens (approvalRuleTemplateDescription :: ApprovalRuleTemplate -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleTemplateDescription = a} :: ApprovalRuleTemplate)
{-# DEPRECATED artApprovalRuleTemplateDescription "Use generic-lens or generic-optics with 'approvalRuleTemplateDescription' instead." #-}

-- | The content of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artApprovalRuleTemplateContent :: Lens.Lens' ApprovalRuleTemplate (Lude.Maybe Lude.Text)
artApprovalRuleTemplateContent = Lens.lens (approvalRuleTemplateContent :: ApprovalRuleTemplate -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleTemplateContent = a} :: ApprovalRuleTemplate)
{-# DEPRECATED artApprovalRuleTemplateContent "Use generic-lens or generic-optics with 'approvalRuleTemplateContent' instead." #-}

-- | The Amazon Resource Name (ARN) of the user who made the most recent changes to the approval rule template.
--
-- /Note:/ Consider using 'lastModifiedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artLastModifiedUser :: Lens.Lens' ApprovalRuleTemplate (Lude.Maybe Lude.Text)
artLastModifiedUser = Lens.lens (lastModifiedUser :: ApprovalRuleTemplate -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedUser = a} :: ApprovalRuleTemplate)
{-# DEPRECATED artLastModifiedUser "Use generic-lens or generic-optics with 'lastModifiedUser' instead." #-}

-- | The date the approval rule template was created, in timestamp format.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artCreationDate :: Lens.Lens' ApprovalRuleTemplate (Lude.Maybe Lude.Timestamp)
artCreationDate = Lens.lens (creationDate :: ApprovalRuleTemplate -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: ApprovalRuleTemplate)
{-# DEPRECATED artCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The name of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artApprovalRuleTemplateName :: Lens.Lens' ApprovalRuleTemplate (Lude.Maybe Lude.Text)
artApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: ApprovalRuleTemplate -> Lude.Maybe Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: ApprovalRuleTemplate)
{-# DEPRECATED artApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

instance Lude.FromJSON ApprovalRuleTemplate where
  parseJSON =
    Lude.withObject
      "ApprovalRuleTemplate"
      ( \x ->
          ApprovalRuleTemplate'
            Lude.<$> (x Lude..:? "ruleContentSha256")
            Lude.<*> (x Lude..:? "approvalRuleTemplateId")
            Lude.<*> (x Lude..:? "lastModifiedDate")
            Lude.<*> (x Lude..:? "approvalRuleTemplateDescription")
            Lude.<*> (x Lude..:? "approvalRuleTemplateContent")
            Lude.<*> (x Lude..:? "lastModifiedUser")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "approvalRuleTemplateName")
      )
