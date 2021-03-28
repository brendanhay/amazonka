{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ApprovalRuleTemplate
  ( ApprovalRuleTemplate (..)
  -- * Smart constructor
  , mkApprovalRuleTemplate
  -- * Lenses
  , artApprovalRuleTemplateContent
  , artApprovalRuleTemplateDescription
  , artApprovalRuleTemplateId
  , artApprovalRuleTemplateName
  , artCreationDate
  , artLastModifiedDate
  , artLastModifiedUser
  , artRuleContentSha256
  ) where

import qualified Network.AWS.CodeCommit.Types.ApprovalRuleTemplateContent as Types
import qualified Network.AWS.CodeCommit.Types.ApprovalRuleTemplateDescription as Types
import qualified Network.AWS.CodeCommit.Types.ApprovalRuleTemplateId as Types
import qualified Network.AWS.CodeCommit.Types.ApprovalRuleTemplateName as Types
import qualified Network.AWS.CodeCommit.Types.Arn as Types
import qualified Network.AWS.CodeCommit.Types.RuleContentSha256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about an approval rule template.
--
-- /See:/ 'mkApprovalRuleTemplate' smart constructor.
data ApprovalRuleTemplate = ApprovalRuleTemplate'
  { approvalRuleTemplateContent :: Core.Maybe Types.ApprovalRuleTemplateContent
    -- ^ The content of the approval rule template.
  , approvalRuleTemplateDescription :: Core.Maybe Types.ApprovalRuleTemplateDescription
    -- ^ The description of the approval rule template.
  , approvalRuleTemplateId :: Core.Maybe Types.ApprovalRuleTemplateId
    -- ^ The system-generated ID of the approval rule template.
  , approvalRuleTemplateName :: Core.Maybe Types.ApprovalRuleTemplateName
    -- ^ The name of the approval rule template.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the approval rule template was created, in timestamp format.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the approval rule template was most recently changed, in timestamp format.
  , lastModifiedUser :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the user who made the most recent changes to the approval rule template.
  , ruleContentSha256 :: Core.Maybe Types.RuleContentSha256
    -- ^ The SHA-256 hash signature for the content of the approval rule template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ApprovalRuleTemplate' value with any optional fields omitted.
mkApprovalRuleTemplate
    :: ApprovalRuleTemplate
mkApprovalRuleTemplate
  = ApprovalRuleTemplate'{approvalRuleTemplateContent = Core.Nothing,
                          approvalRuleTemplateDescription = Core.Nothing,
                          approvalRuleTemplateId = Core.Nothing,
                          approvalRuleTemplateName = Core.Nothing,
                          creationDate = Core.Nothing, lastModifiedDate = Core.Nothing,
                          lastModifiedUser = Core.Nothing, ruleContentSha256 = Core.Nothing}

-- | The content of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artApprovalRuleTemplateContent :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Types.ApprovalRuleTemplateContent)
artApprovalRuleTemplateContent = Lens.field @"approvalRuleTemplateContent"
{-# INLINEABLE artApprovalRuleTemplateContent #-}
{-# DEPRECATED approvalRuleTemplateContent "Use generic-lens or generic-optics with 'approvalRuleTemplateContent' instead"  #-}

-- | The description of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artApprovalRuleTemplateDescription :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Types.ApprovalRuleTemplateDescription)
artApprovalRuleTemplateDescription = Lens.field @"approvalRuleTemplateDescription"
{-# INLINEABLE artApprovalRuleTemplateDescription #-}
{-# DEPRECATED approvalRuleTemplateDescription "Use generic-lens or generic-optics with 'approvalRuleTemplateDescription' instead"  #-}

-- | The system-generated ID of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artApprovalRuleTemplateId :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Types.ApprovalRuleTemplateId)
artApprovalRuleTemplateId = Lens.field @"approvalRuleTemplateId"
{-# INLINEABLE artApprovalRuleTemplateId #-}
{-# DEPRECATED approvalRuleTemplateId "Use generic-lens or generic-optics with 'approvalRuleTemplateId' instead"  #-}

-- | The name of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artApprovalRuleTemplateName :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Types.ApprovalRuleTemplateName)
artApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# INLINEABLE artApprovalRuleTemplateName #-}
{-# DEPRECATED approvalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead"  #-}

-- | The date the approval rule template was created, in timestamp format.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artCreationDate :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Core.NominalDiffTime)
artCreationDate = Lens.field @"creationDate"
{-# INLINEABLE artCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The date the approval rule template was most recently changed, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artLastModifiedDate :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Core.NominalDiffTime)
artLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE artLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The Amazon Resource Name (ARN) of the user who made the most recent changes to the approval rule template.
--
-- /Note:/ Consider using 'lastModifiedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artLastModifiedUser :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Types.Arn)
artLastModifiedUser = Lens.field @"lastModifiedUser"
{-# INLINEABLE artLastModifiedUser #-}
{-# DEPRECATED lastModifiedUser "Use generic-lens or generic-optics with 'lastModifiedUser' instead"  #-}

-- | The SHA-256 hash signature for the content of the approval rule template.
--
-- /Note:/ Consider using 'ruleContentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artRuleContentSha256 :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Types.RuleContentSha256)
artRuleContentSha256 = Lens.field @"ruleContentSha256"
{-# INLINEABLE artRuleContentSha256 #-}
{-# DEPRECATED ruleContentSha256 "Use generic-lens or generic-optics with 'ruleContentSha256' instead"  #-}

instance Core.FromJSON ApprovalRuleTemplate where
        parseJSON
          = Core.withObject "ApprovalRuleTemplate" Core.$
              \ x ->
                ApprovalRuleTemplate' Core.<$>
                  (x Core..:? "approvalRuleTemplateContent") Core.<*>
                    x Core..:? "approvalRuleTemplateDescription"
                    Core.<*> x Core..:? "approvalRuleTemplateId"
                    Core.<*> x Core..:? "approvalRuleTemplateName"
                    Core.<*> x Core..:? "creationDate"
                    Core.<*> x Core..:? "lastModifiedDate"
                    Core.<*> x Core..:? "lastModifiedUser"
                    Core.<*> x Core..:? "ruleContentSha256"
