{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ApprovalRule
  ( ApprovalRule (..)
  -- * Smart constructor
  , mkApprovalRule
  -- * Lenses
  , arApprovalRuleContent
  , arApprovalRuleId
  , arApprovalRuleName
  , arCreationDate
  , arLastModifiedDate
  , arLastModifiedUser
  , arOriginApprovalRuleTemplate
  , arRuleContentSha256
  ) where

import qualified Network.AWS.CodeCommit.Types.ApprovalRuleContent as Types
import qualified Network.AWS.CodeCommit.Types.ApprovalRuleId as Types
import qualified Network.AWS.CodeCommit.Types.ApprovalRuleName as Types
import qualified Network.AWS.CodeCommit.Types.Arn as Types
import qualified Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate as Types
import qualified Network.AWS.CodeCommit.Types.RuleContentSha256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about an approval rule.
--
-- /See:/ 'mkApprovalRule' smart constructor.
data ApprovalRule = ApprovalRule'
  { approvalRuleContent :: Core.Maybe Types.ApprovalRuleContent
    -- ^ The content of the approval rule.
  , approvalRuleId :: Core.Maybe Types.ApprovalRuleId
    -- ^ The system-generated ID of the approval rule.
  , approvalRuleName :: Core.Maybe Types.ApprovalRuleName
    -- ^ The name of the approval rule.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the approval rule was created, in timestamp format.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the approval rule was most recently changed, in timestamp format.
  , lastModifiedUser :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the user who made the most recent changes to the approval rule.
  , originApprovalRuleTemplate :: Core.Maybe Types.OriginApprovalRuleTemplate
    -- ^ The approval rule template used to create the rule.
  , ruleContentSha256 :: Core.Maybe Types.RuleContentSha256
    -- ^ The SHA-256 hash signature for the content of the approval rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ApprovalRule' value with any optional fields omitted.
mkApprovalRule
    :: ApprovalRule
mkApprovalRule
  = ApprovalRule'{approvalRuleContent = Core.Nothing,
                  approvalRuleId = Core.Nothing, approvalRuleName = Core.Nothing,
                  creationDate = Core.Nothing, lastModifiedDate = Core.Nothing,
                  lastModifiedUser = Core.Nothing,
                  originApprovalRuleTemplate = Core.Nothing,
                  ruleContentSha256 = Core.Nothing}

-- | The content of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arApprovalRuleContent :: Lens.Lens' ApprovalRule (Core.Maybe Types.ApprovalRuleContent)
arApprovalRuleContent = Lens.field @"approvalRuleContent"
{-# INLINEABLE arApprovalRuleContent #-}
{-# DEPRECATED approvalRuleContent "Use generic-lens or generic-optics with 'approvalRuleContent' instead"  #-}

-- | The system-generated ID of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arApprovalRuleId :: Lens.Lens' ApprovalRule (Core.Maybe Types.ApprovalRuleId)
arApprovalRuleId = Lens.field @"approvalRuleId"
{-# INLINEABLE arApprovalRuleId #-}
{-# DEPRECATED approvalRuleId "Use generic-lens or generic-optics with 'approvalRuleId' instead"  #-}

-- | The name of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arApprovalRuleName :: Lens.Lens' ApprovalRule (Core.Maybe Types.ApprovalRuleName)
arApprovalRuleName = Lens.field @"approvalRuleName"
{-# INLINEABLE arApprovalRuleName #-}
{-# DEPRECATED approvalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead"  #-}

-- | The date the approval rule was created, in timestamp format.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCreationDate :: Lens.Lens' ApprovalRule (Core.Maybe Core.NominalDiffTime)
arCreationDate = Lens.field @"creationDate"
{-# INLINEABLE arCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The date the approval rule was most recently changed, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arLastModifiedDate :: Lens.Lens' ApprovalRule (Core.Maybe Core.NominalDiffTime)
arLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE arLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The Amazon Resource Name (ARN) of the user who made the most recent changes to the approval rule.
--
-- /Note:/ Consider using 'lastModifiedUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arLastModifiedUser :: Lens.Lens' ApprovalRule (Core.Maybe Types.Arn)
arLastModifiedUser = Lens.field @"lastModifiedUser"
{-# INLINEABLE arLastModifiedUser #-}
{-# DEPRECATED lastModifiedUser "Use generic-lens or generic-optics with 'lastModifiedUser' instead"  #-}

-- | The approval rule template used to create the rule.
--
-- /Note:/ Consider using 'originApprovalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arOriginApprovalRuleTemplate :: Lens.Lens' ApprovalRule (Core.Maybe Types.OriginApprovalRuleTemplate)
arOriginApprovalRuleTemplate = Lens.field @"originApprovalRuleTemplate"
{-# INLINEABLE arOriginApprovalRuleTemplate #-}
{-# DEPRECATED originApprovalRuleTemplate "Use generic-lens or generic-optics with 'originApprovalRuleTemplate' instead"  #-}

-- | The SHA-256 hash signature for the content of the approval rule.
--
-- /Note:/ Consider using 'ruleContentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRuleContentSha256 :: Lens.Lens' ApprovalRule (Core.Maybe Types.RuleContentSha256)
arRuleContentSha256 = Lens.field @"ruleContentSha256"
{-# INLINEABLE arRuleContentSha256 #-}
{-# DEPRECATED ruleContentSha256 "Use generic-lens or generic-optics with 'ruleContentSha256' instead"  #-}

instance Core.FromJSON ApprovalRule where
        parseJSON
          = Core.withObject "ApprovalRule" Core.$
              \ x ->
                ApprovalRule' Core.<$>
                  (x Core..:? "approvalRuleContent") Core.<*>
                    x Core..:? "approvalRuleId"
                    Core.<*> x Core..:? "approvalRuleName"
                    Core.<*> x Core..:? "creationDate"
                    Core.<*> x Core..:? "lastModifiedDate"
                    Core.<*> x Core..:? "lastModifiedUser"
                    Core.<*> x Core..:? "originApprovalRuleTemplate"
                    Core.<*> x Core..:? "ruleContentSha256"
