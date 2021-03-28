{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
  ( ApprovalRuleEventMetadata (..)
  -- * Smart constructor
  , mkApprovalRuleEventMetadata
  -- * Lenses
  , aremApprovalRuleContent
  , aremApprovalRuleId
  , aremApprovalRuleName
  ) where

import qualified Network.AWS.CodeCommit.Types.ApprovalRuleContent as Types
import qualified Network.AWS.CodeCommit.Types.ApprovalRuleId as Types
import qualified Network.AWS.CodeCommit.Types.ApprovalRuleName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about an event for an approval rule.
--
-- /See:/ 'mkApprovalRuleEventMetadata' smart constructor.
data ApprovalRuleEventMetadata = ApprovalRuleEventMetadata'
  { approvalRuleContent :: Core.Maybe Types.ApprovalRuleContent
    -- ^ The content of the approval rule.
  , approvalRuleId :: Core.Maybe Types.ApprovalRuleId
    -- ^ The system-generated ID of the approval rule.
  , approvalRuleName :: Core.Maybe Types.ApprovalRuleName
    -- ^ The name of the approval rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApprovalRuleEventMetadata' value with any optional fields omitted.
mkApprovalRuleEventMetadata
    :: ApprovalRuleEventMetadata
mkApprovalRuleEventMetadata
  = ApprovalRuleEventMetadata'{approvalRuleContent = Core.Nothing,
                               approvalRuleId = Core.Nothing, approvalRuleName = Core.Nothing}

-- | The content of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aremApprovalRuleContent :: Lens.Lens' ApprovalRuleEventMetadata (Core.Maybe Types.ApprovalRuleContent)
aremApprovalRuleContent = Lens.field @"approvalRuleContent"
{-# INLINEABLE aremApprovalRuleContent #-}
{-# DEPRECATED approvalRuleContent "Use generic-lens or generic-optics with 'approvalRuleContent' instead"  #-}

-- | The system-generated ID of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aremApprovalRuleId :: Lens.Lens' ApprovalRuleEventMetadata (Core.Maybe Types.ApprovalRuleId)
aremApprovalRuleId = Lens.field @"approvalRuleId"
{-# INLINEABLE aremApprovalRuleId #-}
{-# DEPRECATED approvalRuleId "Use generic-lens or generic-optics with 'approvalRuleId' instead"  #-}

-- | The name of the approval rule.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aremApprovalRuleName :: Lens.Lens' ApprovalRuleEventMetadata (Core.Maybe Types.ApprovalRuleName)
aremApprovalRuleName = Lens.field @"approvalRuleName"
{-# INLINEABLE aremApprovalRuleName #-}
{-# DEPRECATED approvalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead"  #-}

instance Core.FromJSON ApprovalRuleEventMetadata where
        parseJSON
          = Core.withObject "ApprovalRuleEventMetadata" Core.$
              \ x ->
                ApprovalRuleEventMetadata' Core.<$>
                  (x Core..:? "approvalRuleContent") Core.<*>
                    x Core..:? "approvalRuleId"
                    Core.<*> x Core..:? "approvalRuleName"
