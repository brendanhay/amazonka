{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchRule
  ( PatchRule (..)
  -- * Smart constructor
  , mkPatchRule
  -- * Lenses
  , prPatchFilterGroup
  , prApproveAfterDays
  , prApproveUntilDate
  , prComplianceLevel
  , prEnableNonSecurity
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.PatchComplianceLevel as Types
import qualified Network.AWS.SSM.Types.PatchFilterGroup as Types
import qualified Network.AWS.SSM.Types.PatchStringDateTime as Types

-- | Defines an approval rule for a patch baseline.
--
-- /See:/ 'mkPatchRule' smart constructor.
data PatchRule = PatchRule'
  { patchFilterGroup :: Types.PatchFilterGroup
    -- ^ The patch filter group that defines the criteria for the rule.
  , approveAfterDays :: Core.Maybe Core.Natural
    -- ^ The number of days after the release date of each patch matched by the rule that the patch is marked as approved in the patch baseline. For example, a value of @7@ means that patches are approved seven days after they are released. Not supported on Ubuntu Server.
  , approveUntilDate :: Core.Maybe Types.PatchStringDateTime
    -- ^ The cutoff date for auto approval of released patches. Any patches released on or before this date are installed automatically. Not supported on Ubuntu Server.
--
-- Enter dates in the format @YYYY-MM-DD@ . For example, @2020-12-31@ .
  , complianceLevel :: Core.Maybe Types.PatchComplianceLevel
    -- ^ A compliance severity level for all approved patches in a patch baseline.
  , enableNonSecurity :: Core.Maybe Core.Bool
    -- ^ For instances identified by the approval rule filters, enables a patch baseline to apply non-security updates available in the specified repository. The default value is 'false'. Applies to Linux instances only.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PatchRule' value with any optional fields omitted.
mkPatchRule
    :: Types.PatchFilterGroup -- ^ 'patchFilterGroup'
    -> PatchRule
mkPatchRule patchFilterGroup
  = PatchRule'{patchFilterGroup, approveAfterDays = Core.Nothing,
               approveUntilDate = Core.Nothing, complianceLevel = Core.Nothing,
               enableNonSecurity = Core.Nothing}

-- | The patch filter group that defines the criteria for the rule.
--
-- /Note:/ Consider using 'patchFilterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPatchFilterGroup :: Lens.Lens' PatchRule Types.PatchFilterGroup
prPatchFilterGroup = Lens.field @"patchFilterGroup"
{-# INLINEABLE prPatchFilterGroup #-}
{-# DEPRECATED patchFilterGroup "Use generic-lens or generic-optics with 'patchFilterGroup' instead"  #-}

-- | The number of days after the release date of each patch matched by the rule that the patch is marked as approved in the patch baseline. For example, a value of @7@ means that patches are approved seven days after they are released. Not supported on Ubuntu Server.
--
-- /Note:/ Consider using 'approveAfterDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prApproveAfterDays :: Lens.Lens' PatchRule (Core.Maybe Core.Natural)
prApproveAfterDays = Lens.field @"approveAfterDays"
{-# INLINEABLE prApproveAfterDays #-}
{-# DEPRECATED approveAfterDays "Use generic-lens or generic-optics with 'approveAfterDays' instead"  #-}

-- | The cutoff date for auto approval of released patches. Any patches released on or before this date are installed automatically. Not supported on Ubuntu Server.
--
-- Enter dates in the format @YYYY-MM-DD@ . For example, @2020-12-31@ .
--
-- /Note:/ Consider using 'approveUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prApproveUntilDate :: Lens.Lens' PatchRule (Core.Maybe Types.PatchStringDateTime)
prApproveUntilDate = Lens.field @"approveUntilDate"
{-# INLINEABLE prApproveUntilDate #-}
{-# DEPRECATED approveUntilDate "Use generic-lens or generic-optics with 'approveUntilDate' instead"  #-}

-- | A compliance severity level for all approved patches in a patch baseline.
--
-- /Note:/ Consider using 'complianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prComplianceLevel :: Lens.Lens' PatchRule (Core.Maybe Types.PatchComplianceLevel)
prComplianceLevel = Lens.field @"complianceLevel"
{-# INLINEABLE prComplianceLevel #-}
{-# DEPRECATED complianceLevel "Use generic-lens or generic-optics with 'complianceLevel' instead"  #-}

-- | For instances identified by the approval rule filters, enables a patch baseline to apply non-security updates available in the specified repository. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'enableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prEnableNonSecurity :: Lens.Lens' PatchRule (Core.Maybe Core.Bool)
prEnableNonSecurity = Lens.field @"enableNonSecurity"
{-# INLINEABLE prEnableNonSecurity #-}
{-# DEPRECATED enableNonSecurity "Use generic-lens or generic-optics with 'enableNonSecurity' instead"  #-}

instance Core.FromJSON PatchRule where
        toJSON PatchRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PatchFilterGroup" Core..= patchFilterGroup),
                  ("ApproveAfterDays" Core..=) Core.<$> approveAfterDays,
                  ("ApproveUntilDate" Core..=) Core.<$> approveUntilDate,
                  ("ComplianceLevel" Core..=) Core.<$> complianceLevel,
                  ("EnableNonSecurity" Core..=) Core.<$> enableNonSecurity])

instance Core.FromJSON PatchRule where
        parseJSON
          = Core.withObject "PatchRule" Core.$
              \ x ->
                PatchRule' Core.<$>
                  (x Core..: "PatchFilterGroup") Core.<*>
                    x Core..:? "ApproveAfterDays"
                    Core.<*> x Core..:? "ApproveUntilDate"
                    Core.<*> x Core..:? "ComplianceLevel"
                    Core.<*> x Core..:? "EnableNonSecurity"
