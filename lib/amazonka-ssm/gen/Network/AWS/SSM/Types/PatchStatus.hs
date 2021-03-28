{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchStatus
  ( PatchStatus (..)
  -- * Smart constructor
  , mkPatchStatus
  -- * Lenses
  , psApprovalDate
  , psComplianceLevel
  , psDeploymentStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.PatchComplianceLevel as Types
import qualified Network.AWS.SSM.Types.PatchDeploymentStatus as Types

-- | Information about the approval status of a patch.
--
-- /See:/ 'mkPatchStatus' smart constructor.
data PatchStatus = PatchStatus'
  { approvalDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the patch was approved (or will be approved if the status is PENDING_APPROVAL).
  , complianceLevel :: Core.Maybe Types.PatchComplianceLevel
    -- ^ The compliance severity level for a patch.
  , deploymentStatus :: Core.Maybe Types.PatchDeploymentStatus
    -- ^ The approval status of a patch (APPROVED, PENDING_APPROVAL, EXPLICIT_APPROVED, EXPLICIT_REJECTED).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PatchStatus' value with any optional fields omitted.
mkPatchStatus
    :: PatchStatus
mkPatchStatus
  = PatchStatus'{approvalDate = Core.Nothing,
                 complianceLevel = Core.Nothing, deploymentStatus = Core.Nothing}

-- | The date the patch was approved (or will be approved if the status is PENDING_APPROVAL).
--
-- /Note:/ Consider using 'approvalDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psApprovalDate :: Lens.Lens' PatchStatus (Core.Maybe Core.NominalDiffTime)
psApprovalDate = Lens.field @"approvalDate"
{-# INLINEABLE psApprovalDate #-}
{-# DEPRECATED approvalDate "Use generic-lens or generic-optics with 'approvalDate' instead"  #-}

-- | The compliance severity level for a patch.
--
-- /Note:/ Consider using 'complianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psComplianceLevel :: Lens.Lens' PatchStatus (Core.Maybe Types.PatchComplianceLevel)
psComplianceLevel = Lens.field @"complianceLevel"
{-# INLINEABLE psComplianceLevel #-}
{-# DEPRECATED complianceLevel "Use generic-lens or generic-optics with 'complianceLevel' instead"  #-}

-- | The approval status of a patch (APPROVED, PENDING_APPROVAL, EXPLICIT_APPROVED, EXPLICIT_REJECTED).
--
-- /Note:/ Consider using 'deploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psDeploymentStatus :: Lens.Lens' PatchStatus (Core.Maybe Types.PatchDeploymentStatus)
psDeploymentStatus = Lens.field @"deploymentStatus"
{-# INLINEABLE psDeploymentStatus #-}
{-# DEPRECATED deploymentStatus "Use generic-lens or generic-optics with 'deploymentStatus' instead"  #-}

instance Core.FromJSON PatchStatus where
        parseJSON
          = Core.withObject "PatchStatus" Core.$
              \ x ->
                PatchStatus' Core.<$>
                  (x Core..:? "ApprovalDate") Core.<*> x Core..:? "ComplianceLevel"
                    Core.<*> x Core..:? "DeploymentStatus"
