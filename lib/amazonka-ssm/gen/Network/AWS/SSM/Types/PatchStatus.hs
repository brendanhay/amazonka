{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchStatus
  ( PatchStatus (..),

    -- * Smart constructor
    mkPatchStatus,

    -- * Lenses
    psApprovalDate,
    psDeploymentStatus,
    psComplianceLevel,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.PatchComplianceLevel
import Network.AWS.SSM.Types.PatchDeploymentStatus

-- | Information about the approval status of a patch.
--
-- /See:/ 'mkPatchStatus' smart constructor.
data PatchStatus = PatchStatus'
  { approvalDate ::
      Lude.Maybe Lude.Timestamp,
    deploymentStatus :: Lude.Maybe PatchDeploymentStatus,
    complianceLevel :: Lude.Maybe PatchComplianceLevel
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchStatus' with the minimum fields required to make a request.
--
-- * 'approvalDate' - The date the patch was approved (or will be approved if the status is PENDING_APPROVAL).
-- * 'complianceLevel' - The compliance severity level for a patch.
-- * 'deploymentStatus' - The approval status of a patch (APPROVED, PENDING_APPROVAL, EXPLICIT_APPROVED, EXPLICIT_REJECTED).
mkPatchStatus ::
  PatchStatus
mkPatchStatus =
  PatchStatus'
    { approvalDate = Lude.Nothing,
      deploymentStatus = Lude.Nothing,
      complianceLevel = Lude.Nothing
    }

-- | The date the patch was approved (or will be approved if the status is PENDING_APPROVAL).
--
-- /Note:/ Consider using 'approvalDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psApprovalDate :: Lens.Lens' PatchStatus (Lude.Maybe Lude.Timestamp)
psApprovalDate = Lens.lens (approvalDate :: PatchStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {approvalDate = a} :: PatchStatus)
{-# DEPRECATED psApprovalDate "Use generic-lens or generic-optics with 'approvalDate' instead." #-}

-- | The approval status of a patch (APPROVED, PENDING_APPROVAL, EXPLICIT_APPROVED, EXPLICIT_REJECTED).
--
-- /Note:/ Consider using 'deploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psDeploymentStatus :: Lens.Lens' PatchStatus (Lude.Maybe PatchDeploymentStatus)
psDeploymentStatus = Lens.lens (deploymentStatus :: PatchStatus -> Lude.Maybe PatchDeploymentStatus) (\s a -> s {deploymentStatus = a} :: PatchStatus)
{-# DEPRECATED psDeploymentStatus "Use generic-lens or generic-optics with 'deploymentStatus' instead." #-}

-- | The compliance severity level for a patch.
--
-- /Note:/ Consider using 'complianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psComplianceLevel :: Lens.Lens' PatchStatus (Lude.Maybe PatchComplianceLevel)
psComplianceLevel = Lens.lens (complianceLevel :: PatchStatus -> Lude.Maybe PatchComplianceLevel) (\s a -> s {complianceLevel = a} :: PatchStatus)
{-# DEPRECATED psComplianceLevel "Use generic-lens or generic-optics with 'complianceLevel' instead." #-}

instance Lude.FromJSON PatchStatus where
  parseJSON =
    Lude.withObject
      "PatchStatus"
      ( \x ->
          PatchStatus'
            Lude.<$> (x Lude..:? "ApprovalDate")
            Lude.<*> (x Lude..:? "DeploymentStatus")
            Lude.<*> (x Lude..:? "ComplianceLevel")
      )
