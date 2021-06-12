{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.PatchComplianceLevel
import Network.AWS.SSM.Types.PatchDeploymentStatus

-- | Information about the approval status of a patch.
--
-- /See:/ 'newPatchStatus' smart constructor.
data PatchStatus = PatchStatus'
  { -- | The date the patch was approved (or will be approved if the status is
    -- PENDING_APPROVAL).
    approvalDate :: Core.Maybe Core.POSIX,
    -- | The compliance severity level for a patch.
    complianceLevel :: Core.Maybe PatchComplianceLevel,
    -- | The approval status of a patch (APPROVED, PENDING_APPROVAL,
    -- EXPLICIT_APPROVED, EXPLICIT_REJECTED).
    deploymentStatus :: Core.Maybe PatchDeploymentStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PatchStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalDate', 'patchStatus_approvalDate' - The date the patch was approved (or will be approved if the status is
-- PENDING_APPROVAL).
--
-- 'complianceLevel', 'patchStatus_complianceLevel' - The compliance severity level for a patch.
--
-- 'deploymentStatus', 'patchStatus_deploymentStatus' - The approval status of a patch (APPROVED, PENDING_APPROVAL,
-- EXPLICIT_APPROVED, EXPLICIT_REJECTED).
newPatchStatus ::
  PatchStatus
newPatchStatus =
  PatchStatus'
    { approvalDate = Core.Nothing,
      complianceLevel = Core.Nothing,
      deploymentStatus = Core.Nothing
    }

-- | The date the patch was approved (or will be approved if the status is
-- PENDING_APPROVAL).
patchStatus_approvalDate :: Lens.Lens' PatchStatus (Core.Maybe Core.UTCTime)
patchStatus_approvalDate = Lens.lens (\PatchStatus' {approvalDate} -> approvalDate) (\s@PatchStatus' {} a -> s {approvalDate = a} :: PatchStatus) Core.. Lens.mapping Core._Time

-- | The compliance severity level for a patch.
patchStatus_complianceLevel :: Lens.Lens' PatchStatus (Core.Maybe PatchComplianceLevel)
patchStatus_complianceLevel = Lens.lens (\PatchStatus' {complianceLevel} -> complianceLevel) (\s@PatchStatus' {} a -> s {complianceLevel = a} :: PatchStatus)

-- | The approval status of a patch (APPROVED, PENDING_APPROVAL,
-- EXPLICIT_APPROVED, EXPLICIT_REJECTED).
patchStatus_deploymentStatus :: Lens.Lens' PatchStatus (Core.Maybe PatchDeploymentStatus)
patchStatus_deploymentStatus = Lens.lens (\PatchStatus' {deploymentStatus} -> deploymentStatus) (\s@PatchStatus' {} a -> s {deploymentStatus = a} :: PatchStatus)

instance Core.FromJSON PatchStatus where
  parseJSON =
    Core.withObject
      "PatchStatus"
      ( \x ->
          PatchStatus'
            Core.<$> (x Core..:? "ApprovalDate")
            Core.<*> (x Core..:? "ComplianceLevel")
            Core.<*> (x Core..:? "DeploymentStatus")
      )

instance Core.Hashable PatchStatus

instance Core.NFData PatchStatus
