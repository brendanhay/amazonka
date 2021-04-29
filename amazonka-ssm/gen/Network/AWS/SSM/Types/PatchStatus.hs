{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.PatchComplianceLevel
import Network.AWS.SSM.Types.PatchDeploymentStatus

-- | Information about the approval status of a patch.
--
-- /See:/ 'newPatchStatus' smart constructor.
data PatchStatus = PatchStatus'
  { -- | The date the patch was approved (or will be approved if the status is
    -- PENDING_APPROVAL).
    approvalDate :: Prelude.Maybe Prelude.POSIX,
    -- | The compliance severity level for a patch.
    complianceLevel :: Prelude.Maybe PatchComplianceLevel,
    -- | The approval status of a patch (APPROVED, PENDING_APPROVAL,
    -- EXPLICIT_APPROVED, EXPLICIT_REJECTED).
    deploymentStatus :: Prelude.Maybe PatchDeploymentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { approvalDate = Prelude.Nothing,
      complianceLevel = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing
    }

-- | The date the patch was approved (or will be approved if the status is
-- PENDING_APPROVAL).
patchStatus_approvalDate :: Lens.Lens' PatchStatus (Prelude.Maybe Prelude.UTCTime)
patchStatus_approvalDate = Lens.lens (\PatchStatus' {approvalDate} -> approvalDate) (\s@PatchStatus' {} a -> s {approvalDate = a} :: PatchStatus) Prelude.. Lens.mapping Prelude._Time

-- | The compliance severity level for a patch.
patchStatus_complianceLevel :: Lens.Lens' PatchStatus (Prelude.Maybe PatchComplianceLevel)
patchStatus_complianceLevel = Lens.lens (\PatchStatus' {complianceLevel} -> complianceLevel) (\s@PatchStatus' {} a -> s {complianceLevel = a} :: PatchStatus)

-- | The approval status of a patch (APPROVED, PENDING_APPROVAL,
-- EXPLICIT_APPROVED, EXPLICIT_REJECTED).
patchStatus_deploymentStatus :: Lens.Lens' PatchStatus (Prelude.Maybe PatchDeploymentStatus)
patchStatus_deploymentStatus = Lens.lens (\PatchStatus' {deploymentStatus} -> deploymentStatus) (\s@PatchStatus' {} a -> s {deploymentStatus = a} :: PatchStatus)

instance Prelude.FromJSON PatchStatus where
  parseJSON =
    Prelude.withObject
      "PatchStatus"
      ( \x ->
          PatchStatus'
            Prelude.<$> (x Prelude..:? "ApprovalDate")
            Prelude.<*> (x Prelude..:? "ComplianceLevel")
            Prelude.<*> (x Prelude..:? "DeploymentStatus")
      )

instance Prelude.Hashable PatchStatus

instance Prelude.NFData PatchStatus
