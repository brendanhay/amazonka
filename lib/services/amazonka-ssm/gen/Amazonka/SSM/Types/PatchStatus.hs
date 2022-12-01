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
-- Module      : Amazonka.SSM.Types.PatchStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.PatchStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.PatchComplianceLevel
import Amazonka.SSM.Types.PatchDeploymentStatus

-- | Information about the approval status of a patch.
--
-- /See:/ 'newPatchStatus' smart constructor.
data PatchStatus = PatchStatus'
  { -- | The approval status of a patch.
    deploymentStatus :: Prelude.Maybe PatchDeploymentStatus,
    -- | The compliance severity level for a patch.
    complianceLevel :: Prelude.Maybe PatchComplianceLevel,
    -- | The date the patch was approved (or will be approved if the status is
    -- @PENDING_APPROVAL@).
    approvalDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PatchStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStatus', 'patchStatus_deploymentStatus' - The approval status of a patch.
--
-- 'complianceLevel', 'patchStatus_complianceLevel' - The compliance severity level for a patch.
--
-- 'approvalDate', 'patchStatus_approvalDate' - The date the patch was approved (or will be approved if the status is
-- @PENDING_APPROVAL@).
newPatchStatus ::
  PatchStatus
newPatchStatus =
  PatchStatus'
    { deploymentStatus = Prelude.Nothing,
      complianceLevel = Prelude.Nothing,
      approvalDate = Prelude.Nothing
    }

-- | The approval status of a patch.
patchStatus_deploymentStatus :: Lens.Lens' PatchStatus (Prelude.Maybe PatchDeploymentStatus)
patchStatus_deploymentStatus = Lens.lens (\PatchStatus' {deploymentStatus} -> deploymentStatus) (\s@PatchStatus' {} a -> s {deploymentStatus = a} :: PatchStatus)

-- | The compliance severity level for a patch.
patchStatus_complianceLevel :: Lens.Lens' PatchStatus (Prelude.Maybe PatchComplianceLevel)
patchStatus_complianceLevel = Lens.lens (\PatchStatus' {complianceLevel} -> complianceLevel) (\s@PatchStatus' {} a -> s {complianceLevel = a} :: PatchStatus)

-- | The date the patch was approved (or will be approved if the status is
-- @PENDING_APPROVAL@).
patchStatus_approvalDate :: Lens.Lens' PatchStatus (Prelude.Maybe Prelude.UTCTime)
patchStatus_approvalDate = Lens.lens (\PatchStatus' {approvalDate} -> approvalDate) (\s@PatchStatus' {} a -> s {approvalDate = a} :: PatchStatus) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON PatchStatus where
  parseJSON =
    Core.withObject
      "PatchStatus"
      ( \x ->
          PatchStatus'
            Prelude.<$> (x Core..:? "DeploymentStatus")
            Prelude.<*> (x Core..:? "ComplianceLevel")
            Prelude.<*> (x Core..:? "ApprovalDate")
      )

instance Prelude.Hashable PatchStatus where
  hashWithSalt _salt PatchStatus' {..} =
    _salt `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` complianceLevel
      `Prelude.hashWithSalt` approvalDate

instance Prelude.NFData PatchStatus where
  rnf PatchStatus' {..} =
    Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf complianceLevel
      `Prelude.seq` Prelude.rnf approvalDate
