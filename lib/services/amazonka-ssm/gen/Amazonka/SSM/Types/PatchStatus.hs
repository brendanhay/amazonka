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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.PatchStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.PatchComplianceLevel
import Amazonka.SSM.Types.PatchDeploymentStatus

-- | Information about the approval status of a patch.
--
-- /See:/ 'newPatchStatus' smart constructor.
data PatchStatus = PatchStatus'
  { -- | The date the patch was approved (or will be approved if the status is
    -- @PENDING_APPROVAL@).
    approvalDate :: Prelude.Maybe Data.POSIX,
    -- | The compliance severity level for a patch.
    complianceLevel :: Prelude.Maybe PatchComplianceLevel,
    -- | The approval status of a patch.
    deploymentStatus :: Prelude.Maybe PatchDeploymentStatus
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
-- 'approvalDate', 'patchStatus_approvalDate' - The date the patch was approved (or will be approved if the status is
-- @PENDING_APPROVAL@).
--
-- 'complianceLevel', 'patchStatus_complianceLevel' - The compliance severity level for a patch.
--
-- 'deploymentStatus', 'patchStatus_deploymentStatus' - The approval status of a patch.
newPatchStatus ::
  PatchStatus
newPatchStatus =
  PatchStatus'
    { approvalDate = Prelude.Nothing,
      complianceLevel = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing
    }

-- | The date the patch was approved (or will be approved if the status is
-- @PENDING_APPROVAL@).
patchStatus_approvalDate :: Lens.Lens' PatchStatus (Prelude.Maybe Prelude.UTCTime)
patchStatus_approvalDate = Lens.lens (\PatchStatus' {approvalDate} -> approvalDate) (\s@PatchStatus' {} a -> s {approvalDate = a} :: PatchStatus) Prelude.. Lens.mapping Data._Time

-- | The compliance severity level for a patch.
patchStatus_complianceLevel :: Lens.Lens' PatchStatus (Prelude.Maybe PatchComplianceLevel)
patchStatus_complianceLevel = Lens.lens (\PatchStatus' {complianceLevel} -> complianceLevel) (\s@PatchStatus' {} a -> s {complianceLevel = a} :: PatchStatus)

-- | The approval status of a patch.
patchStatus_deploymentStatus :: Lens.Lens' PatchStatus (Prelude.Maybe PatchDeploymentStatus)
patchStatus_deploymentStatus = Lens.lens (\PatchStatus' {deploymentStatus} -> deploymentStatus) (\s@PatchStatus' {} a -> s {deploymentStatus = a} :: PatchStatus)

instance Data.FromJSON PatchStatus where
  parseJSON =
    Data.withObject
      "PatchStatus"
      ( \x ->
          PatchStatus'
            Prelude.<$> (x Data..:? "ApprovalDate")
            Prelude.<*> (x Data..:? "ComplianceLevel")
            Prelude.<*> (x Data..:? "DeploymentStatus")
      )

instance Prelude.Hashable PatchStatus where
  hashWithSalt _salt PatchStatus' {..} =
    _salt
      `Prelude.hashWithSalt` approvalDate
      `Prelude.hashWithSalt` complianceLevel
      `Prelude.hashWithSalt` deploymentStatus

instance Prelude.NFData PatchStatus where
  rnf PatchStatus' {..} =
    Prelude.rnf approvalDate `Prelude.seq`
      Prelude.rnf complianceLevel `Prelude.seq`
        Prelude.rnf deploymentStatus
