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
-- Module      : Amazonka.M2.Types.DeployedVersionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DeployedVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.DeploymentLifecycle
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of a deployed application.
--
-- /See:/ 'newDeployedVersionSummary' smart constructor.
data DeployedVersionSummary = DeployedVersionSummary'
  { -- | The reason for the reported status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The version of the deployed application.
    applicationVersion :: Prelude.Natural,
    -- | The status of the deployment.
    status :: DeploymentLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeployedVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReason', 'deployedVersionSummary_statusReason' - The reason for the reported status.
--
-- 'applicationVersion', 'deployedVersionSummary_applicationVersion' - The version of the deployed application.
--
-- 'status', 'deployedVersionSummary_status' - The status of the deployment.
newDeployedVersionSummary ::
  -- | 'applicationVersion'
  Prelude.Natural ->
  -- | 'status'
  DeploymentLifecycle ->
  DeployedVersionSummary
newDeployedVersionSummary
  pApplicationVersion_
  pStatus_ =
    DeployedVersionSummary'
      { statusReason =
          Prelude.Nothing,
        applicationVersion = pApplicationVersion_,
        status = pStatus_
      }

-- | The reason for the reported status.
deployedVersionSummary_statusReason :: Lens.Lens' DeployedVersionSummary (Prelude.Maybe Prelude.Text)
deployedVersionSummary_statusReason = Lens.lens (\DeployedVersionSummary' {statusReason} -> statusReason) (\s@DeployedVersionSummary' {} a -> s {statusReason = a} :: DeployedVersionSummary)

-- | The version of the deployed application.
deployedVersionSummary_applicationVersion :: Lens.Lens' DeployedVersionSummary Prelude.Natural
deployedVersionSummary_applicationVersion = Lens.lens (\DeployedVersionSummary' {applicationVersion} -> applicationVersion) (\s@DeployedVersionSummary' {} a -> s {applicationVersion = a} :: DeployedVersionSummary)

-- | The status of the deployment.
deployedVersionSummary_status :: Lens.Lens' DeployedVersionSummary DeploymentLifecycle
deployedVersionSummary_status = Lens.lens (\DeployedVersionSummary' {status} -> status) (\s@DeployedVersionSummary' {} a -> s {status = a} :: DeployedVersionSummary)

instance Data.FromJSON DeployedVersionSummary where
  parseJSON =
    Data.withObject
      "DeployedVersionSummary"
      ( \x ->
          DeployedVersionSummary'
            Prelude.<$> (x Data..:? "statusReason")
            Prelude.<*> (x Data..: "applicationVersion")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable DeployedVersionSummary where
  hashWithSalt _salt DeployedVersionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` status

instance Prelude.NFData DeployedVersionSummary where
  rnf DeployedVersionSummary' {..} =
    Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf status
