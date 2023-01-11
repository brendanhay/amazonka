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
-- Module      : Amazonka.GamesParks.Types.StageDeploymentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.StageDeploymentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types.DeploymentAction
import Amazonka.GamesParks.Types.DeploymentResult
import Amazonka.GamesParks.Types.DeploymentState
import qualified Amazonka.Prelude as Prelude

-- | The summary of the properties of a stage deployment.
--
-- /See:/ 'newStageDeploymentSummary' smart constructor.
data StageDeploymentSummary = StageDeploymentSummary'
  { -- | The type of action of the deployment.
    deploymentAction :: Prelude.Maybe DeploymentAction,
    -- | The identifier of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The result of the deployment.
    deploymentResult :: Prelude.Maybe DeploymentResult,
    -- | The state of the deployment.
    deploymentState :: Prelude.Maybe DeploymentState,
    -- | The timestamp of when the deployment was last updated.
    lastUpdated :: Prelude.Maybe Data.ISO8601,
    -- | The identifier of the snapshot associated with the stage deployment.
    snapshotId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StageDeploymentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentAction', 'stageDeploymentSummary_deploymentAction' - The type of action of the deployment.
--
-- 'deploymentId', 'stageDeploymentSummary_deploymentId' - The identifier of the deployment.
--
-- 'deploymentResult', 'stageDeploymentSummary_deploymentResult' - The result of the deployment.
--
-- 'deploymentState', 'stageDeploymentSummary_deploymentState' - The state of the deployment.
--
-- 'lastUpdated', 'stageDeploymentSummary_lastUpdated' - The timestamp of when the deployment was last updated.
--
-- 'snapshotId', 'stageDeploymentSummary_snapshotId' - The identifier of the snapshot associated with the stage deployment.
newStageDeploymentSummary ::
  StageDeploymentSummary
newStageDeploymentSummary =
  StageDeploymentSummary'
    { deploymentAction =
        Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      deploymentResult = Prelude.Nothing,
      deploymentState = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      snapshotId = Prelude.Nothing
    }

-- | The type of action of the deployment.
stageDeploymentSummary_deploymentAction :: Lens.Lens' StageDeploymentSummary (Prelude.Maybe DeploymentAction)
stageDeploymentSummary_deploymentAction = Lens.lens (\StageDeploymentSummary' {deploymentAction} -> deploymentAction) (\s@StageDeploymentSummary' {} a -> s {deploymentAction = a} :: StageDeploymentSummary)

-- | The identifier of the deployment.
stageDeploymentSummary_deploymentId :: Lens.Lens' StageDeploymentSummary (Prelude.Maybe Prelude.Text)
stageDeploymentSummary_deploymentId = Lens.lens (\StageDeploymentSummary' {deploymentId} -> deploymentId) (\s@StageDeploymentSummary' {} a -> s {deploymentId = a} :: StageDeploymentSummary)

-- | The result of the deployment.
stageDeploymentSummary_deploymentResult :: Lens.Lens' StageDeploymentSummary (Prelude.Maybe DeploymentResult)
stageDeploymentSummary_deploymentResult = Lens.lens (\StageDeploymentSummary' {deploymentResult} -> deploymentResult) (\s@StageDeploymentSummary' {} a -> s {deploymentResult = a} :: StageDeploymentSummary)

-- | The state of the deployment.
stageDeploymentSummary_deploymentState :: Lens.Lens' StageDeploymentSummary (Prelude.Maybe DeploymentState)
stageDeploymentSummary_deploymentState = Lens.lens (\StageDeploymentSummary' {deploymentState} -> deploymentState) (\s@StageDeploymentSummary' {} a -> s {deploymentState = a} :: StageDeploymentSummary)

-- | The timestamp of when the deployment was last updated.
stageDeploymentSummary_lastUpdated :: Lens.Lens' StageDeploymentSummary (Prelude.Maybe Prelude.UTCTime)
stageDeploymentSummary_lastUpdated = Lens.lens (\StageDeploymentSummary' {lastUpdated} -> lastUpdated) (\s@StageDeploymentSummary' {} a -> s {lastUpdated = a} :: StageDeploymentSummary) Prelude.. Lens.mapping Data._Time

-- | The identifier of the snapshot associated with the stage deployment.
stageDeploymentSummary_snapshotId :: Lens.Lens' StageDeploymentSummary (Prelude.Maybe Prelude.Text)
stageDeploymentSummary_snapshotId = Lens.lens (\StageDeploymentSummary' {snapshotId} -> snapshotId) (\s@StageDeploymentSummary' {} a -> s {snapshotId = a} :: StageDeploymentSummary)

instance Data.FromJSON StageDeploymentSummary where
  parseJSON =
    Data.withObject
      "StageDeploymentSummary"
      ( \x ->
          StageDeploymentSummary'
            Prelude.<$> (x Data..:? "DeploymentAction")
            Prelude.<*> (x Data..:? "DeploymentId")
            Prelude.<*> (x Data..:? "DeploymentResult")
            Prelude.<*> (x Data..:? "DeploymentState")
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "SnapshotId")
      )

instance Prelude.Hashable StageDeploymentSummary where
  hashWithSalt _salt StageDeploymentSummary' {..} =
    _salt `Prelude.hashWithSalt` deploymentAction
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` deploymentResult
      `Prelude.hashWithSalt` deploymentState
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData StageDeploymentSummary where
  rnf StageDeploymentSummary' {..} =
    Prelude.rnf deploymentAction
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf deploymentResult
      `Prelude.seq` Prelude.rnf deploymentState
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf snapshotId
