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
-- Module      : Amazonka.GamesParks.Types.StageDeploymentDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.StageDeploymentDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types.DeploymentAction
import Amazonka.GamesParks.Types.DeploymentResult
import Amazonka.GamesParks.Types.DeploymentState
import qualified Amazonka.Prelude as Prelude

-- | Properties that provide details of a stage deployment.
--
-- /See:/ 'newStageDeploymentDetails' smart constructor.
data StageDeploymentDetails = StageDeploymentDetails'
  { -- | The state of the deployment.
    deploymentState :: Prelude.Maybe DeploymentState,
    -- | The type of action of the stage deployment.
    deploymentAction :: Prelude.Maybe DeploymentAction,
    -- | The identifier of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The result of the deployment.
    deploymentResult :: Prelude.Maybe DeploymentResult,
    -- | The timestamp of when the stage deployment was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the snapshot associated with the stage deployment.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the deployment was last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StageDeploymentDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentState', 'stageDeploymentDetails_deploymentState' - The state of the deployment.
--
-- 'deploymentAction', 'stageDeploymentDetails_deploymentAction' - The type of action of the stage deployment.
--
-- 'deploymentId', 'stageDeploymentDetails_deploymentId' - The identifier of the deployment.
--
-- 'deploymentResult', 'stageDeploymentDetails_deploymentResult' - The result of the deployment.
--
-- 'created', 'stageDeploymentDetails_created' - The timestamp of when the stage deployment was created.
--
-- 'snapshotId', 'stageDeploymentDetails_snapshotId' - The identifier of the snapshot associated with the stage deployment.
--
-- 'lastUpdated', 'stageDeploymentDetails_lastUpdated' - The timestamp of when the deployment was last updated.
newStageDeploymentDetails ::
  StageDeploymentDetails
newStageDeploymentDetails =
  StageDeploymentDetails'
    { deploymentState =
        Prelude.Nothing,
      deploymentAction = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      deploymentResult = Prelude.Nothing,
      created = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      lastUpdated = Prelude.Nothing
    }

-- | The state of the deployment.
stageDeploymentDetails_deploymentState :: Lens.Lens' StageDeploymentDetails (Prelude.Maybe DeploymentState)
stageDeploymentDetails_deploymentState = Lens.lens (\StageDeploymentDetails' {deploymentState} -> deploymentState) (\s@StageDeploymentDetails' {} a -> s {deploymentState = a} :: StageDeploymentDetails)

-- | The type of action of the stage deployment.
stageDeploymentDetails_deploymentAction :: Lens.Lens' StageDeploymentDetails (Prelude.Maybe DeploymentAction)
stageDeploymentDetails_deploymentAction = Lens.lens (\StageDeploymentDetails' {deploymentAction} -> deploymentAction) (\s@StageDeploymentDetails' {} a -> s {deploymentAction = a} :: StageDeploymentDetails)

-- | The identifier of the deployment.
stageDeploymentDetails_deploymentId :: Lens.Lens' StageDeploymentDetails (Prelude.Maybe Prelude.Text)
stageDeploymentDetails_deploymentId = Lens.lens (\StageDeploymentDetails' {deploymentId} -> deploymentId) (\s@StageDeploymentDetails' {} a -> s {deploymentId = a} :: StageDeploymentDetails)

-- | The result of the deployment.
stageDeploymentDetails_deploymentResult :: Lens.Lens' StageDeploymentDetails (Prelude.Maybe DeploymentResult)
stageDeploymentDetails_deploymentResult = Lens.lens (\StageDeploymentDetails' {deploymentResult} -> deploymentResult) (\s@StageDeploymentDetails' {} a -> s {deploymentResult = a} :: StageDeploymentDetails)

-- | The timestamp of when the stage deployment was created.
stageDeploymentDetails_created :: Lens.Lens' StageDeploymentDetails (Prelude.Maybe Prelude.UTCTime)
stageDeploymentDetails_created = Lens.lens (\StageDeploymentDetails' {created} -> created) (\s@StageDeploymentDetails' {} a -> s {created = a} :: StageDeploymentDetails) Prelude.. Lens.mapping Data._Time

-- | The identifier of the snapshot associated with the stage deployment.
stageDeploymentDetails_snapshotId :: Lens.Lens' StageDeploymentDetails (Prelude.Maybe Prelude.Text)
stageDeploymentDetails_snapshotId = Lens.lens (\StageDeploymentDetails' {snapshotId} -> snapshotId) (\s@StageDeploymentDetails' {} a -> s {snapshotId = a} :: StageDeploymentDetails)

-- | The timestamp of when the deployment was last updated.
stageDeploymentDetails_lastUpdated :: Lens.Lens' StageDeploymentDetails (Prelude.Maybe Prelude.UTCTime)
stageDeploymentDetails_lastUpdated = Lens.lens (\StageDeploymentDetails' {lastUpdated} -> lastUpdated) (\s@StageDeploymentDetails' {} a -> s {lastUpdated = a} :: StageDeploymentDetails) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON StageDeploymentDetails where
  parseJSON =
    Data.withObject
      "StageDeploymentDetails"
      ( \x ->
          StageDeploymentDetails'
            Prelude.<$> (x Data..:? "DeploymentState")
            Prelude.<*> (x Data..:? "DeploymentAction")
            Prelude.<*> (x Data..:? "DeploymentId")
            Prelude.<*> (x Data..:? "DeploymentResult")
            Prelude.<*> (x Data..:? "Created")
            Prelude.<*> (x Data..:? "SnapshotId")
            Prelude.<*> (x Data..:? "LastUpdated")
      )

instance Prelude.Hashable StageDeploymentDetails where
  hashWithSalt _salt StageDeploymentDetails' {..} =
    _salt `Prelude.hashWithSalt` deploymentState
      `Prelude.hashWithSalt` deploymentAction
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` deploymentResult
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` lastUpdated

instance Prelude.NFData StageDeploymentDetails where
  rnf StageDeploymentDetails' {..} =
    Prelude.rnf deploymentState
      `Prelude.seq` Prelude.rnf deploymentAction
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf deploymentResult
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf lastUpdated
