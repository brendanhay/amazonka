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
-- Module      : Amazonka.SageMakerEdge.Types.DeploymentModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Types.DeploymentModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerEdge.Types.DeploymentStatus
import Amazonka.SageMakerEdge.Types.ModelState

-- |
--
-- /See:/ 'newDeploymentModel' smart constructor.
data DeploymentModel = DeploymentModel'
  { -- | Returns the error message if there is a rollback.
    rollbackFailureReason :: Prelude.Maybe Prelude.Text,
    -- | Returns the error message for the deployment status result.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Returns the current state of the model.
    state :: Prelude.Maybe ModelState,
    -- | The version of the model.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | Returns the deployment status of the model.
    status :: Prelude.Maybe DeploymentStatus,
    -- | The desired state of the model.
    desiredState :: Prelude.Maybe ModelState,
    -- | The name of the model.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The unique handle of the model.
    modelHandle :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rollbackFailureReason', 'deploymentModel_rollbackFailureReason' - Returns the error message if there is a rollback.
--
-- 'statusReason', 'deploymentModel_statusReason' - Returns the error message for the deployment status result.
--
-- 'state', 'deploymentModel_state' - Returns the current state of the model.
--
-- 'modelVersion', 'deploymentModel_modelVersion' - The version of the model.
--
-- 'status', 'deploymentModel_status' - Returns the deployment status of the model.
--
-- 'desiredState', 'deploymentModel_desiredState' - The desired state of the model.
--
-- 'modelName', 'deploymentModel_modelName' - The name of the model.
--
-- 'modelHandle', 'deploymentModel_modelHandle' - The unique handle of the model.
newDeploymentModel ::
  DeploymentModel
newDeploymentModel =
  DeploymentModel'
    { rollbackFailureReason =
        Prelude.Nothing,
      statusReason = Prelude.Nothing,
      state = Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      modelName = Prelude.Nothing,
      modelHandle = Prelude.Nothing
    }

-- | Returns the error message if there is a rollback.
deploymentModel_rollbackFailureReason :: Lens.Lens' DeploymentModel (Prelude.Maybe Prelude.Text)
deploymentModel_rollbackFailureReason = Lens.lens (\DeploymentModel' {rollbackFailureReason} -> rollbackFailureReason) (\s@DeploymentModel' {} a -> s {rollbackFailureReason = a} :: DeploymentModel)

-- | Returns the error message for the deployment status result.
deploymentModel_statusReason :: Lens.Lens' DeploymentModel (Prelude.Maybe Prelude.Text)
deploymentModel_statusReason = Lens.lens (\DeploymentModel' {statusReason} -> statusReason) (\s@DeploymentModel' {} a -> s {statusReason = a} :: DeploymentModel)

-- | Returns the current state of the model.
deploymentModel_state :: Lens.Lens' DeploymentModel (Prelude.Maybe ModelState)
deploymentModel_state = Lens.lens (\DeploymentModel' {state} -> state) (\s@DeploymentModel' {} a -> s {state = a} :: DeploymentModel)

-- | The version of the model.
deploymentModel_modelVersion :: Lens.Lens' DeploymentModel (Prelude.Maybe Prelude.Text)
deploymentModel_modelVersion = Lens.lens (\DeploymentModel' {modelVersion} -> modelVersion) (\s@DeploymentModel' {} a -> s {modelVersion = a} :: DeploymentModel)

-- | Returns the deployment status of the model.
deploymentModel_status :: Lens.Lens' DeploymentModel (Prelude.Maybe DeploymentStatus)
deploymentModel_status = Lens.lens (\DeploymentModel' {status} -> status) (\s@DeploymentModel' {} a -> s {status = a} :: DeploymentModel)

-- | The desired state of the model.
deploymentModel_desiredState :: Lens.Lens' DeploymentModel (Prelude.Maybe ModelState)
deploymentModel_desiredState = Lens.lens (\DeploymentModel' {desiredState} -> desiredState) (\s@DeploymentModel' {} a -> s {desiredState = a} :: DeploymentModel)

-- | The name of the model.
deploymentModel_modelName :: Lens.Lens' DeploymentModel (Prelude.Maybe Prelude.Text)
deploymentModel_modelName = Lens.lens (\DeploymentModel' {modelName} -> modelName) (\s@DeploymentModel' {} a -> s {modelName = a} :: DeploymentModel)

-- | The unique handle of the model.
deploymentModel_modelHandle :: Lens.Lens' DeploymentModel (Prelude.Maybe Prelude.Text)
deploymentModel_modelHandle = Lens.lens (\DeploymentModel' {modelHandle} -> modelHandle) (\s@DeploymentModel' {} a -> s {modelHandle = a} :: DeploymentModel)

instance Prelude.Hashable DeploymentModel where
  hashWithSalt _salt DeploymentModel' {..} =
    _salt `Prelude.hashWithSalt` rollbackFailureReason
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` modelHandle

instance Prelude.NFData DeploymentModel where
  rnf DeploymentModel' {..} =
    Prelude.rnf rollbackFailureReason
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf modelHandle

instance Core.ToJSON DeploymentModel where
  toJSON DeploymentModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RollbackFailureReason" Core..=)
              Prelude.<$> rollbackFailureReason,
            ("StatusReason" Core..=) Prelude.<$> statusReason,
            ("State" Core..=) Prelude.<$> state,
            ("ModelVersion" Core..=) Prelude.<$> modelVersion,
            ("Status" Core..=) Prelude.<$> status,
            ("DesiredState" Core..=) Prelude.<$> desiredState,
            ("ModelName" Core..=) Prelude.<$> modelName,
            ("ModelHandle" Core..=) Prelude.<$> modelHandle
          ]
      )
