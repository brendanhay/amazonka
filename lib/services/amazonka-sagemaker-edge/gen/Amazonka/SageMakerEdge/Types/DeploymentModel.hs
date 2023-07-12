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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Types.DeploymentModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerEdge.Types.DeploymentStatus
import Amazonka.SageMakerEdge.Types.ModelState

-- |
--
-- /See:/ 'newDeploymentModel' smart constructor.
data DeploymentModel = DeploymentModel'
  { -- | The desired state of the model.
    desiredState :: Prelude.Maybe ModelState,
    -- | The unique handle of the model.
    modelHandle :: Prelude.Maybe Prelude.Text,
    -- | The name of the model.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The version of the model.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | Returns the error message if there is a rollback.
    rollbackFailureReason :: Prelude.Maybe Prelude.Text,
    -- | Returns the current state of the model.
    state :: Prelude.Maybe ModelState,
    -- | Returns the deployment status of the model.
    status :: Prelude.Maybe DeploymentStatus,
    -- | Returns the error message for the deployment status result.
    statusReason :: Prelude.Maybe Prelude.Text
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
-- 'desiredState', 'deploymentModel_desiredState' - The desired state of the model.
--
-- 'modelHandle', 'deploymentModel_modelHandle' - The unique handle of the model.
--
-- 'modelName', 'deploymentModel_modelName' - The name of the model.
--
-- 'modelVersion', 'deploymentModel_modelVersion' - The version of the model.
--
-- 'rollbackFailureReason', 'deploymentModel_rollbackFailureReason' - Returns the error message if there is a rollback.
--
-- 'state', 'deploymentModel_state' - Returns the current state of the model.
--
-- 'status', 'deploymentModel_status' - Returns the deployment status of the model.
--
-- 'statusReason', 'deploymentModel_statusReason' - Returns the error message for the deployment status result.
newDeploymentModel ::
  DeploymentModel
newDeploymentModel =
  DeploymentModel'
    { desiredState = Prelude.Nothing,
      modelHandle = Prelude.Nothing,
      modelName = Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      rollbackFailureReason = Prelude.Nothing,
      state = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The desired state of the model.
deploymentModel_desiredState :: Lens.Lens' DeploymentModel (Prelude.Maybe ModelState)
deploymentModel_desiredState = Lens.lens (\DeploymentModel' {desiredState} -> desiredState) (\s@DeploymentModel' {} a -> s {desiredState = a} :: DeploymentModel)

-- | The unique handle of the model.
deploymentModel_modelHandle :: Lens.Lens' DeploymentModel (Prelude.Maybe Prelude.Text)
deploymentModel_modelHandle = Lens.lens (\DeploymentModel' {modelHandle} -> modelHandle) (\s@DeploymentModel' {} a -> s {modelHandle = a} :: DeploymentModel)

-- | The name of the model.
deploymentModel_modelName :: Lens.Lens' DeploymentModel (Prelude.Maybe Prelude.Text)
deploymentModel_modelName = Lens.lens (\DeploymentModel' {modelName} -> modelName) (\s@DeploymentModel' {} a -> s {modelName = a} :: DeploymentModel)

-- | The version of the model.
deploymentModel_modelVersion :: Lens.Lens' DeploymentModel (Prelude.Maybe Prelude.Text)
deploymentModel_modelVersion = Lens.lens (\DeploymentModel' {modelVersion} -> modelVersion) (\s@DeploymentModel' {} a -> s {modelVersion = a} :: DeploymentModel)

-- | Returns the error message if there is a rollback.
deploymentModel_rollbackFailureReason :: Lens.Lens' DeploymentModel (Prelude.Maybe Prelude.Text)
deploymentModel_rollbackFailureReason = Lens.lens (\DeploymentModel' {rollbackFailureReason} -> rollbackFailureReason) (\s@DeploymentModel' {} a -> s {rollbackFailureReason = a} :: DeploymentModel)

-- | Returns the current state of the model.
deploymentModel_state :: Lens.Lens' DeploymentModel (Prelude.Maybe ModelState)
deploymentModel_state = Lens.lens (\DeploymentModel' {state} -> state) (\s@DeploymentModel' {} a -> s {state = a} :: DeploymentModel)

-- | Returns the deployment status of the model.
deploymentModel_status :: Lens.Lens' DeploymentModel (Prelude.Maybe DeploymentStatus)
deploymentModel_status = Lens.lens (\DeploymentModel' {status} -> status) (\s@DeploymentModel' {} a -> s {status = a} :: DeploymentModel)

-- | Returns the error message for the deployment status result.
deploymentModel_statusReason :: Lens.Lens' DeploymentModel (Prelude.Maybe Prelude.Text)
deploymentModel_statusReason = Lens.lens (\DeploymentModel' {statusReason} -> statusReason) (\s@DeploymentModel' {} a -> s {statusReason = a} :: DeploymentModel)

instance Prelude.Hashable DeploymentModel where
  hashWithSalt _salt DeploymentModel' {..} =
    _salt
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` modelHandle
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` rollbackFailureReason
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason

instance Prelude.NFData DeploymentModel where
  rnf DeploymentModel' {..} =
    Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf modelHandle
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf rollbackFailureReason
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason

instance Data.ToJSON DeploymentModel where
  toJSON DeploymentModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DesiredState" Data..=) Prelude.<$> desiredState,
            ("ModelHandle" Data..=) Prelude.<$> modelHandle,
            ("ModelName" Data..=) Prelude.<$> modelName,
            ("ModelVersion" Data..=) Prelude.<$> modelVersion,
            ("RollbackFailureReason" Data..=)
              Prelude.<$> rollbackFailureReason,
            ("State" Data..=) Prelude.<$> state,
            ("Status" Data..=) Prelude.<$> status,
            ("StatusReason" Data..=) Prelude.<$> statusReason
          ]
      )
