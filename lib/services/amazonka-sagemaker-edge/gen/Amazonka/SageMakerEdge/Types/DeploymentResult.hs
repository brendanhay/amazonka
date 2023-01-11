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
-- Module      : Amazonka.SageMakerEdge.Types.DeploymentResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Types.DeploymentResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerEdge.Types.DeploymentModel

-- | Information about the result of a deployment on an edge device that is
-- registered with SageMaker Edge Manager.
--
-- /See:/ 'newDeploymentResult' smart constructor.
data DeploymentResult = DeploymentResult'
  { -- | The timestamp of when the deployment was ended, and the agent got the
    -- deployment results.
    deploymentEndTime :: Prelude.Maybe Data.POSIX,
    -- | Returns a list of models deployed on the agent.
    deploymentModels :: Prelude.Maybe [DeploymentModel],
    -- | The name and unique ID of the deployment.
    deploymentName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the deployment was started on the agent.
    deploymentStartTime :: Prelude.Maybe Data.POSIX,
    -- | Returns the bucket error code.
    deploymentStatus :: Prelude.Maybe Prelude.Text,
    -- | Returns the detailed error message.
    deploymentStatusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentEndTime', 'deploymentResult_deploymentEndTime' - The timestamp of when the deployment was ended, and the agent got the
-- deployment results.
--
-- 'deploymentModels', 'deploymentResult_deploymentModels' - Returns a list of models deployed on the agent.
--
-- 'deploymentName', 'deploymentResult_deploymentName' - The name and unique ID of the deployment.
--
-- 'deploymentStartTime', 'deploymentResult_deploymentStartTime' - The timestamp of when the deployment was started on the agent.
--
-- 'deploymentStatus', 'deploymentResult_deploymentStatus' - Returns the bucket error code.
--
-- 'deploymentStatusMessage', 'deploymentResult_deploymentStatusMessage' - Returns the detailed error message.
newDeploymentResult ::
  DeploymentResult
newDeploymentResult =
  DeploymentResult'
    { deploymentEndTime =
        Prelude.Nothing,
      deploymentModels = Prelude.Nothing,
      deploymentName = Prelude.Nothing,
      deploymentStartTime = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      deploymentStatusMessage = Prelude.Nothing
    }

-- | The timestamp of when the deployment was ended, and the agent got the
-- deployment results.
deploymentResult_deploymentEndTime :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.UTCTime)
deploymentResult_deploymentEndTime = Lens.lens (\DeploymentResult' {deploymentEndTime} -> deploymentEndTime) (\s@DeploymentResult' {} a -> s {deploymentEndTime = a} :: DeploymentResult) Prelude.. Lens.mapping Data._Time

-- | Returns a list of models deployed on the agent.
deploymentResult_deploymentModels :: Lens.Lens' DeploymentResult (Prelude.Maybe [DeploymentModel])
deploymentResult_deploymentModels = Lens.lens (\DeploymentResult' {deploymentModels} -> deploymentModels) (\s@DeploymentResult' {} a -> s {deploymentModels = a} :: DeploymentResult) Prelude.. Lens.mapping Lens.coerced

-- | The name and unique ID of the deployment.
deploymentResult_deploymentName :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.Text)
deploymentResult_deploymentName = Lens.lens (\DeploymentResult' {deploymentName} -> deploymentName) (\s@DeploymentResult' {} a -> s {deploymentName = a} :: DeploymentResult)

-- | The timestamp of when the deployment was started on the agent.
deploymentResult_deploymentStartTime :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.UTCTime)
deploymentResult_deploymentStartTime = Lens.lens (\DeploymentResult' {deploymentStartTime} -> deploymentStartTime) (\s@DeploymentResult' {} a -> s {deploymentStartTime = a} :: DeploymentResult) Prelude.. Lens.mapping Data._Time

-- | Returns the bucket error code.
deploymentResult_deploymentStatus :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.Text)
deploymentResult_deploymentStatus = Lens.lens (\DeploymentResult' {deploymentStatus} -> deploymentStatus) (\s@DeploymentResult' {} a -> s {deploymentStatus = a} :: DeploymentResult)

-- | Returns the detailed error message.
deploymentResult_deploymentStatusMessage :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.Text)
deploymentResult_deploymentStatusMessage = Lens.lens (\DeploymentResult' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@DeploymentResult' {} a -> s {deploymentStatusMessage = a} :: DeploymentResult)

instance Prelude.Hashable DeploymentResult where
  hashWithSalt _salt DeploymentResult' {..} =
    _salt `Prelude.hashWithSalt` deploymentEndTime
      `Prelude.hashWithSalt` deploymentModels
      `Prelude.hashWithSalt` deploymentName
      `Prelude.hashWithSalt` deploymentStartTime
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` deploymentStatusMessage

instance Prelude.NFData DeploymentResult where
  rnf DeploymentResult' {..} =
    Prelude.rnf deploymentEndTime
      `Prelude.seq` Prelude.rnf deploymentModels
      `Prelude.seq` Prelude.rnf deploymentName
      `Prelude.seq` Prelude.rnf deploymentStartTime
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf deploymentStatusMessage

instance Data.ToJSON DeploymentResult where
  toJSON DeploymentResult' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeploymentEndTime" Data..=)
              Prelude.<$> deploymentEndTime,
            ("DeploymentModels" Data..=)
              Prelude.<$> deploymentModels,
            ("DeploymentName" Data..=)
              Prelude.<$> deploymentName,
            ("DeploymentStartTime" Data..=)
              Prelude.<$> deploymentStartTime,
            ("DeploymentStatus" Data..=)
              Prelude.<$> deploymentStatus,
            ("DeploymentStatusMessage" Data..=)
              Prelude.<$> deploymentStatusMessage
          ]
      )
