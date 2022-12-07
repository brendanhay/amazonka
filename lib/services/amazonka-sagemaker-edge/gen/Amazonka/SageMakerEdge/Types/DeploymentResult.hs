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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | Returns the bucket error code.
    deploymentStatus :: Prelude.Maybe Prelude.Text,
    -- | The name and unique ID of the deployment.
    deploymentName :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of models deployed on the agent.
    deploymentModels :: Prelude.Maybe [DeploymentModel],
    -- | The timestamp of when the deployment was ended, and the agent got the
    -- deployment results.
    deploymentEndTime :: Prelude.Maybe Data.POSIX,
    -- | Returns the detailed error message.
    deploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the deployment was started on the agent.
    deploymentStartTime :: Prelude.Maybe Data.POSIX
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
-- 'deploymentStatus', 'deploymentResult_deploymentStatus' - Returns the bucket error code.
--
-- 'deploymentName', 'deploymentResult_deploymentName' - The name and unique ID of the deployment.
--
-- 'deploymentModels', 'deploymentResult_deploymentModels' - Returns a list of models deployed on the agent.
--
-- 'deploymentEndTime', 'deploymentResult_deploymentEndTime' - The timestamp of when the deployment was ended, and the agent got the
-- deployment results.
--
-- 'deploymentStatusMessage', 'deploymentResult_deploymentStatusMessage' - Returns the detailed error message.
--
-- 'deploymentStartTime', 'deploymentResult_deploymentStartTime' - The timestamp of when the deployment was started on the agent.
newDeploymentResult ::
  DeploymentResult
newDeploymentResult =
  DeploymentResult'
    { deploymentStatus =
        Prelude.Nothing,
      deploymentName = Prelude.Nothing,
      deploymentModels = Prelude.Nothing,
      deploymentEndTime = Prelude.Nothing,
      deploymentStatusMessage = Prelude.Nothing,
      deploymentStartTime = Prelude.Nothing
    }

-- | Returns the bucket error code.
deploymentResult_deploymentStatus :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.Text)
deploymentResult_deploymentStatus = Lens.lens (\DeploymentResult' {deploymentStatus} -> deploymentStatus) (\s@DeploymentResult' {} a -> s {deploymentStatus = a} :: DeploymentResult)

-- | The name and unique ID of the deployment.
deploymentResult_deploymentName :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.Text)
deploymentResult_deploymentName = Lens.lens (\DeploymentResult' {deploymentName} -> deploymentName) (\s@DeploymentResult' {} a -> s {deploymentName = a} :: DeploymentResult)

-- | Returns a list of models deployed on the agent.
deploymentResult_deploymentModels :: Lens.Lens' DeploymentResult (Prelude.Maybe [DeploymentModel])
deploymentResult_deploymentModels = Lens.lens (\DeploymentResult' {deploymentModels} -> deploymentModels) (\s@DeploymentResult' {} a -> s {deploymentModels = a} :: DeploymentResult) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of when the deployment was ended, and the agent got the
-- deployment results.
deploymentResult_deploymentEndTime :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.UTCTime)
deploymentResult_deploymentEndTime = Lens.lens (\DeploymentResult' {deploymentEndTime} -> deploymentEndTime) (\s@DeploymentResult' {} a -> s {deploymentEndTime = a} :: DeploymentResult) Prelude.. Lens.mapping Data._Time

-- | Returns the detailed error message.
deploymentResult_deploymentStatusMessage :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.Text)
deploymentResult_deploymentStatusMessage = Lens.lens (\DeploymentResult' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@DeploymentResult' {} a -> s {deploymentStatusMessage = a} :: DeploymentResult)

-- | The timestamp of when the deployment was started on the agent.
deploymentResult_deploymentStartTime :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.UTCTime)
deploymentResult_deploymentStartTime = Lens.lens (\DeploymentResult' {deploymentStartTime} -> deploymentStartTime) (\s@DeploymentResult' {} a -> s {deploymentStartTime = a} :: DeploymentResult) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable DeploymentResult where
  hashWithSalt _salt DeploymentResult' {..} =
    _salt `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` deploymentName
      `Prelude.hashWithSalt` deploymentModels
      `Prelude.hashWithSalt` deploymentEndTime
      `Prelude.hashWithSalt` deploymentStatusMessage
      `Prelude.hashWithSalt` deploymentStartTime

instance Prelude.NFData DeploymentResult where
  rnf DeploymentResult' {..} =
    Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf deploymentName
      `Prelude.seq` Prelude.rnf deploymentModels
      `Prelude.seq` Prelude.rnf deploymentEndTime
      `Prelude.seq` Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf deploymentStartTime

instance Data.ToJSON DeploymentResult where
  toJSON DeploymentResult' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeploymentStatus" Data..=)
              Prelude.<$> deploymentStatus,
            ("DeploymentName" Data..=)
              Prelude.<$> deploymentName,
            ("DeploymentModels" Data..=)
              Prelude.<$> deploymentModels,
            ("DeploymentEndTime" Data..=)
              Prelude.<$> deploymentEndTime,
            ("DeploymentStatusMessage" Data..=)
              Prelude.<$> deploymentStatusMessage,
            ("DeploymentStartTime" Data..=)
              Prelude.<$> deploymentStartTime
          ]
      )
