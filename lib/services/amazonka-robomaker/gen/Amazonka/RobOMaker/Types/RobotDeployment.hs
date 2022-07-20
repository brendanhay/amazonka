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
-- Module      : Amazonka.RobOMaker.Types.RobotDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.RobotDeployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.DeploymentJobErrorCode
import Amazonka.RobOMaker.Types.ProgressDetail
import Amazonka.RobOMaker.Types.RobotStatus

-- | Information about a robot deployment.
--
-- /See:/ 'newRobotDeployment' smart constructor.
data RobotDeployment = RobotDeployment'
  { -- | The time, in milliseconds since the epoch, when the deployment finished.
    deploymentFinishTime :: Prelude.Maybe Core.POSIX,
    -- | The robot deployment failure code.
    failureCode :: Prelude.Maybe DeploymentJobErrorCode,
    -- | The robot deployment Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | Information about how the deployment is progressing.
    progressDetail :: Prelude.Maybe ProgressDetail,
    -- | The status of the robot deployment.
    status :: Prelude.Maybe RobotStatus,
    -- | The time, in milliseconds since the epoch, when the deployment was
    -- started.
    deploymentStartTime :: Prelude.Maybe Core.POSIX,
    -- | A short description of the reason why the robot deployment failed.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RobotDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentFinishTime', 'robotDeployment_deploymentFinishTime' - The time, in milliseconds since the epoch, when the deployment finished.
--
-- 'failureCode', 'robotDeployment_failureCode' - The robot deployment failure code.
--
-- 'arn', 'robotDeployment_arn' - The robot deployment Amazon Resource Name (ARN).
--
-- 'progressDetail', 'robotDeployment_progressDetail' - Information about how the deployment is progressing.
--
-- 'status', 'robotDeployment_status' - The status of the robot deployment.
--
-- 'deploymentStartTime', 'robotDeployment_deploymentStartTime' - The time, in milliseconds since the epoch, when the deployment was
-- started.
--
-- 'failureReason', 'robotDeployment_failureReason' - A short description of the reason why the robot deployment failed.
newRobotDeployment ::
  RobotDeployment
newRobotDeployment =
  RobotDeployment'
    { deploymentFinishTime =
        Prelude.Nothing,
      failureCode = Prelude.Nothing,
      arn = Prelude.Nothing,
      progressDetail = Prelude.Nothing,
      status = Prelude.Nothing,
      deploymentStartTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The time, in milliseconds since the epoch, when the deployment finished.
robotDeployment_deploymentFinishTime :: Lens.Lens' RobotDeployment (Prelude.Maybe Prelude.UTCTime)
robotDeployment_deploymentFinishTime = Lens.lens (\RobotDeployment' {deploymentFinishTime} -> deploymentFinishTime) (\s@RobotDeployment' {} a -> s {deploymentFinishTime = a} :: RobotDeployment) Prelude.. Lens.mapping Core._Time

-- | The robot deployment failure code.
robotDeployment_failureCode :: Lens.Lens' RobotDeployment (Prelude.Maybe DeploymentJobErrorCode)
robotDeployment_failureCode = Lens.lens (\RobotDeployment' {failureCode} -> failureCode) (\s@RobotDeployment' {} a -> s {failureCode = a} :: RobotDeployment)

-- | The robot deployment Amazon Resource Name (ARN).
robotDeployment_arn :: Lens.Lens' RobotDeployment (Prelude.Maybe Prelude.Text)
robotDeployment_arn = Lens.lens (\RobotDeployment' {arn} -> arn) (\s@RobotDeployment' {} a -> s {arn = a} :: RobotDeployment)

-- | Information about how the deployment is progressing.
robotDeployment_progressDetail :: Lens.Lens' RobotDeployment (Prelude.Maybe ProgressDetail)
robotDeployment_progressDetail = Lens.lens (\RobotDeployment' {progressDetail} -> progressDetail) (\s@RobotDeployment' {} a -> s {progressDetail = a} :: RobotDeployment)

-- | The status of the robot deployment.
robotDeployment_status :: Lens.Lens' RobotDeployment (Prelude.Maybe RobotStatus)
robotDeployment_status = Lens.lens (\RobotDeployment' {status} -> status) (\s@RobotDeployment' {} a -> s {status = a} :: RobotDeployment)

-- | The time, in milliseconds since the epoch, when the deployment was
-- started.
robotDeployment_deploymentStartTime :: Lens.Lens' RobotDeployment (Prelude.Maybe Prelude.UTCTime)
robotDeployment_deploymentStartTime = Lens.lens (\RobotDeployment' {deploymentStartTime} -> deploymentStartTime) (\s@RobotDeployment' {} a -> s {deploymentStartTime = a} :: RobotDeployment) Prelude.. Lens.mapping Core._Time

-- | A short description of the reason why the robot deployment failed.
robotDeployment_failureReason :: Lens.Lens' RobotDeployment (Prelude.Maybe Prelude.Text)
robotDeployment_failureReason = Lens.lens (\RobotDeployment' {failureReason} -> failureReason) (\s@RobotDeployment' {} a -> s {failureReason = a} :: RobotDeployment)

instance Core.FromJSON RobotDeployment where
  parseJSON =
    Core.withObject
      "RobotDeployment"
      ( \x ->
          RobotDeployment'
            Prelude.<$> (x Core..:? "deploymentFinishTime")
            Prelude.<*> (x Core..:? "failureCode")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "progressDetail")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "deploymentStartTime")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance Prelude.Hashable RobotDeployment where
  hashWithSalt _salt RobotDeployment' {..} =
    _salt `Prelude.hashWithSalt` deploymentFinishTime
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` progressDetail
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` deploymentStartTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData RobotDeployment where
  rnf RobotDeployment' {..} =
    Prelude.rnf deploymentFinishTime
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf progressDetail
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf deploymentStartTime
      `Prelude.seq` Prelude.rnf failureReason
