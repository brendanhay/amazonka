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
-- Module      : Network.AWS.RobOMaker.Types.Robot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RobOMaker.Types.Robot where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RobOMaker.Types.Architecture
import Network.AWS.RobOMaker.Types.RobotStatus

-- | Information about a robot.
--
-- /See:/ 'newRobot' smart constructor.
data Robot = Robot'
  { -- | The Amazon Resource Name (ARN) of the last deployment job.
    lastDeploymentJob :: Prelude.Maybe Prelude.Text,
    -- | The status of the robot.
    status :: Prelude.Maybe RobotStatus,
    -- | The Amazon Resource Name (ARN) of the robot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the robot was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Greengrass group associated with the robot.
    greenGrassGroupId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the fleet.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the robot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The architecture of the robot.
    architecture :: Prelude.Maybe Architecture,
    -- | The time of the last deployment.
    lastDeploymentTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Robot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastDeploymentJob', 'robot_lastDeploymentJob' - The Amazon Resource Name (ARN) of the last deployment job.
--
-- 'status', 'robot_status' - The status of the robot.
--
-- 'arn', 'robot_arn' - The Amazon Resource Name (ARN) of the robot.
--
-- 'createdAt', 'robot_createdAt' - The time, in milliseconds since the epoch, when the robot was created.
--
-- 'greenGrassGroupId', 'robot_greenGrassGroupId' - The Greengrass group associated with the robot.
--
-- 'fleetArn', 'robot_fleetArn' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'name', 'robot_name' - The name of the robot.
--
-- 'architecture', 'robot_architecture' - The architecture of the robot.
--
-- 'lastDeploymentTime', 'robot_lastDeploymentTime' - The time of the last deployment.
newRobot ::
  Robot
newRobot =
  Robot'
    { lastDeploymentJob = Prelude.Nothing,
      status = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      greenGrassGroupId = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      name = Prelude.Nothing,
      architecture = Prelude.Nothing,
      lastDeploymentTime = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the last deployment job.
robot_lastDeploymentJob :: Lens.Lens' Robot (Prelude.Maybe Prelude.Text)
robot_lastDeploymentJob = Lens.lens (\Robot' {lastDeploymentJob} -> lastDeploymentJob) (\s@Robot' {} a -> s {lastDeploymentJob = a} :: Robot)

-- | The status of the robot.
robot_status :: Lens.Lens' Robot (Prelude.Maybe RobotStatus)
robot_status = Lens.lens (\Robot' {status} -> status) (\s@Robot' {} a -> s {status = a} :: Robot)

-- | The Amazon Resource Name (ARN) of the robot.
robot_arn :: Lens.Lens' Robot (Prelude.Maybe Prelude.Text)
robot_arn = Lens.lens (\Robot' {arn} -> arn) (\s@Robot' {} a -> s {arn = a} :: Robot)

-- | The time, in milliseconds since the epoch, when the robot was created.
robot_createdAt :: Lens.Lens' Robot (Prelude.Maybe Prelude.UTCTime)
robot_createdAt = Lens.lens (\Robot' {createdAt} -> createdAt) (\s@Robot' {} a -> s {createdAt = a} :: Robot) Prelude.. Lens.mapping Core._Time

-- | The Greengrass group associated with the robot.
robot_greenGrassGroupId :: Lens.Lens' Robot (Prelude.Maybe Prelude.Text)
robot_greenGrassGroupId = Lens.lens (\Robot' {greenGrassGroupId} -> greenGrassGroupId) (\s@Robot' {} a -> s {greenGrassGroupId = a} :: Robot)

-- | The Amazon Resource Name (ARN) of the fleet.
robot_fleetArn :: Lens.Lens' Robot (Prelude.Maybe Prelude.Text)
robot_fleetArn = Lens.lens (\Robot' {fleetArn} -> fleetArn) (\s@Robot' {} a -> s {fleetArn = a} :: Robot)

-- | The name of the robot.
robot_name :: Lens.Lens' Robot (Prelude.Maybe Prelude.Text)
robot_name = Lens.lens (\Robot' {name} -> name) (\s@Robot' {} a -> s {name = a} :: Robot)

-- | The architecture of the robot.
robot_architecture :: Lens.Lens' Robot (Prelude.Maybe Architecture)
robot_architecture = Lens.lens (\Robot' {architecture} -> architecture) (\s@Robot' {} a -> s {architecture = a} :: Robot)

-- | The time of the last deployment.
robot_lastDeploymentTime :: Lens.Lens' Robot (Prelude.Maybe Prelude.UTCTime)
robot_lastDeploymentTime = Lens.lens (\Robot' {lastDeploymentTime} -> lastDeploymentTime) (\s@Robot' {} a -> s {lastDeploymentTime = a} :: Robot) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Robot where
  parseJSON =
    Core.withObject
      "Robot"
      ( \x ->
          Robot'
            Prelude.<$> (x Core..:? "lastDeploymentJob")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "greenGrassGroupId")
            Prelude.<*> (x Core..:? "fleetArn")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "architecture")
            Prelude.<*> (x Core..:? "lastDeploymentTime")
      )

instance Prelude.Hashable Robot

instance Prelude.NFData Robot
