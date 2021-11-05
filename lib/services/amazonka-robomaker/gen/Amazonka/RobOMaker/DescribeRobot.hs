{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RobOMaker.DescribeRobot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a robot.
module Amazonka.RobOMaker.DescribeRobot
  ( -- * Creating a Request
    DescribeRobot (..),
    newDescribeRobot,

    -- * Request Lenses
    describeRobot_robot,

    -- * Destructuring the Response
    DescribeRobotResponse (..),
    newDescribeRobotResponse,

    -- * Response Lenses
    describeRobotResponse_lastDeploymentJob,
    describeRobotResponse_status,
    describeRobotResponse_arn,
    describeRobotResponse_createdAt,
    describeRobotResponse_greengrassGroupId,
    describeRobotResponse_fleetArn,
    describeRobotResponse_name,
    describeRobotResponse_architecture,
    describeRobotResponse_lastDeploymentTime,
    describeRobotResponse_tags,
    describeRobotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDescribeRobot' smart constructor.
data DescribeRobot = DescribeRobot'
  { -- | The Amazon Resource Name (ARN) of the robot to be described.
    robot :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRobot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'robot', 'describeRobot_robot' - The Amazon Resource Name (ARN) of the robot to be described.
newDescribeRobot ::
  -- | 'robot'
  Prelude.Text ->
  DescribeRobot
newDescribeRobot pRobot_ =
  DescribeRobot' {robot = pRobot_}

-- | The Amazon Resource Name (ARN) of the robot to be described.
describeRobot_robot :: Lens.Lens' DescribeRobot Prelude.Text
describeRobot_robot = Lens.lens (\DescribeRobot' {robot} -> robot) (\s@DescribeRobot' {} a -> s {robot = a} :: DescribeRobot)

instance Core.AWSRequest DescribeRobot where
  type
    AWSResponse DescribeRobot =
      DescribeRobotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRobotResponse'
            Prelude.<$> (x Core..?> "lastDeploymentJob")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "greengrassGroupId")
            Prelude.<*> (x Core..?> "fleetArn")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "architecture")
            Prelude.<*> (x Core..?> "lastDeploymentTime")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRobot

instance Prelude.NFData DescribeRobot

instance Core.ToHeaders DescribeRobot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeRobot where
  toJSON DescribeRobot' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("robot" Core..= robot)]
      )

instance Core.ToPath DescribeRobot where
  toPath = Prelude.const "/describeRobot"

instance Core.ToQuery DescribeRobot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRobotResponse' smart constructor.
data DescribeRobotResponse = DescribeRobotResponse'
  { -- | The Amazon Resource Name (ARN) of the last deployment job.
    lastDeploymentJob :: Prelude.Maybe Prelude.Text,
    -- | The status of the fleet.
    status :: Prelude.Maybe RobotStatus,
    -- | The Amazon Resource Name (ARN) of the robot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the robot was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Greengrass group id.
    greengrassGroupId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the fleet.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the robot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The target architecture of the robot application.
    architecture :: Prelude.Maybe Architecture,
    -- | The time of the last deployment job.
    lastDeploymentTime :: Prelude.Maybe Core.POSIX,
    -- | The list of all tags added to the specified robot.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRobotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastDeploymentJob', 'describeRobotResponse_lastDeploymentJob' - The Amazon Resource Name (ARN) of the last deployment job.
--
-- 'status', 'describeRobotResponse_status' - The status of the fleet.
--
-- 'arn', 'describeRobotResponse_arn' - The Amazon Resource Name (ARN) of the robot.
--
-- 'createdAt', 'describeRobotResponse_createdAt' - The time, in milliseconds since the epoch, when the robot was created.
--
-- 'greengrassGroupId', 'describeRobotResponse_greengrassGroupId' - The Greengrass group id.
--
-- 'fleetArn', 'describeRobotResponse_fleetArn' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'name', 'describeRobotResponse_name' - The name of the robot.
--
-- 'architecture', 'describeRobotResponse_architecture' - The target architecture of the robot application.
--
-- 'lastDeploymentTime', 'describeRobotResponse_lastDeploymentTime' - The time of the last deployment job.
--
-- 'tags', 'describeRobotResponse_tags' - The list of all tags added to the specified robot.
--
-- 'httpStatus', 'describeRobotResponse_httpStatus' - The response's http status code.
newDescribeRobotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRobotResponse
newDescribeRobotResponse pHttpStatus_ =
  DescribeRobotResponse'
    { lastDeploymentJob =
        Prelude.Nothing,
      status = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      greengrassGroupId = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      name = Prelude.Nothing,
      architecture = Prelude.Nothing,
      lastDeploymentTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the last deployment job.
describeRobotResponse_lastDeploymentJob :: Lens.Lens' DescribeRobotResponse (Prelude.Maybe Prelude.Text)
describeRobotResponse_lastDeploymentJob = Lens.lens (\DescribeRobotResponse' {lastDeploymentJob} -> lastDeploymentJob) (\s@DescribeRobotResponse' {} a -> s {lastDeploymentJob = a} :: DescribeRobotResponse)

-- | The status of the fleet.
describeRobotResponse_status :: Lens.Lens' DescribeRobotResponse (Prelude.Maybe RobotStatus)
describeRobotResponse_status = Lens.lens (\DescribeRobotResponse' {status} -> status) (\s@DescribeRobotResponse' {} a -> s {status = a} :: DescribeRobotResponse)

-- | The Amazon Resource Name (ARN) of the robot.
describeRobotResponse_arn :: Lens.Lens' DescribeRobotResponse (Prelude.Maybe Prelude.Text)
describeRobotResponse_arn = Lens.lens (\DescribeRobotResponse' {arn} -> arn) (\s@DescribeRobotResponse' {} a -> s {arn = a} :: DescribeRobotResponse)

-- | The time, in milliseconds since the epoch, when the robot was created.
describeRobotResponse_createdAt :: Lens.Lens' DescribeRobotResponse (Prelude.Maybe Prelude.UTCTime)
describeRobotResponse_createdAt = Lens.lens (\DescribeRobotResponse' {createdAt} -> createdAt) (\s@DescribeRobotResponse' {} a -> s {createdAt = a} :: DescribeRobotResponse) Prelude.. Lens.mapping Core._Time

-- | The Greengrass group id.
describeRobotResponse_greengrassGroupId :: Lens.Lens' DescribeRobotResponse (Prelude.Maybe Prelude.Text)
describeRobotResponse_greengrassGroupId = Lens.lens (\DescribeRobotResponse' {greengrassGroupId} -> greengrassGroupId) (\s@DescribeRobotResponse' {} a -> s {greengrassGroupId = a} :: DescribeRobotResponse)

-- | The Amazon Resource Name (ARN) of the fleet.
describeRobotResponse_fleetArn :: Lens.Lens' DescribeRobotResponse (Prelude.Maybe Prelude.Text)
describeRobotResponse_fleetArn = Lens.lens (\DescribeRobotResponse' {fleetArn} -> fleetArn) (\s@DescribeRobotResponse' {} a -> s {fleetArn = a} :: DescribeRobotResponse)

-- | The name of the robot.
describeRobotResponse_name :: Lens.Lens' DescribeRobotResponse (Prelude.Maybe Prelude.Text)
describeRobotResponse_name = Lens.lens (\DescribeRobotResponse' {name} -> name) (\s@DescribeRobotResponse' {} a -> s {name = a} :: DescribeRobotResponse)

-- | The target architecture of the robot application.
describeRobotResponse_architecture :: Lens.Lens' DescribeRobotResponse (Prelude.Maybe Architecture)
describeRobotResponse_architecture = Lens.lens (\DescribeRobotResponse' {architecture} -> architecture) (\s@DescribeRobotResponse' {} a -> s {architecture = a} :: DescribeRobotResponse)

-- | The time of the last deployment job.
describeRobotResponse_lastDeploymentTime :: Lens.Lens' DescribeRobotResponse (Prelude.Maybe Prelude.UTCTime)
describeRobotResponse_lastDeploymentTime = Lens.lens (\DescribeRobotResponse' {lastDeploymentTime} -> lastDeploymentTime) (\s@DescribeRobotResponse' {} a -> s {lastDeploymentTime = a} :: DescribeRobotResponse) Prelude.. Lens.mapping Core._Time

-- | The list of all tags added to the specified robot.
describeRobotResponse_tags :: Lens.Lens' DescribeRobotResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeRobotResponse_tags = Lens.lens (\DescribeRobotResponse' {tags} -> tags) (\s@DescribeRobotResponse' {} a -> s {tags = a} :: DescribeRobotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRobotResponse_httpStatus :: Lens.Lens' DescribeRobotResponse Prelude.Int
describeRobotResponse_httpStatus = Lens.lens (\DescribeRobotResponse' {httpStatus} -> httpStatus) (\s@DescribeRobotResponse' {} a -> s {httpStatus = a} :: DescribeRobotResponse)

instance Prelude.NFData DescribeRobotResponse
