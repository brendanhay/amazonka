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
-- Module      : Network.AWS.RobOMaker.DeregisterRobot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a robot.
module Network.AWS.RobOMaker.DeregisterRobot
  ( -- * Creating a Request
    DeregisterRobot (..),
    newDeregisterRobot,

    -- * Request Lenses
    deregisterRobot_fleet,
    deregisterRobot_robot,

    -- * Destructuring the Response
    DeregisterRobotResponse (..),
    newDeregisterRobotResponse,

    -- * Response Lenses
    deregisterRobotResponse_robot,
    deregisterRobotResponse_fleet,
    deregisterRobotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.RobOMaker.Types

-- | /See:/ 'newDeregisterRobot' smart constructor.
data DeregisterRobot = DeregisterRobot'
  { -- | The Amazon Resource Name (ARN) of the fleet.
    fleet :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the robot.
    robot :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterRobot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleet', 'deregisterRobot_fleet' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'robot', 'deregisterRobot_robot' - The Amazon Resource Name (ARN) of the robot.
newDeregisterRobot ::
  -- | 'fleet'
  Prelude.Text ->
  -- | 'robot'
  Prelude.Text ->
  DeregisterRobot
newDeregisterRobot pFleet_ pRobot_ =
  DeregisterRobot' {fleet = pFleet_, robot = pRobot_}

-- | The Amazon Resource Name (ARN) of the fleet.
deregisterRobot_fleet :: Lens.Lens' DeregisterRobot Prelude.Text
deregisterRobot_fleet = Lens.lens (\DeregisterRobot' {fleet} -> fleet) (\s@DeregisterRobot' {} a -> s {fleet = a} :: DeregisterRobot)

-- | The Amazon Resource Name (ARN) of the robot.
deregisterRobot_robot :: Lens.Lens' DeregisterRobot Prelude.Text
deregisterRobot_robot = Lens.lens (\DeregisterRobot' {robot} -> robot) (\s@DeregisterRobot' {} a -> s {robot = a} :: DeregisterRobot)

instance Core.AWSRequest DeregisterRobot where
  type
    AWSResponse DeregisterRobot =
      DeregisterRobotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterRobotResponse'
            Prelude.<$> (x Core..?> "robot")
            Prelude.<*> (x Core..?> "fleet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterRobot

instance Prelude.NFData DeregisterRobot

instance Core.ToHeaders DeregisterRobot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeregisterRobot where
  toJSON DeregisterRobot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("fleet" Core..= fleet),
            Prelude.Just ("robot" Core..= robot)
          ]
      )

instance Core.ToPath DeregisterRobot where
  toPath = Prelude.const "/deregisterRobot"

instance Core.ToQuery DeregisterRobot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterRobotResponse' smart constructor.
data DeregisterRobotResponse = DeregisterRobotResponse'
  { -- | The Amazon Resource Name (ARN) of the robot.
    robot :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the fleet.
    fleet :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterRobotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'robot', 'deregisterRobotResponse_robot' - The Amazon Resource Name (ARN) of the robot.
--
-- 'fleet', 'deregisterRobotResponse_fleet' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'httpStatus', 'deregisterRobotResponse_httpStatus' - The response's http status code.
newDeregisterRobotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterRobotResponse
newDeregisterRobotResponse pHttpStatus_ =
  DeregisterRobotResponse'
    { robot = Prelude.Nothing,
      fleet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the robot.
deregisterRobotResponse_robot :: Lens.Lens' DeregisterRobotResponse (Prelude.Maybe Prelude.Text)
deregisterRobotResponse_robot = Lens.lens (\DeregisterRobotResponse' {robot} -> robot) (\s@DeregisterRobotResponse' {} a -> s {robot = a} :: DeregisterRobotResponse)

-- | The Amazon Resource Name (ARN) of the fleet.
deregisterRobotResponse_fleet :: Lens.Lens' DeregisterRobotResponse (Prelude.Maybe Prelude.Text)
deregisterRobotResponse_fleet = Lens.lens (\DeregisterRobotResponse' {fleet} -> fleet) (\s@DeregisterRobotResponse' {} a -> s {fleet = a} :: DeregisterRobotResponse)

-- | The response's http status code.
deregisterRobotResponse_httpStatus :: Lens.Lens' DeregisterRobotResponse Prelude.Int
deregisterRobotResponse_httpStatus = Lens.lens (\DeregisterRobotResponse' {httpStatus} -> httpStatus) (\s@DeregisterRobotResponse' {} a -> s {httpStatus = a} :: DeregisterRobotResponse)

instance Prelude.NFData DeregisterRobotResponse
