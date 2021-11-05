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
-- Module      : Amazonka.RobOMaker.RegisterRobot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a robot with a fleet.
module Amazonka.RobOMaker.RegisterRobot
  ( -- * Creating a Request
    RegisterRobot (..),
    newRegisterRobot,

    -- * Request Lenses
    registerRobot_fleet,
    registerRobot_robot,

    -- * Destructuring the Response
    RegisterRobotResponse (..),
    newRegisterRobotResponse,

    -- * Response Lenses
    registerRobotResponse_robot,
    registerRobotResponse_fleet,
    registerRobotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newRegisterRobot' smart constructor.
data RegisterRobot = RegisterRobot'
  { -- | The Amazon Resource Name (ARN) of the fleet.
    fleet :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the robot.
    robot :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterRobot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleet', 'registerRobot_fleet' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'robot', 'registerRobot_robot' - The Amazon Resource Name (ARN) of the robot.
newRegisterRobot ::
  -- | 'fleet'
  Prelude.Text ->
  -- | 'robot'
  Prelude.Text ->
  RegisterRobot
newRegisterRobot pFleet_ pRobot_ =
  RegisterRobot' {fleet = pFleet_, robot = pRobot_}

-- | The Amazon Resource Name (ARN) of the fleet.
registerRobot_fleet :: Lens.Lens' RegisterRobot Prelude.Text
registerRobot_fleet = Lens.lens (\RegisterRobot' {fleet} -> fleet) (\s@RegisterRobot' {} a -> s {fleet = a} :: RegisterRobot)

-- | The Amazon Resource Name (ARN) of the robot.
registerRobot_robot :: Lens.Lens' RegisterRobot Prelude.Text
registerRobot_robot = Lens.lens (\RegisterRobot' {robot} -> robot) (\s@RegisterRobot' {} a -> s {robot = a} :: RegisterRobot)

instance Core.AWSRequest RegisterRobot where
  type
    AWSResponse RegisterRobot =
      RegisterRobotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterRobotResponse'
            Prelude.<$> (x Core..?> "robot")
            Prelude.<*> (x Core..?> "fleet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterRobot

instance Prelude.NFData RegisterRobot

instance Core.ToHeaders RegisterRobot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterRobot where
  toJSON RegisterRobot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("fleet" Core..= fleet),
            Prelude.Just ("robot" Core..= robot)
          ]
      )

instance Core.ToPath RegisterRobot where
  toPath = Prelude.const "/registerRobot"

instance Core.ToQuery RegisterRobot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterRobotResponse' smart constructor.
data RegisterRobotResponse = RegisterRobotResponse'
  { -- | Information about the robot registration.
    robot :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the fleet that the robot will join.
    fleet :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterRobotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'robot', 'registerRobotResponse_robot' - Information about the robot registration.
--
-- 'fleet', 'registerRobotResponse_fleet' - The Amazon Resource Name (ARN) of the fleet that the robot will join.
--
-- 'httpStatus', 'registerRobotResponse_httpStatus' - The response's http status code.
newRegisterRobotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterRobotResponse
newRegisterRobotResponse pHttpStatus_ =
  RegisterRobotResponse'
    { robot = Prelude.Nothing,
      fleet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the robot registration.
registerRobotResponse_robot :: Lens.Lens' RegisterRobotResponse (Prelude.Maybe Prelude.Text)
registerRobotResponse_robot = Lens.lens (\RegisterRobotResponse' {robot} -> robot) (\s@RegisterRobotResponse' {} a -> s {robot = a} :: RegisterRobotResponse)

-- | The Amazon Resource Name (ARN) of the fleet that the robot will join.
registerRobotResponse_fleet :: Lens.Lens' RegisterRobotResponse (Prelude.Maybe Prelude.Text)
registerRobotResponse_fleet = Lens.lens (\RegisterRobotResponse' {fleet} -> fleet) (\s@RegisterRobotResponse' {} a -> s {fleet = a} :: RegisterRobotResponse)

-- | The response's http status code.
registerRobotResponse_httpStatus :: Lens.Lens' RegisterRobotResponse Prelude.Int
registerRobotResponse_httpStatus = Lens.lens (\RegisterRobotResponse' {httpStatus} -> httpStatus) (\s@RegisterRobotResponse' {} a -> s {httpStatus = a} :: RegisterRobotResponse)

instance Prelude.NFData RegisterRobotResponse
