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
-- Module      : Network.AWS.RobOMaker.DeleteRobot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a robot.
module Network.AWS.RobOMaker.DeleteRobot
  ( -- * Creating a Request
    DeleteRobot (..),
    newDeleteRobot,

    -- * Request Lenses
    deleteRobot_robot,

    -- * Destructuring the Response
    DeleteRobotResponse (..),
    newDeleteRobotResponse,

    -- * Response Lenses
    deleteRobotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.RobOMaker.Types

-- | /See:/ 'newDeleteRobot' smart constructor.
data DeleteRobot = DeleteRobot'
  { -- | The Amazon Resource Name (ARN) of the robot.
    robot :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRobot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'robot', 'deleteRobot_robot' - The Amazon Resource Name (ARN) of the robot.
newDeleteRobot ::
  -- | 'robot'
  Prelude.Text ->
  DeleteRobot
newDeleteRobot pRobot_ =
  DeleteRobot' {robot = pRobot_}

-- | The Amazon Resource Name (ARN) of the robot.
deleteRobot_robot :: Lens.Lens' DeleteRobot Prelude.Text
deleteRobot_robot = Lens.lens (\DeleteRobot' {robot} -> robot) (\s@DeleteRobot' {} a -> s {robot = a} :: DeleteRobot)

instance Core.AWSRequest DeleteRobot where
  type AWSResponse DeleteRobot = DeleteRobotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRobotResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRobot

instance Prelude.NFData DeleteRobot

instance Core.ToHeaders DeleteRobot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteRobot where
  toJSON DeleteRobot' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("robot" Core..= robot)]
      )

instance Core.ToPath DeleteRobot where
  toPath = Prelude.const "/deleteRobot"

instance Core.ToQuery DeleteRobot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRobotResponse' smart constructor.
data DeleteRobotResponse = DeleteRobotResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRobotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRobotResponse_httpStatus' - The response's http status code.
newDeleteRobotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRobotResponse
newDeleteRobotResponse pHttpStatus_ =
  DeleteRobotResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRobotResponse_httpStatus :: Lens.Lens' DeleteRobotResponse Prelude.Int
deleteRobotResponse_httpStatus = Lens.lens (\DeleteRobotResponse' {httpStatus} -> httpStatus) (\s@DeleteRobotResponse' {} a -> s {httpStatus = a} :: DeleteRobotResponse)

instance Prelude.NFData DeleteRobotResponse
