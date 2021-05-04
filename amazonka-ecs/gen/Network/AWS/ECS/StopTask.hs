{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECS.StopTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running task. Any tags associated with the task will be deleted.
--
-- When StopTask is called on a task, the equivalent of @docker stop@ is
-- issued to the containers running in the task. This results in a
-- @SIGTERM@ value and a default 30-second timeout, after which the
-- @SIGKILL@ value is sent and the containers are forcibly stopped. If the
-- container handles the @SIGTERM@ value gracefully and exits within 30
-- seconds from receiving it, no @SIGKILL@ value is sent.
--
-- The default 30-second timeout can be configured on the Amazon ECS
-- container agent with the @ECS_CONTAINER_STOP_TIMEOUT@ variable. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Network.AWS.ECS.StopTask
  ( -- * Creating a Request
    StopTask (..),
    newStopTask,

    -- * Request Lenses
    stopTask_reason,
    stopTask_cluster,
    stopTask_task,

    -- * Destructuring the Response
    StopTaskResponse (..),
    newStopTaskResponse,

    -- * Response Lenses
    stopTaskResponse_task,
    stopTaskResponse_httpStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopTask' smart constructor.
data StopTask = StopTask'
  { -- | An optional message specified when a task is stopped. For example, if
    -- you are using a custom scheduler, you can use this parameter to specify
    -- the reason for stopping the task here, and the message appears in
    -- subsequent DescribeTasks API operations on this task. Up to 255
    -- characters are allowed in this message.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the task to stop. If you do not specify a cluster, the default
    -- cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The task ID or full Amazon Resource Name (ARN) of the task to stop.
    task :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'stopTask_reason' - An optional message specified when a task is stopped. For example, if
-- you are using a custom scheduler, you can use this parameter to specify
-- the reason for stopping the task here, and the message appears in
-- subsequent DescribeTasks API operations on this task. Up to 255
-- characters are allowed in this message.
--
-- 'cluster', 'stopTask_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task to stop. If you do not specify a cluster, the default
-- cluster is assumed.
--
-- 'task', 'stopTask_task' - The task ID or full Amazon Resource Name (ARN) of the task to stop.
newStopTask ::
  -- | 'task'
  Prelude.Text ->
  StopTask
newStopTask pTask_ =
  StopTask'
    { reason = Prelude.Nothing,
      cluster = Prelude.Nothing,
      task = pTask_
    }

-- | An optional message specified when a task is stopped. For example, if
-- you are using a custom scheduler, you can use this parameter to specify
-- the reason for stopping the task here, and the message appears in
-- subsequent DescribeTasks API operations on this task. Up to 255
-- characters are allowed in this message.
stopTask_reason :: Lens.Lens' StopTask (Prelude.Maybe Prelude.Text)
stopTask_reason = Lens.lens (\StopTask' {reason} -> reason) (\s@StopTask' {} a -> s {reason = a} :: StopTask)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task to stop. If you do not specify a cluster, the default
-- cluster is assumed.
stopTask_cluster :: Lens.Lens' StopTask (Prelude.Maybe Prelude.Text)
stopTask_cluster = Lens.lens (\StopTask' {cluster} -> cluster) (\s@StopTask' {} a -> s {cluster = a} :: StopTask)

-- | The task ID or full Amazon Resource Name (ARN) of the task to stop.
stopTask_task :: Lens.Lens' StopTask Prelude.Text
stopTask_task = Lens.lens (\StopTask' {task} -> task) (\s@StopTask' {} a -> s {task = a} :: StopTask)

instance Prelude.AWSRequest StopTask where
  type Rs StopTask = StopTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopTaskResponse'
            Prelude.<$> (x Prelude..?> "task")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopTask

instance Prelude.NFData StopTask

instance Prelude.ToHeaders StopTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.StopTask" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopTask where
  toJSON StopTask' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("reason" Prelude..=) Prelude.<$> reason,
            ("cluster" Prelude..=) Prelude.<$> cluster,
            Prelude.Just ("task" Prelude..= task)
          ]
      )

instance Prelude.ToPath StopTask where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopTaskResponse' smart constructor.
data StopTaskResponse = StopTaskResponse'
  { -- | The task that was stopped.
    task :: Prelude.Maybe Task,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'task', 'stopTaskResponse_task' - The task that was stopped.
--
-- 'httpStatus', 'stopTaskResponse_httpStatus' - The response's http status code.
newStopTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopTaskResponse
newStopTaskResponse pHttpStatus_ =
  StopTaskResponse'
    { task = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The task that was stopped.
stopTaskResponse_task :: Lens.Lens' StopTaskResponse (Prelude.Maybe Task)
stopTaskResponse_task = Lens.lens (\StopTaskResponse' {task} -> task) (\s@StopTaskResponse' {} a -> s {task = a} :: StopTaskResponse)

-- | The response's http status code.
stopTaskResponse_httpStatus :: Lens.Lens' StopTaskResponse Prelude.Int
stopTaskResponse_httpStatus = Lens.lens (\StopTaskResponse' {httpStatus} -> httpStatus) (\s@StopTaskResponse' {} a -> s {httpStatus = a} :: StopTaskResponse)

instance Prelude.NFData StopTaskResponse
