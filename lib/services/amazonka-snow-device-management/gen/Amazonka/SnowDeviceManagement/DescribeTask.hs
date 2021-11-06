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
-- Module      : Amazonka.SnowDeviceManagement.DescribeTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the metadata for a given task on a device.
module Amazonka.SnowDeviceManagement.DescribeTask
  ( -- * Creating a Request
    DescribeTask (..),
    newDescribeTask,

    -- * Request Lenses
    describeTask_taskId,

    -- * Destructuring the Response
    DescribeTaskResponse (..),
    newDescribeTaskResponse,

    -- * Response Lenses
    describeTaskResponse_state,
    describeTaskResponse_lastUpdatedAt,
    describeTaskResponse_createdAt,
    describeTaskResponse_taskId,
    describeTaskResponse_taskArn,
    describeTaskResponse_targets,
    describeTaskResponse_completedAt,
    describeTaskResponse_description,
    describeTaskResponse_tags,
    describeTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SnowDeviceManagement.Types

-- | /See:/ 'newDescribeTask' smart constructor.
data DescribeTask = DescribeTask'
  { -- | The ID of the task to be described.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'describeTask_taskId' - The ID of the task to be described.
newDescribeTask ::
  -- | 'taskId'
  Prelude.Text ->
  DescribeTask
newDescribeTask pTaskId_ =
  DescribeTask' {taskId = pTaskId_}

-- | The ID of the task to be described.
describeTask_taskId :: Lens.Lens' DescribeTask Prelude.Text
describeTask_taskId = Lens.lens (\DescribeTask' {taskId} -> taskId) (\s@DescribeTask' {} a -> s {taskId = a} :: DescribeTask)

instance Core.AWSRequest DescribeTask where
  type AWSResponse DescribeTask = DescribeTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTaskResponse'
            Prelude.<$> (x Core..?> "state")
            Prelude.<*> (x Core..?> "lastUpdatedAt")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "taskId")
            Prelude.<*> (x Core..?> "taskArn")
            Prelude.<*> (x Core..?> "targets")
            Prelude.<*> (x Core..?> "completedAt")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTask

instance Prelude.NFData DescribeTask

instance Core.ToHeaders DescribeTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTask where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DescribeTask where
  toPath DescribeTask' {..} =
    Prelude.mconcat ["/task/", Core.toBS taskId]

instance Core.ToQuery DescribeTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTaskResponse' smart constructor.
data DescribeTaskResponse = DescribeTaskResponse'
  { -- | The current state of the task.
    state :: Prelude.Maybe TaskState,
    -- | When the state of the task was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | When the @CreateTask@ operation was called.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the task.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The managed devices that the task was sent to.
    targets :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | When the task was completed.
    completedAt :: Prelude.Maybe Core.POSIX,
    -- | The description provided of the task and managed devices.
    description :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata that you assign to a resource. You can use tags to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'describeTaskResponse_state' - The current state of the task.
--
-- 'lastUpdatedAt', 'describeTaskResponse_lastUpdatedAt' - When the state of the task was last updated.
--
-- 'createdAt', 'describeTaskResponse_createdAt' - When the @CreateTask@ operation was called.
--
-- 'taskId', 'describeTaskResponse_taskId' - The ID of the task.
--
-- 'taskArn', 'describeTaskResponse_taskArn' - The Amazon Resource Name (ARN) of the task.
--
-- 'targets', 'describeTaskResponse_targets' - The managed devices that the task was sent to.
--
-- 'completedAt', 'describeTaskResponse_completedAt' - When the task was completed.
--
-- 'description', 'describeTaskResponse_description' - The description provided of the task and managed devices.
--
-- 'tags', 'describeTaskResponse_tags' - Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
--
-- 'httpStatus', 'describeTaskResponse_httpStatus' - The response's http status code.
newDescribeTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTaskResponse
newDescribeTaskResponse pHttpStatus_ =
  DescribeTaskResponse'
    { state = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      taskId = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      targets = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the task.
describeTaskResponse_state :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe TaskState)
describeTaskResponse_state = Lens.lens (\DescribeTaskResponse' {state} -> state) (\s@DescribeTaskResponse' {} a -> s {state = a} :: DescribeTaskResponse)

-- | When the state of the task was last updated.
describeTaskResponse_lastUpdatedAt :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeTaskResponse_lastUpdatedAt = Lens.lens (\DescribeTaskResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeTaskResponse' {} a -> s {lastUpdatedAt = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Core._Time

-- | When the @CreateTask@ operation was called.
describeTaskResponse_createdAt :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeTaskResponse_createdAt = Lens.lens (\DescribeTaskResponse' {createdAt} -> createdAt) (\s@DescribeTaskResponse' {} a -> s {createdAt = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the task.
describeTaskResponse_taskId :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_taskId = Lens.lens (\DescribeTaskResponse' {taskId} -> taskId) (\s@DescribeTaskResponse' {} a -> s {taskId = a} :: DescribeTaskResponse)

-- | The Amazon Resource Name (ARN) of the task.
describeTaskResponse_taskArn :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_taskArn = Lens.lens (\DescribeTaskResponse' {taskArn} -> taskArn) (\s@DescribeTaskResponse' {} a -> s {taskArn = a} :: DescribeTaskResponse)

-- | The managed devices that the task was sent to.
describeTaskResponse_targets :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeTaskResponse_targets = Lens.lens (\DescribeTaskResponse' {targets} -> targets) (\s@DescribeTaskResponse' {} a -> s {targets = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | When the task was completed.
describeTaskResponse_completedAt :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeTaskResponse_completedAt = Lens.lens (\DescribeTaskResponse' {completedAt} -> completedAt) (\s@DescribeTaskResponse' {} a -> s {completedAt = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The description provided of the task and managed devices.
describeTaskResponse_description :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_description = Lens.lens (\DescribeTaskResponse' {description} -> description) (\s@DescribeTaskResponse' {} a -> s {description = a} :: DescribeTaskResponse)

-- | Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
describeTaskResponse_tags :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeTaskResponse_tags = Lens.lens (\DescribeTaskResponse' {tags} -> tags) (\s@DescribeTaskResponse' {} a -> s {tags = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTaskResponse_httpStatus :: Lens.Lens' DescribeTaskResponse Prelude.Int
describeTaskResponse_httpStatus = Lens.lens (\DescribeTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeTaskResponse' {} a -> s {httpStatus = a} :: DescribeTaskResponse)

instance Prelude.NFData DescribeTaskResponse
