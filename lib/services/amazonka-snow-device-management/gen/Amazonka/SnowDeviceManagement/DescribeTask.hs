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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeTaskResponse_tags,
    describeTaskResponse_lastUpdatedAt,
    describeTaskResponse_taskArn,
    describeTaskResponse_taskId,
    describeTaskResponse_state,
    describeTaskResponse_targets,
    describeTaskResponse_description,
    describeTaskResponse_completedAt,
    describeTaskResponse_createdAt,
    describeTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTaskResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "taskArn")
            Prelude.<*> (x Data..?> "taskId")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (x Data..?> "targets")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "completedAt")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTask where
  hashWithSalt _salt DescribeTask' {..} =
    _salt `Prelude.hashWithSalt` taskId

instance Prelude.NFData DescribeTask where
  rnf DescribeTask' {..} = Prelude.rnf taskId

instance Data.ToHeaders DescribeTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTask where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeTask where
  toPath DescribeTask' {..} =
    Prelude.mconcat ["/task/", Data.toBS taskId]

instance Data.ToQuery DescribeTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTaskResponse' smart constructor.
data DescribeTaskResponse = DescribeTaskResponse'
  { -- | Optional metadata that you assign to a resource. You can use tags to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | When the state of the task was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the task.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the task.
    state :: Prelude.Maybe TaskState,
    -- | The managed devices that the task was sent to.
    targets :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The description provided of the task and managed devices.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the task was completed.
    completedAt :: Prelude.Maybe Data.POSIX,
    -- | When the @CreateTask@ operation was called.
    createdAt :: Prelude.Maybe Data.POSIX,
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
-- 'tags', 'describeTaskResponse_tags' - Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
--
-- 'lastUpdatedAt', 'describeTaskResponse_lastUpdatedAt' - When the state of the task was last updated.
--
-- 'taskArn', 'describeTaskResponse_taskArn' - The Amazon Resource Name (ARN) of the task.
--
-- 'taskId', 'describeTaskResponse_taskId' - The ID of the task.
--
-- 'state', 'describeTaskResponse_state' - The current state of the task.
--
-- 'targets', 'describeTaskResponse_targets' - The managed devices that the task was sent to.
--
-- 'description', 'describeTaskResponse_description' - The description provided of the task and managed devices.
--
-- 'completedAt', 'describeTaskResponse_completedAt' - When the task was completed.
--
-- 'createdAt', 'describeTaskResponse_createdAt' - When the @CreateTask@ operation was called.
--
-- 'httpStatus', 'describeTaskResponse_httpStatus' - The response's http status code.
newDescribeTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTaskResponse
newDescribeTaskResponse pHttpStatus_ =
  DescribeTaskResponse'
    { tags = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      taskId = Prelude.Nothing,
      state = Prelude.Nothing,
      targets = Prelude.Nothing,
      description = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
describeTaskResponse_tags :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeTaskResponse_tags = Lens.lens (\DescribeTaskResponse' {tags} -> tags) (\s@DescribeTaskResponse' {} a -> s {tags = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | When the state of the task was last updated.
describeTaskResponse_lastUpdatedAt :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeTaskResponse_lastUpdatedAt = Lens.lens (\DescribeTaskResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeTaskResponse' {} a -> s {lastUpdatedAt = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the task.
describeTaskResponse_taskArn :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_taskArn = Lens.lens (\DescribeTaskResponse' {taskArn} -> taskArn) (\s@DescribeTaskResponse' {} a -> s {taskArn = a} :: DescribeTaskResponse)

-- | The ID of the task.
describeTaskResponse_taskId :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_taskId = Lens.lens (\DescribeTaskResponse' {taskId} -> taskId) (\s@DescribeTaskResponse' {} a -> s {taskId = a} :: DescribeTaskResponse)

-- | The current state of the task.
describeTaskResponse_state :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe TaskState)
describeTaskResponse_state = Lens.lens (\DescribeTaskResponse' {state} -> state) (\s@DescribeTaskResponse' {} a -> s {state = a} :: DescribeTaskResponse)

-- | The managed devices that the task was sent to.
describeTaskResponse_targets :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeTaskResponse_targets = Lens.lens (\DescribeTaskResponse' {targets} -> targets) (\s@DescribeTaskResponse' {} a -> s {targets = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The description provided of the task and managed devices.
describeTaskResponse_description :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_description = Lens.lens (\DescribeTaskResponse' {description} -> description) (\s@DescribeTaskResponse' {} a -> s {description = a} :: DescribeTaskResponse)

-- | When the task was completed.
describeTaskResponse_completedAt :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeTaskResponse_completedAt = Lens.lens (\DescribeTaskResponse' {completedAt} -> completedAt) (\s@DescribeTaskResponse' {} a -> s {completedAt = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Data._Time

-- | When the @CreateTask@ operation was called.
describeTaskResponse_createdAt :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeTaskResponse_createdAt = Lens.lens (\DescribeTaskResponse' {createdAt} -> createdAt) (\s@DescribeTaskResponse' {} a -> s {createdAt = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeTaskResponse_httpStatus :: Lens.Lens' DescribeTaskResponse Prelude.Int
describeTaskResponse_httpStatus = Lens.lens (\DescribeTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeTaskResponse' {} a -> s {httpStatus = a} :: DescribeTaskResponse)

instance Prelude.NFData DescribeTaskResponse where
  rnf DescribeTaskResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf completedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf httpStatus
