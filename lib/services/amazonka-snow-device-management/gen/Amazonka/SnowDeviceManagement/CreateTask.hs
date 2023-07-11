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
-- Module      : Amazonka.SnowDeviceManagement.CreateTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instructs one or more devices to start a task, such as unlocking or
-- rebooting.
module Amazonka.SnowDeviceManagement.CreateTask
  ( -- * Creating a Request
    CreateTask (..),
    newCreateTask,

    -- * Request Lenses
    createTask_clientToken,
    createTask_description,
    createTask_tags,
    createTask_command,
    createTask_targets,

    -- * Destructuring the Response
    CreateTaskResponse (..),
    newCreateTaskResponse,

    -- * Response Lenses
    createTaskResponse_taskArn,
    createTaskResponse_taskId,
    createTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SnowDeviceManagement.Types

-- | /See:/ 'newCreateTask' smart constructor.
data CreateTask = CreateTask'
  { -- | A token ensuring that the action is called only once with the specified
    -- details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the task and its targets.
    description :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata that you assign to a resource. You can use tags to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The task to be performed. Only one task is executed on a device at a
    -- time.
    command :: Command,
    -- | A list of managed device IDs.
    targets :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createTask_clientToken' - A token ensuring that the action is called only once with the specified
-- details.
--
-- 'description', 'createTask_description' - A description of the task and its targets.
--
-- 'tags', 'createTask_tags' - Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
--
-- 'command', 'createTask_command' - The task to be performed. Only one task is executed on a device at a
-- time.
--
-- 'targets', 'createTask_targets' - A list of managed device IDs.
newCreateTask ::
  -- | 'command'
  Command ->
  -- | 'targets'
  Prelude.NonEmpty Prelude.Text ->
  CreateTask
newCreateTask pCommand_ pTargets_ =
  CreateTask'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      command = pCommand_,
      targets = Lens.coerced Lens.# pTargets_
    }

-- | A token ensuring that the action is called only once with the specified
-- details.
createTask_clientToken :: Lens.Lens' CreateTask (Prelude.Maybe Prelude.Text)
createTask_clientToken = Lens.lens (\CreateTask' {clientToken} -> clientToken) (\s@CreateTask' {} a -> s {clientToken = a} :: CreateTask)

-- | A description of the task and its targets.
createTask_description :: Lens.Lens' CreateTask (Prelude.Maybe Prelude.Text)
createTask_description = Lens.lens (\CreateTask' {description} -> description) (\s@CreateTask' {} a -> s {description = a} :: CreateTask)

-- | Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
createTask_tags :: Lens.Lens' CreateTask (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTask_tags = Lens.lens (\CreateTask' {tags} -> tags) (\s@CreateTask' {} a -> s {tags = a} :: CreateTask) Prelude.. Lens.mapping Lens.coerced

-- | The task to be performed. Only one task is executed on a device at a
-- time.
createTask_command :: Lens.Lens' CreateTask Command
createTask_command = Lens.lens (\CreateTask' {command} -> command) (\s@CreateTask' {} a -> s {command = a} :: CreateTask)

-- | A list of managed device IDs.
createTask_targets :: Lens.Lens' CreateTask (Prelude.NonEmpty Prelude.Text)
createTask_targets = Lens.lens (\CreateTask' {targets} -> targets) (\s@CreateTask' {} a -> s {targets = a} :: CreateTask) Prelude.. Lens.coerced

instance Core.AWSRequest CreateTask where
  type AWSResponse CreateTask = CreateTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTaskResponse'
            Prelude.<$> (x Data..?> "taskArn")
            Prelude.<*> (x Data..?> "taskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTask where
  hashWithSalt _salt CreateTask' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` targets

instance Prelude.NFData CreateTask where
  rnf CreateTask' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf command
      `Prelude.seq` Prelude.rnf targets

instance Data.ToHeaders CreateTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTask where
  toJSON CreateTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("command" Data..= command),
            Prelude.Just ("targets" Data..= targets)
          ]
      )

instance Data.ToPath CreateTask where
  toPath = Prelude.const "/task"

instance Data.ToQuery CreateTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTaskResponse' smart constructor.
data CreateTaskResponse = CreateTaskResponse'
  { -- | The Amazon Resource Name (ARN) of the task that you created.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the task that you created.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskArn', 'createTaskResponse_taskArn' - The Amazon Resource Name (ARN) of the task that you created.
--
-- 'taskId', 'createTaskResponse_taskId' - The ID of the task that you created.
--
-- 'httpStatus', 'createTaskResponse_httpStatus' - The response's http status code.
newCreateTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTaskResponse
newCreateTaskResponse pHttpStatus_ =
  CreateTaskResponse'
    { taskArn = Prelude.Nothing,
      taskId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the task that you created.
createTaskResponse_taskArn :: Lens.Lens' CreateTaskResponse (Prelude.Maybe Prelude.Text)
createTaskResponse_taskArn = Lens.lens (\CreateTaskResponse' {taskArn} -> taskArn) (\s@CreateTaskResponse' {} a -> s {taskArn = a} :: CreateTaskResponse)

-- | The ID of the task that you created.
createTaskResponse_taskId :: Lens.Lens' CreateTaskResponse (Prelude.Maybe Prelude.Text)
createTaskResponse_taskId = Lens.lens (\CreateTaskResponse' {taskId} -> taskId) (\s@CreateTaskResponse' {} a -> s {taskId = a} :: CreateTaskResponse)

-- | The response's http status code.
createTaskResponse_httpStatus :: Lens.Lens' CreateTaskResponse Prelude.Int
createTaskResponse_httpStatus = Lens.lens (\CreateTaskResponse' {httpStatus} -> httpStatus) (\s@CreateTaskResponse' {} a -> s {httpStatus = a} :: CreateTaskResponse)

instance Prelude.NFData CreateTaskResponse where
  rnf CreateTaskResponse' {..} =
    Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf httpStatus
