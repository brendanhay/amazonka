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
-- Module      : Amazonka.DataSync.CreateTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures a task, which defines where and how DataSync transfers your
-- data.
--
-- A task includes a source location, a destination location, and the
-- preferences for how and when you want to transfer your data (such as
-- bandwidth limits, scheduling, among other options).
module Amazonka.DataSync.CreateTask
  ( -- * Creating a Request
    CreateTask (..),
    newCreateTask,

    -- * Request Lenses
    createTask_cloudWatchLogGroupArn,
    createTask_excludes,
    createTask_includes,
    createTask_name,
    createTask_options,
    createTask_schedule,
    createTask_tags,
    createTask_sourceLocationArn,
    createTask_destinationLocationArn,

    -- * Destructuring the Response
    CreateTaskResponse (..),
    newCreateTaskResponse,

    -- * Response Lenses
    createTaskResponse_taskArn,
    createTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | CreateTaskRequest
--
-- /See:/ 'newCreateTask' smart constructor.
data CreateTask = CreateTask'
  { -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
    -- is used to monitor and log events in the task.
    cloudWatchLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies a list of filter rules that exclude specific data during your
    -- transfer. For more information and examples, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
    excludes :: Prelude.Maybe [FilterRule],
    -- | Specifies a list of filter rules that include specific data during your
    -- transfer. For more information and examples, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
    includes :: Prelude.Maybe [FilterRule],
    -- | The name of a task. This value is a text reference that is used to
    -- identify the task in the console.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the configuration options for a task. Some options include
    -- preserving file or object metadata and verifying data integrity.
    --
    -- You can also override these options before starting an individual run of
    -- a task (also known as a /task execution/). For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>.
    options :: Prelude.Maybe Options,
    -- | Specifies a schedule used to periodically transfer files from a source
    -- to a destination location. The schedule should be specified in UTC time.
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/task-scheduling.html Scheduling your task>.
    schedule :: Prelude.Maybe TaskSchedule,
    -- | Specifies the tags that you want to apply to the Amazon Resource Name
    -- (ARN) representing the task.
    --
    -- /Tags/ are key-value pairs that help you manage, filter, and search for
    -- your DataSync resources.
    tags :: Prelude.Maybe [TagListEntry],
    -- | The Amazon Resource Name (ARN) of the source location for the task.
    sourceLocationArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Amazon Web Services storage
    -- resource\'s location.
    destinationLocationArn :: Prelude.Text
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
-- 'cloudWatchLogGroupArn', 'createTask_cloudWatchLogGroupArn' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- is used to monitor and log events in the task.
--
-- 'excludes', 'createTask_excludes' - Specifies a list of filter rules that exclude specific data during your
-- transfer. For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
--
-- 'includes', 'createTask_includes' - Specifies a list of filter rules that include specific data during your
-- transfer. For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
--
-- 'name', 'createTask_name' - The name of a task. This value is a text reference that is used to
-- identify the task in the console.
--
-- 'options', 'createTask_options' - Specifies the configuration options for a task. Some options include
-- preserving file or object metadata and verifying data integrity.
--
-- You can also override these options before starting an individual run of
-- a task (also known as a /task execution/). For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>.
--
-- 'schedule', 'createTask_schedule' - Specifies a schedule used to periodically transfer files from a source
-- to a destination location. The schedule should be specified in UTC time.
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/task-scheduling.html Scheduling your task>.
--
-- 'tags', 'createTask_tags' - Specifies the tags that you want to apply to the Amazon Resource Name
-- (ARN) representing the task.
--
-- /Tags/ are key-value pairs that help you manage, filter, and search for
-- your DataSync resources.
--
-- 'sourceLocationArn', 'createTask_sourceLocationArn' - The Amazon Resource Name (ARN) of the source location for the task.
--
-- 'destinationLocationArn', 'createTask_destinationLocationArn' - The Amazon Resource Name (ARN) of an Amazon Web Services storage
-- resource\'s location.
newCreateTask ::
  -- | 'sourceLocationArn'
  Prelude.Text ->
  -- | 'destinationLocationArn'
  Prelude.Text ->
  CreateTask
newCreateTask
  pSourceLocationArn_
  pDestinationLocationArn_ =
    CreateTask'
      { cloudWatchLogGroupArn =
          Prelude.Nothing,
        excludes = Prelude.Nothing,
        includes = Prelude.Nothing,
        name = Prelude.Nothing,
        options = Prelude.Nothing,
        schedule = Prelude.Nothing,
        tags = Prelude.Nothing,
        sourceLocationArn = pSourceLocationArn_,
        destinationLocationArn = pDestinationLocationArn_
      }

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- is used to monitor and log events in the task.
createTask_cloudWatchLogGroupArn :: Lens.Lens' CreateTask (Prelude.Maybe Prelude.Text)
createTask_cloudWatchLogGroupArn = Lens.lens (\CreateTask' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@CreateTask' {} a -> s {cloudWatchLogGroupArn = a} :: CreateTask)

-- | Specifies a list of filter rules that exclude specific data during your
-- transfer. For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
createTask_excludes :: Lens.Lens' CreateTask (Prelude.Maybe [FilterRule])
createTask_excludes = Lens.lens (\CreateTask' {excludes} -> excludes) (\s@CreateTask' {} a -> s {excludes = a} :: CreateTask) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a list of filter rules that include specific data during your
-- transfer. For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
createTask_includes :: Lens.Lens' CreateTask (Prelude.Maybe [FilterRule])
createTask_includes = Lens.lens (\CreateTask' {includes} -> includes) (\s@CreateTask' {} a -> s {includes = a} :: CreateTask) Prelude.. Lens.mapping Lens.coerced

-- | The name of a task. This value is a text reference that is used to
-- identify the task in the console.
createTask_name :: Lens.Lens' CreateTask (Prelude.Maybe Prelude.Text)
createTask_name = Lens.lens (\CreateTask' {name} -> name) (\s@CreateTask' {} a -> s {name = a} :: CreateTask)

-- | Specifies the configuration options for a task. Some options include
-- preserving file or object metadata and verifying data integrity.
--
-- You can also override these options before starting an individual run of
-- a task (also known as a /task execution/). For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>.
createTask_options :: Lens.Lens' CreateTask (Prelude.Maybe Options)
createTask_options = Lens.lens (\CreateTask' {options} -> options) (\s@CreateTask' {} a -> s {options = a} :: CreateTask)

-- | Specifies a schedule used to periodically transfer files from a source
-- to a destination location. The schedule should be specified in UTC time.
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/task-scheduling.html Scheduling your task>.
createTask_schedule :: Lens.Lens' CreateTask (Prelude.Maybe TaskSchedule)
createTask_schedule = Lens.lens (\CreateTask' {schedule} -> schedule) (\s@CreateTask' {} a -> s {schedule = a} :: CreateTask)

-- | Specifies the tags that you want to apply to the Amazon Resource Name
-- (ARN) representing the task.
--
-- /Tags/ are key-value pairs that help you manage, filter, and search for
-- your DataSync resources.
createTask_tags :: Lens.Lens' CreateTask (Prelude.Maybe [TagListEntry])
createTask_tags = Lens.lens (\CreateTask' {tags} -> tags) (\s@CreateTask' {} a -> s {tags = a} :: CreateTask) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the source location for the task.
createTask_sourceLocationArn :: Lens.Lens' CreateTask Prelude.Text
createTask_sourceLocationArn = Lens.lens (\CreateTask' {sourceLocationArn} -> sourceLocationArn) (\s@CreateTask' {} a -> s {sourceLocationArn = a} :: CreateTask)

-- | The Amazon Resource Name (ARN) of an Amazon Web Services storage
-- resource\'s location.
createTask_destinationLocationArn :: Lens.Lens' CreateTask Prelude.Text
createTask_destinationLocationArn = Lens.lens (\CreateTask' {destinationLocationArn} -> destinationLocationArn) (\s@CreateTask' {} a -> s {destinationLocationArn = a} :: CreateTask)

instance Core.AWSRequest CreateTask where
  type AWSResponse CreateTask = CreateTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTaskResponse'
            Prelude.<$> (x Data..?> "TaskArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTask where
  hashWithSalt _salt CreateTask' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogGroupArn
      `Prelude.hashWithSalt` excludes
      `Prelude.hashWithSalt` includes
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceLocationArn
      `Prelude.hashWithSalt` destinationLocationArn

instance Prelude.NFData CreateTask where
  rnf CreateTask' {..} =
    Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf excludes
      `Prelude.seq` Prelude.rnf includes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceLocationArn
      `Prelude.seq` Prelude.rnf destinationLocationArn

instance Data.ToHeaders CreateTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("FmrsService.CreateTask" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTask where
  toJSON CreateTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogGroupArn" Data..=)
              Prelude.<$> cloudWatchLogGroupArn,
            ("Excludes" Data..=) Prelude.<$> excludes,
            ("Includes" Data..=) Prelude.<$> includes,
            ("Name" Data..=) Prelude.<$> name,
            ("Options" Data..=) Prelude.<$> options,
            ("Schedule" Data..=) Prelude.<$> schedule,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("SourceLocationArn" Data..= sourceLocationArn),
            Prelude.Just
              ( "DestinationLocationArn"
                  Data..= destinationLocationArn
              )
          ]
      )

instance Data.ToPath CreateTask where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTask where
  toQuery = Prelude.const Prelude.mempty

-- | CreateTaskResponse
--
-- /See:/ 'newCreateTaskResponse' smart constructor.
data CreateTaskResponse = CreateTaskResponse'
  { -- | The Amazon Resource Name (ARN) of the task.
    taskArn :: Prelude.Maybe Prelude.Text,
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
-- 'taskArn', 'createTaskResponse_taskArn' - The Amazon Resource Name (ARN) of the task.
--
-- 'httpStatus', 'createTaskResponse_httpStatus' - The response's http status code.
newCreateTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTaskResponse
newCreateTaskResponse pHttpStatus_ =
  CreateTaskResponse'
    { taskArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the task.
createTaskResponse_taskArn :: Lens.Lens' CreateTaskResponse (Prelude.Maybe Prelude.Text)
createTaskResponse_taskArn = Lens.lens (\CreateTaskResponse' {taskArn} -> taskArn) (\s@CreateTaskResponse' {} a -> s {taskArn = a} :: CreateTaskResponse)

-- | The response's http status code.
createTaskResponse_httpStatus :: Lens.Lens' CreateTaskResponse Prelude.Int
createTaskResponse_httpStatus = Lens.lens (\CreateTaskResponse' {httpStatus} -> httpStatus) (\s@CreateTaskResponse' {} a -> s {httpStatus = a} :: CreateTaskResponse)

instance Prelude.NFData CreateTaskResponse where
  rnf CreateTaskResponse' {..} =
    Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf httpStatus
