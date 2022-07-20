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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a task.
--
-- A task includes a source location and a destination location, and a
-- configuration that specifies how data is transferred. A task always
-- transfers data from the source location to the destination location. The
-- configuration specifies options such as task scheduling, bandwidth
-- limits, etc. A task is the complete definition of a data transfer.
--
-- When you create a task that transfers data between Amazon Web Services
-- services in different Amazon Web Services Regions, one of the two
-- locations that you specify must reside in the Region where DataSync is
-- being used. The other location must be specified in a different Region.
--
-- You can transfer data between commercial Amazon Web Services Regions
-- except for China, or between Amazon Web Services GovCloud (US) Regions.
--
-- When you use DataSync to copy files or objects between Amazon Web
-- Services Regions, you pay for data transfer between Regions. This is
-- billed as data transfer OUT from your source Region to your destination
-- Region. For more information, see
-- <http://aws.amazon.com/ec2/pricing/on-demand/#Data_Transfer Data Transfer pricing>.
module Amazonka.DataSync.CreateTask
  ( -- * Creating a Request
    CreateTask (..),
    newCreateTask,

    -- * Request Lenses
    createTask_tags,
    createTask_schedule,
    createTask_name,
    createTask_cloudWatchLogGroupArn,
    createTask_excludes,
    createTask_options,
    createTask_includes,
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
import Amazonka.DataSync.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | CreateTaskRequest
--
-- /See:/ 'newCreateTask' smart constructor.
data CreateTask = CreateTask'
  { -- | The key-value pair that represents the tag that you want to add to the
    -- resource. The value can be an empty string.
    tags :: Prelude.Maybe [TagListEntry],
    -- | Specifies a schedule used to periodically transfer files from a source
    -- to a destination location. The schedule should be specified in UTC time.
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/task-scheduling.html Scheduling your task>.
    schedule :: Prelude.Maybe TaskSchedule,
    -- | The name of a task. This value is a text reference that is used to
    -- identify the task in the console.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
    -- is used to monitor and log events in the task.
    cloudWatchLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | A list of filter rules that determines which files to exclude from a
    -- task. The list should contain a single filter string that consists of
    -- the patterns to exclude. The patterns are delimited by \"|\" (that is, a
    -- pipe), for example, @\"\/folder1|\/folder2\"@.
    excludes :: Prelude.Maybe [FilterRule],
    -- | The set of configuration options that control the behavior of a single
    -- execution of the task that occurs when you call @StartTaskExecution@.
    -- You can configure these options to preserve metadata such as user ID
    -- (UID) and group ID (GID), file permissions, data integrity verification,
    -- and so on.
    --
    -- For each individual task execution, you can override these options by
    -- specifying the @OverrideOptions@ before starting the task execution. For
    -- more information, see the
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>
    -- operation.
    options :: Prelude.Maybe Options,
    -- | A list of filter rules that determines which files to include when
    -- running a task. The pattern should contain a single filter string that
    -- consists of the patterns to include. The patterns are delimited by \"|\"
    -- (that is, a pipe). For example: @\"\/folder1|\/folder2@\"
    includes :: Prelude.Maybe [FilterRule],
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
-- 'tags', 'createTask_tags' - The key-value pair that represents the tag that you want to add to the
-- resource. The value can be an empty string.
--
-- 'schedule', 'createTask_schedule' - Specifies a schedule used to periodically transfer files from a source
-- to a destination location. The schedule should be specified in UTC time.
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/task-scheduling.html Scheduling your task>.
--
-- 'name', 'createTask_name' - The name of a task. This value is a text reference that is used to
-- identify the task in the console.
--
-- 'cloudWatchLogGroupArn', 'createTask_cloudWatchLogGroupArn' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- is used to monitor and log events in the task.
--
-- 'excludes', 'createTask_excludes' - A list of filter rules that determines which files to exclude from a
-- task. The list should contain a single filter string that consists of
-- the patterns to exclude. The patterns are delimited by \"|\" (that is, a
-- pipe), for example, @\"\/folder1|\/folder2\"@.
--
-- 'options', 'createTask_options' - The set of configuration options that control the behavior of a single
-- execution of the task that occurs when you call @StartTaskExecution@.
-- You can configure these options to preserve metadata such as user ID
-- (UID) and group ID (GID), file permissions, data integrity verification,
-- and so on.
--
-- For each individual task execution, you can override these options by
-- specifying the @OverrideOptions@ before starting the task execution. For
-- more information, see the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>
-- operation.
--
-- 'includes', 'createTask_includes' - A list of filter rules that determines which files to include when
-- running a task. The pattern should contain a single filter string that
-- consists of the patterns to include. The patterns are delimited by \"|\"
-- (that is, a pipe). For example: @\"\/folder1|\/folder2@\"
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
      { tags = Prelude.Nothing,
        schedule = Prelude.Nothing,
        name = Prelude.Nothing,
        cloudWatchLogGroupArn = Prelude.Nothing,
        excludes = Prelude.Nothing,
        options = Prelude.Nothing,
        includes = Prelude.Nothing,
        sourceLocationArn = pSourceLocationArn_,
        destinationLocationArn = pDestinationLocationArn_
      }

-- | The key-value pair that represents the tag that you want to add to the
-- resource. The value can be an empty string.
createTask_tags :: Lens.Lens' CreateTask (Prelude.Maybe [TagListEntry])
createTask_tags = Lens.lens (\CreateTask' {tags} -> tags) (\s@CreateTask' {} a -> s {tags = a} :: CreateTask) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a schedule used to periodically transfer files from a source
-- to a destination location. The schedule should be specified in UTC time.
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/task-scheduling.html Scheduling your task>.
createTask_schedule :: Lens.Lens' CreateTask (Prelude.Maybe TaskSchedule)
createTask_schedule = Lens.lens (\CreateTask' {schedule} -> schedule) (\s@CreateTask' {} a -> s {schedule = a} :: CreateTask)

-- | The name of a task. This value is a text reference that is used to
-- identify the task in the console.
createTask_name :: Lens.Lens' CreateTask (Prelude.Maybe Prelude.Text)
createTask_name = Lens.lens (\CreateTask' {name} -> name) (\s@CreateTask' {} a -> s {name = a} :: CreateTask)

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- is used to monitor and log events in the task.
createTask_cloudWatchLogGroupArn :: Lens.Lens' CreateTask (Prelude.Maybe Prelude.Text)
createTask_cloudWatchLogGroupArn = Lens.lens (\CreateTask' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@CreateTask' {} a -> s {cloudWatchLogGroupArn = a} :: CreateTask)

-- | A list of filter rules that determines which files to exclude from a
-- task. The list should contain a single filter string that consists of
-- the patterns to exclude. The patterns are delimited by \"|\" (that is, a
-- pipe), for example, @\"\/folder1|\/folder2\"@.
createTask_excludes :: Lens.Lens' CreateTask (Prelude.Maybe [FilterRule])
createTask_excludes = Lens.lens (\CreateTask' {excludes} -> excludes) (\s@CreateTask' {} a -> s {excludes = a} :: CreateTask) Prelude.. Lens.mapping Lens.coerced

-- | The set of configuration options that control the behavior of a single
-- execution of the task that occurs when you call @StartTaskExecution@.
-- You can configure these options to preserve metadata such as user ID
-- (UID) and group ID (GID), file permissions, data integrity verification,
-- and so on.
--
-- For each individual task execution, you can override these options by
-- specifying the @OverrideOptions@ before starting the task execution. For
-- more information, see the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>
-- operation.
createTask_options :: Lens.Lens' CreateTask (Prelude.Maybe Options)
createTask_options = Lens.lens (\CreateTask' {options} -> options) (\s@CreateTask' {} a -> s {options = a} :: CreateTask)

-- | A list of filter rules that determines which files to include when
-- running a task. The pattern should contain a single filter string that
-- consists of the patterns to include. The patterns are delimited by \"|\"
-- (that is, a pipe). For example: @\"\/folder1|\/folder2@\"
createTask_includes :: Lens.Lens' CreateTask (Prelude.Maybe [FilterRule])
createTask_includes = Lens.lens (\CreateTask' {includes} -> includes) (\s@CreateTask' {} a -> s {includes = a} :: CreateTask) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the source location for the task.
createTask_sourceLocationArn :: Lens.Lens' CreateTask Prelude.Text
createTask_sourceLocationArn = Lens.lens (\CreateTask' {sourceLocationArn} -> sourceLocationArn) (\s@CreateTask' {} a -> s {sourceLocationArn = a} :: CreateTask)

-- | The Amazon Resource Name (ARN) of an Amazon Web Services storage
-- resource\'s location.
createTask_destinationLocationArn :: Lens.Lens' CreateTask Prelude.Text
createTask_destinationLocationArn = Lens.lens (\CreateTask' {destinationLocationArn} -> destinationLocationArn) (\s@CreateTask' {} a -> s {destinationLocationArn = a} :: CreateTask)

instance Core.AWSRequest CreateTask where
  type AWSResponse CreateTask = CreateTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTaskResponse'
            Prelude.<$> (x Core..?> "TaskArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTask where
  hashWithSalt _salt CreateTask' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` cloudWatchLogGroupArn
      `Prelude.hashWithSalt` excludes
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` includes
      `Prelude.hashWithSalt` sourceLocationArn
      `Prelude.hashWithSalt` destinationLocationArn

instance Prelude.NFData CreateTask where
  rnf CreateTask' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf excludes
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf includes
      `Prelude.seq` Prelude.rnf sourceLocationArn
      `Prelude.seq` Prelude.rnf destinationLocationArn

instance Core.ToHeaders CreateTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("FmrsService.CreateTask" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateTask where
  toJSON CreateTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Schedule" Core..=) Prelude.<$> schedule,
            ("Name" Core..=) Prelude.<$> name,
            ("CloudWatchLogGroupArn" Core..=)
              Prelude.<$> cloudWatchLogGroupArn,
            ("Excludes" Core..=) Prelude.<$> excludes,
            ("Options" Core..=) Prelude.<$> options,
            ("Includes" Core..=) Prelude.<$> includes,
            Prelude.Just
              ("SourceLocationArn" Core..= sourceLocationArn),
            Prelude.Just
              ( "DestinationLocationArn"
                  Core..= destinationLocationArn
              )
          ]
      )

instance Core.ToPath CreateTask where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTask where
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
