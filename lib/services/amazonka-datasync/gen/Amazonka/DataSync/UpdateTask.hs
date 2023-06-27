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
-- Module      : Amazonka.DataSync.UpdateTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the metadata associated with a task.
module Amazonka.DataSync.UpdateTask
  ( -- * Creating a Request
    UpdateTask (..),
    newUpdateTask,

    -- * Request Lenses
    updateTask_cloudWatchLogGroupArn,
    updateTask_excludes,
    updateTask_includes,
    updateTask_name,
    updateTask_options,
    updateTask_schedule,
    updateTask_taskArn,

    -- * Destructuring the Response
    UpdateTaskResponse (..),
    newUpdateTaskResponse,

    -- * Response Lenses
    updateTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | UpdateTaskResponse
--
-- /See:/ 'newUpdateTask' smart constructor.
data UpdateTask = UpdateTask'
  { -- | The Amazon Resource Name (ARN) of the resource name of the Amazon
    -- CloudWatch log group.
    cloudWatchLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies a list of filter rules that exclude specific data during your
    -- transfer. For more information and examples, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
    excludes :: Prelude.Maybe [FilterRule],
    -- | Specifies a list of filter rules that include specific data during your
    -- transfer. For more information and examples, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
    includes :: Prelude.Maybe [FilterRule],
    -- | The name of the task to update.
    name :: Prelude.Maybe Prelude.Text,
    options :: Prelude.Maybe Options,
    -- | Specifies a schedule used to periodically transfer files from a source
    -- to a destination location. You can configure your task to execute
    -- hourly, daily, weekly or on specific days of the week. You control when
    -- in the day or hour you want the task to execute. The time you specify is
    -- UTC time. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/task-scheduling.html Scheduling your task>.
    schedule :: Prelude.Maybe TaskSchedule,
    -- | The Amazon Resource Name (ARN) of the resource name of the task to
    -- update.
    taskArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogGroupArn', 'updateTask_cloudWatchLogGroupArn' - The Amazon Resource Name (ARN) of the resource name of the Amazon
-- CloudWatch log group.
--
-- 'excludes', 'updateTask_excludes' - Specifies a list of filter rules that exclude specific data during your
-- transfer. For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
--
-- 'includes', 'updateTask_includes' - Specifies a list of filter rules that include specific data during your
-- transfer. For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
--
-- 'name', 'updateTask_name' - The name of the task to update.
--
-- 'options', 'updateTask_options' - Undocumented member.
--
-- 'schedule', 'updateTask_schedule' - Specifies a schedule used to periodically transfer files from a source
-- to a destination location. You can configure your task to execute
-- hourly, daily, weekly or on specific days of the week. You control when
-- in the day or hour you want the task to execute. The time you specify is
-- UTC time. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/task-scheduling.html Scheduling your task>.
--
-- 'taskArn', 'updateTask_taskArn' - The Amazon Resource Name (ARN) of the resource name of the task to
-- update.
newUpdateTask ::
  -- | 'taskArn'
  Prelude.Text ->
  UpdateTask
newUpdateTask pTaskArn_ =
  UpdateTask'
    { cloudWatchLogGroupArn =
        Prelude.Nothing,
      excludes = Prelude.Nothing,
      includes = Prelude.Nothing,
      name = Prelude.Nothing,
      options = Prelude.Nothing,
      schedule = Prelude.Nothing,
      taskArn = pTaskArn_
    }

-- | The Amazon Resource Name (ARN) of the resource name of the Amazon
-- CloudWatch log group.
updateTask_cloudWatchLogGroupArn :: Lens.Lens' UpdateTask (Prelude.Maybe Prelude.Text)
updateTask_cloudWatchLogGroupArn = Lens.lens (\UpdateTask' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@UpdateTask' {} a -> s {cloudWatchLogGroupArn = a} :: UpdateTask)

-- | Specifies a list of filter rules that exclude specific data during your
-- transfer. For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
updateTask_excludes :: Lens.Lens' UpdateTask (Prelude.Maybe [FilterRule])
updateTask_excludes = Lens.lens (\UpdateTask' {excludes} -> excludes) (\s@UpdateTask' {} a -> s {excludes = a} :: UpdateTask) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a list of filter rules that include specific data during your
-- transfer. For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
updateTask_includes :: Lens.Lens' UpdateTask (Prelude.Maybe [FilterRule])
updateTask_includes = Lens.lens (\UpdateTask' {includes} -> includes) (\s@UpdateTask' {} a -> s {includes = a} :: UpdateTask) Prelude.. Lens.mapping Lens.coerced

-- | The name of the task to update.
updateTask_name :: Lens.Lens' UpdateTask (Prelude.Maybe Prelude.Text)
updateTask_name = Lens.lens (\UpdateTask' {name} -> name) (\s@UpdateTask' {} a -> s {name = a} :: UpdateTask)

-- | Undocumented member.
updateTask_options :: Lens.Lens' UpdateTask (Prelude.Maybe Options)
updateTask_options = Lens.lens (\UpdateTask' {options} -> options) (\s@UpdateTask' {} a -> s {options = a} :: UpdateTask)

-- | Specifies a schedule used to periodically transfer files from a source
-- to a destination location. You can configure your task to execute
-- hourly, daily, weekly or on specific days of the week. You control when
-- in the day or hour you want the task to execute. The time you specify is
-- UTC time. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/task-scheduling.html Scheduling your task>.
updateTask_schedule :: Lens.Lens' UpdateTask (Prelude.Maybe TaskSchedule)
updateTask_schedule = Lens.lens (\UpdateTask' {schedule} -> schedule) (\s@UpdateTask' {} a -> s {schedule = a} :: UpdateTask)

-- | The Amazon Resource Name (ARN) of the resource name of the task to
-- update.
updateTask_taskArn :: Lens.Lens' UpdateTask Prelude.Text
updateTask_taskArn = Lens.lens (\UpdateTask' {taskArn} -> taskArn) (\s@UpdateTask' {} a -> s {taskArn = a} :: UpdateTask)

instance Core.AWSRequest UpdateTask where
  type AWSResponse UpdateTask = UpdateTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTask where
  hashWithSalt _salt UpdateTask' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogGroupArn
      `Prelude.hashWithSalt` excludes
      `Prelude.hashWithSalt` includes
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` taskArn

instance Prelude.NFData UpdateTask where
  rnf UpdateTask' {..} =
    Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf excludes
      `Prelude.seq` Prelude.rnf includes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf taskArn

instance Data.ToHeaders UpdateTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("FmrsService.UpdateTask" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTask where
  toJSON UpdateTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogGroupArn" Data..=)
              Prelude.<$> cloudWatchLogGroupArn,
            ("Excludes" Data..=) Prelude.<$> excludes,
            ("Includes" Data..=) Prelude.<$> includes,
            ("Name" Data..=) Prelude.<$> name,
            ("Options" Data..=) Prelude.<$> options,
            ("Schedule" Data..=) Prelude.<$> schedule,
            Prelude.Just ("TaskArn" Data..= taskArn)
          ]
      )

instance Data.ToPath UpdateTask where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTaskResponse' smart constructor.
data UpdateTaskResponse = UpdateTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTaskResponse_httpStatus' - The response's http status code.
newUpdateTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTaskResponse
newUpdateTaskResponse pHttpStatus_ =
  UpdateTaskResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateTaskResponse_httpStatus :: Lens.Lens' UpdateTaskResponse Prelude.Int
updateTaskResponse_httpStatus = Lens.lens (\UpdateTaskResponse' {httpStatus} -> httpStatus) (\s@UpdateTaskResponse' {} a -> s {httpStatus = a} :: UpdateTaskResponse)

instance Prelude.NFData UpdateTaskResponse where
  rnf UpdateTaskResponse' {..} = Prelude.rnf httpStatus
