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
-- Module      : Amazonka.DataSync.DescribeTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about a task.
module Amazonka.DataSync.DescribeTask
  ( -- * Creating a Request
    DescribeTask (..),
    newDescribeTask,

    -- * Request Lenses
    describeTask_taskArn,

    -- * Destructuring the Response
    DescribeTaskResponse (..),
    newDescribeTaskResponse,

    -- * Response Lenses
    describeTaskResponse_cloudWatchLogGroupArn,
    describeTaskResponse_creationTime,
    describeTaskResponse_currentTaskExecutionArn,
    describeTaskResponse_destinationLocationArn,
    describeTaskResponse_destinationNetworkInterfaceArns,
    describeTaskResponse_errorCode,
    describeTaskResponse_errorDetail,
    describeTaskResponse_excludes,
    describeTaskResponse_includes,
    describeTaskResponse_name,
    describeTaskResponse_options,
    describeTaskResponse_schedule,
    describeTaskResponse_sourceLocationArn,
    describeTaskResponse_sourceNetworkInterfaceArns,
    describeTaskResponse_status,
    describeTaskResponse_taskArn,
    describeTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeTaskRequest
--
-- /See:/ 'newDescribeTask' smart constructor.
data DescribeTask = DescribeTask'
  { -- | The Amazon Resource Name (ARN) of the task to describe.
    taskArn :: Prelude.Text
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
-- 'taskArn', 'describeTask_taskArn' - The Amazon Resource Name (ARN) of the task to describe.
newDescribeTask ::
  -- | 'taskArn'
  Prelude.Text ->
  DescribeTask
newDescribeTask pTaskArn_ =
  DescribeTask' {taskArn = pTaskArn_}

-- | The Amazon Resource Name (ARN) of the task to describe.
describeTask_taskArn :: Lens.Lens' DescribeTask Prelude.Text
describeTask_taskArn = Lens.lens (\DescribeTask' {taskArn} -> taskArn) (\s@DescribeTask' {} a -> s {taskArn = a} :: DescribeTask)

instance Core.AWSRequest DescribeTask where
  type AWSResponse DescribeTask = DescribeTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTaskResponse'
            Prelude.<$> (x Data..?> "CloudWatchLogGroupArn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "CurrentTaskExecutionArn")
            Prelude.<*> (x Data..?> "DestinationLocationArn")
            Prelude.<*> ( x
                            Data..?> "DestinationNetworkInterfaceArns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ErrorCode")
            Prelude.<*> (x Data..?> "ErrorDetail")
            Prelude.<*> (x Data..?> "Excludes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Includes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Options")
            Prelude.<*> (x Data..?> "Schedule")
            Prelude.<*> (x Data..?> "SourceLocationArn")
            Prelude.<*> ( x
                            Data..?> "SourceNetworkInterfaceArns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TaskArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTask where
  hashWithSalt _salt DescribeTask' {..} =
    _salt `Prelude.hashWithSalt` taskArn

instance Prelude.NFData DescribeTask where
  rnf DescribeTask' {..} = Prelude.rnf taskArn

instance Data.ToHeaders DescribeTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("FmrsService.DescribeTask" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTask where
  toJSON DescribeTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TaskArn" Data..= taskArn)]
      )

instance Data.ToPath DescribeTask where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTask where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeTaskResponse
--
-- /See:/ 'newDescribeTaskResponse' smart constructor.
data DescribeTaskResponse = DescribeTaskResponse'
  { -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
    -- was used to monitor and log events in the task.
    --
    -- For more information on these groups, see Working with Log Groups and
    -- Log Streams in the /Amazon CloudWatch User Guide/.
    cloudWatchLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the task was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the task execution that is
    -- transferring files.
    currentTaskExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services storage
    -- resource\'s location.
    destinationLocationArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the network interfaces created for
    -- your destination location. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/datasync-network.html#required-network-interfaces Network interface requirements>.
    destinationNetworkInterfaceArns :: Prelude.Maybe [Prelude.Text],
    -- | Errors that DataSync encountered during execution of the task. You can
    -- use this error code to help troubleshoot issues.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | Detailed description of an error that was encountered during the task
    -- execution. You can use this information to help troubleshoot issues.
    errorDetail :: Prelude.Maybe Prelude.Text,
    -- | A list of filter rules that exclude specific data during your transfer.
    -- For more information and examples, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
    excludes :: Prelude.Maybe [FilterRule],
    -- | A list of filter rules that include specific data during your transfer.
    -- For more information and examples, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
    includes :: Prelude.Maybe [FilterRule],
    -- | The name of the task that was described.
    name :: Prelude.Maybe Prelude.Text,
    -- | The configuration options that control the behavior of the
    -- @StartTaskExecution@ operation. Some options include preserving file or
    -- object metadata and verifying data integrity.
    --
    -- You can override these options for each task execution. For more
    -- information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>.
    options :: Prelude.Maybe Options,
    -- | The schedule used to periodically transfer files from a source to a
    -- destination location.
    schedule :: Prelude.Maybe TaskSchedule,
    -- | The Amazon Resource Name (ARN) of the source file system\'s location.
    sourceLocationArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the network interfaces created for
    -- your source location. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/datasync-network.html#required-network-interfaces Network interface requirements>.
    sourceNetworkInterfaceArns :: Prelude.Maybe [Prelude.Text],
    -- | The status of the task that was described.
    --
    -- For detailed information about task execution statuses, see
    -- Understanding Task Statuses in the /DataSync User Guide/.
    status :: Prelude.Maybe TaskStatus,
    -- | The Amazon Resource Name (ARN) of the task that was described.
    taskArn :: Prelude.Maybe Prelude.Text,
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
-- 'cloudWatchLogGroupArn', 'describeTaskResponse_cloudWatchLogGroupArn' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- was used to monitor and log events in the task.
--
-- For more information on these groups, see Working with Log Groups and
-- Log Streams in the /Amazon CloudWatch User Guide/.
--
-- 'creationTime', 'describeTaskResponse_creationTime' - The time that the task was created.
--
-- 'currentTaskExecutionArn', 'describeTaskResponse_currentTaskExecutionArn' - The Amazon Resource Name (ARN) of the task execution that is
-- transferring files.
--
-- 'destinationLocationArn', 'describeTaskResponse_destinationLocationArn' - The Amazon Resource Name (ARN) of the Amazon Web Services storage
-- resource\'s location.
--
-- 'destinationNetworkInterfaceArns', 'describeTaskResponse_destinationNetworkInterfaceArns' - The Amazon Resource Names (ARNs) of the network interfaces created for
-- your destination location. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/datasync-network.html#required-network-interfaces Network interface requirements>.
--
-- 'errorCode', 'describeTaskResponse_errorCode' - Errors that DataSync encountered during execution of the task. You can
-- use this error code to help troubleshoot issues.
--
-- 'errorDetail', 'describeTaskResponse_errorDetail' - Detailed description of an error that was encountered during the task
-- execution. You can use this information to help troubleshoot issues.
--
-- 'excludes', 'describeTaskResponse_excludes' - A list of filter rules that exclude specific data during your transfer.
-- For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
--
-- 'includes', 'describeTaskResponse_includes' - A list of filter rules that include specific data during your transfer.
-- For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
--
-- 'name', 'describeTaskResponse_name' - The name of the task that was described.
--
-- 'options', 'describeTaskResponse_options' - The configuration options that control the behavior of the
-- @StartTaskExecution@ operation. Some options include preserving file or
-- object metadata and verifying data integrity.
--
-- You can override these options for each task execution. For more
-- information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>.
--
-- 'schedule', 'describeTaskResponse_schedule' - The schedule used to periodically transfer files from a source to a
-- destination location.
--
-- 'sourceLocationArn', 'describeTaskResponse_sourceLocationArn' - The Amazon Resource Name (ARN) of the source file system\'s location.
--
-- 'sourceNetworkInterfaceArns', 'describeTaskResponse_sourceNetworkInterfaceArns' - The Amazon Resource Names (ARNs) of the network interfaces created for
-- your source location. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/datasync-network.html#required-network-interfaces Network interface requirements>.
--
-- 'status', 'describeTaskResponse_status' - The status of the task that was described.
--
-- For detailed information about task execution statuses, see
-- Understanding Task Statuses in the /DataSync User Guide/.
--
-- 'taskArn', 'describeTaskResponse_taskArn' - The Amazon Resource Name (ARN) of the task that was described.
--
-- 'httpStatus', 'describeTaskResponse_httpStatus' - The response's http status code.
newDescribeTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTaskResponse
newDescribeTaskResponse pHttpStatus_ =
  DescribeTaskResponse'
    { cloudWatchLogGroupArn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currentTaskExecutionArn = Prelude.Nothing,
      destinationLocationArn = Prelude.Nothing,
      destinationNetworkInterfaceArns = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorDetail = Prelude.Nothing,
      excludes = Prelude.Nothing,
      includes = Prelude.Nothing,
      name = Prelude.Nothing,
      options = Prelude.Nothing,
      schedule = Prelude.Nothing,
      sourceLocationArn = Prelude.Nothing,
      sourceNetworkInterfaceArns = Prelude.Nothing,
      status = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- was used to monitor and log events in the task.
--
-- For more information on these groups, see Working with Log Groups and
-- Log Streams in the /Amazon CloudWatch User Guide/.
describeTaskResponse_cloudWatchLogGroupArn :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_cloudWatchLogGroupArn = Lens.lens (\DescribeTaskResponse' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@DescribeTaskResponse' {} a -> s {cloudWatchLogGroupArn = a} :: DescribeTaskResponse)

-- | The time that the task was created.
describeTaskResponse_creationTime :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeTaskResponse_creationTime = Lens.lens (\DescribeTaskResponse' {creationTime} -> creationTime) (\s@DescribeTaskResponse' {} a -> s {creationTime = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the task execution that is
-- transferring files.
describeTaskResponse_currentTaskExecutionArn :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_currentTaskExecutionArn = Lens.lens (\DescribeTaskResponse' {currentTaskExecutionArn} -> currentTaskExecutionArn) (\s@DescribeTaskResponse' {} a -> s {currentTaskExecutionArn = a} :: DescribeTaskResponse)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services storage
-- resource\'s location.
describeTaskResponse_destinationLocationArn :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_destinationLocationArn = Lens.lens (\DescribeTaskResponse' {destinationLocationArn} -> destinationLocationArn) (\s@DescribeTaskResponse' {} a -> s {destinationLocationArn = a} :: DescribeTaskResponse)

-- | The Amazon Resource Names (ARNs) of the network interfaces created for
-- your destination location. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/datasync-network.html#required-network-interfaces Network interface requirements>.
describeTaskResponse_destinationNetworkInterfaceArns :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe [Prelude.Text])
describeTaskResponse_destinationNetworkInterfaceArns = Lens.lens (\DescribeTaskResponse' {destinationNetworkInterfaceArns} -> destinationNetworkInterfaceArns) (\s@DescribeTaskResponse' {} a -> s {destinationNetworkInterfaceArns = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | Errors that DataSync encountered during execution of the task. You can
-- use this error code to help troubleshoot issues.
describeTaskResponse_errorCode :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_errorCode = Lens.lens (\DescribeTaskResponse' {errorCode} -> errorCode) (\s@DescribeTaskResponse' {} a -> s {errorCode = a} :: DescribeTaskResponse)

-- | Detailed description of an error that was encountered during the task
-- execution. You can use this information to help troubleshoot issues.
describeTaskResponse_errorDetail :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_errorDetail = Lens.lens (\DescribeTaskResponse' {errorDetail} -> errorDetail) (\s@DescribeTaskResponse' {} a -> s {errorDetail = a} :: DescribeTaskResponse)

-- | A list of filter rules that exclude specific data during your transfer.
-- For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
describeTaskResponse_excludes :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe [FilterRule])
describeTaskResponse_excludes = Lens.lens (\DescribeTaskResponse' {excludes} -> excludes) (\s@DescribeTaskResponse' {} a -> s {excludes = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of filter rules that include specific data during your transfer.
-- For more information and examples, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/filtering.html Filtering data transferred by DataSync>.
describeTaskResponse_includes :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe [FilterRule])
describeTaskResponse_includes = Lens.lens (\DescribeTaskResponse' {includes} -> includes) (\s@DescribeTaskResponse' {} a -> s {includes = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the task that was described.
describeTaskResponse_name :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_name = Lens.lens (\DescribeTaskResponse' {name} -> name) (\s@DescribeTaskResponse' {} a -> s {name = a} :: DescribeTaskResponse)

-- | The configuration options that control the behavior of the
-- @StartTaskExecution@ operation. Some options include preserving file or
-- object metadata and verifying data integrity.
--
-- You can override these options for each task execution. For more
-- information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>.
describeTaskResponse_options :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Options)
describeTaskResponse_options = Lens.lens (\DescribeTaskResponse' {options} -> options) (\s@DescribeTaskResponse' {} a -> s {options = a} :: DescribeTaskResponse)

-- | The schedule used to periodically transfer files from a source to a
-- destination location.
describeTaskResponse_schedule :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe TaskSchedule)
describeTaskResponse_schedule = Lens.lens (\DescribeTaskResponse' {schedule} -> schedule) (\s@DescribeTaskResponse' {} a -> s {schedule = a} :: DescribeTaskResponse)

-- | The Amazon Resource Name (ARN) of the source file system\'s location.
describeTaskResponse_sourceLocationArn :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_sourceLocationArn = Lens.lens (\DescribeTaskResponse' {sourceLocationArn} -> sourceLocationArn) (\s@DescribeTaskResponse' {} a -> s {sourceLocationArn = a} :: DescribeTaskResponse)

-- | The Amazon Resource Names (ARNs) of the network interfaces created for
-- your source location. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/datasync-network.html#required-network-interfaces Network interface requirements>.
describeTaskResponse_sourceNetworkInterfaceArns :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe [Prelude.Text])
describeTaskResponse_sourceNetworkInterfaceArns = Lens.lens (\DescribeTaskResponse' {sourceNetworkInterfaceArns} -> sourceNetworkInterfaceArns) (\s@DescribeTaskResponse' {} a -> s {sourceNetworkInterfaceArns = a} :: DescribeTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the task that was described.
--
-- For detailed information about task execution statuses, see
-- Understanding Task Statuses in the /DataSync User Guide/.
describeTaskResponse_status :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe TaskStatus)
describeTaskResponse_status = Lens.lens (\DescribeTaskResponse' {status} -> status) (\s@DescribeTaskResponse' {} a -> s {status = a} :: DescribeTaskResponse)

-- | The Amazon Resource Name (ARN) of the task that was described.
describeTaskResponse_taskArn :: Lens.Lens' DescribeTaskResponse (Prelude.Maybe Prelude.Text)
describeTaskResponse_taskArn = Lens.lens (\DescribeTaskResponse' {taskArn} -> taskArn) (\s@DescribeTaskResponse' {} a -> s {taskArn = a} :: DescribeTaskResponse)

-- | The response's http status code.
describeTaskResponse_httpStatus :: Lens.Lens' DescribeTaskResponse Prelude.Int
describeTaskResponse_httpStatus = Lens.lens (\DescribeTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeTaskResponse' {} a -> s {httpStatus = a} :: DescribeTaskResponse)

instance Prelude.NFData DescribeTaskResponse where
  rnf DescribeTaskResponse' {..} =
    Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf currentTaskExecutionArn
      `Prelude.seq` Prelude.rnf destinationLocationArn
      `Prelude.seq` Prelude.rnf destinationNetworkInterfaceArns
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorDetail
      `Prelude.seq` Prelude.rnf excludes
      `Prelude.seq` Prelude.rnf includes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf sourceLocationArn
      `Prelude.seq` Prelude.rnf sourceNetworkInterfaceArns
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf httpStatus
