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
-- Module      : Amazonka.Braket.GetQuantumTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified quantum task.
module Amazonka.Braket.GetQuantumTask
  ( -- * Creating a Request
    GetQuantumTask (..),
    newGetQuantumTask,

    -- * Request Lenses
    getQuantumTask_quantumTaskArn,

    -- * Destructuring the Response
    GetQuantumTaskResponse (..),
    newGetQuantumTaskResponse,

    -- * Response Lenses
    getQuantumTaskResponse_tags,
    getQuantumTaskResponse_endedAt,
    getQuantumTaskResponse_jobArn,
    getQuantumTaskResponse_failureReason,
    getQuantumTaskResponse_httpStatus,
    getQuantumTaskResponse_createdAt,
    getQuantumTaskResponse_deviceArn,
    getQuantumTaskResponse_deviceParameters,
    getQuantumTaskResponse_outputS3Bucket,
    getQuantumTaskResponse_outputS3Directory,
    getQuantumTaskResponse_quantumTaskArn,
    getQuantumTaskResponse_shots,
    getQuantumTaskResponse_status,
  )
where

import Amazonka.Braket.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQuantumTask' smart constructor.
data GetQuantumTask = GetQuantumTask'
  { -- | the ARN of the task to retrieve.
    quantumTaskArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQuantumTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantumTaskArn', 'getQuantumTask_quantumTaskArn' - the ARN of the task to retrieve.
newGetQuantumTask ::
  -- | 'quantumTaskArn'
  Prelude.Text ->
  GetQuantumTask
newGetQuantumTask pQuantumTaskArn_ =
  GetQuantumTask' {quantumTaskArn = pQuantumTaskArn_}

-- | the ARN of the task to retrieve.
getQuantumTask_quantumTaskArn :: Lens.Lens' GetQuantumTask Prelude.Text
getQuantumTask_quantumTaskArn = Lens.lens (\GetQuantumTask' {quantumTaskArn} -> quantumTaskArn) (\s@GetQuantumTask' {} a -> s {quantumTaskArn = a} :: GetQuantumTask)

instance Core.AWSRequest GetQuantumTask where
  type
    AWSResponse GetQuantumTask =
      GetQuantumTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQuantumTaskResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "endedAt")
            Prelude.<*> (x Core..?> "jobArn")
            Prelude.<*> (x Core..?> "failureReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "createdAt")
            Prelude.<*> (x Core..:> "deviceArn")
            Prelude.<*> (x Core..:> "deviceParameters")
            Prelude.<*> (x Core..:> "outputS3Bucket")
            Prelude.<*> (x Core..:> "outputS3Directory")
            Prelude.<*> (x Core..:> "quantumTaskArn")
            Prelude.<*> (x Core..:> "shots")
            Prelude.<*> (x Core..:> "status")
      )

instance Prelude.Hashable GetQuantumTask where
  hashWithSalt _salt GetQuantumTask' {..} =
    _salt `Prelude.hashWithSalt` quantumTaskArn

instance Prelude.NFData GetQuantumTask where
  rnf GetQuantumTask' {..} = Prelude.rnf quantumTaskArn

instance Core.ToHeaders GetQuantumTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetQuantumTask where
  toPath GetQuantumTask' {..} =
    Prelude.mconcat
      ["/quantum-task/", Core.toBS quantumTaskArn]

instance Core.ToQuery GetQuantumTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQuantumTaskResponse' smart constructor.
data GetQuantumTaskResponse = GetQuantumTaskResponse'
  { -- | The tags that belong to this task.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time at which the task ended.
    endedAt :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the Amazon Braket job associated with the quantum task.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The reason that a task failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The time at which the task was created.
    createdAt :: Core.POSIX,
    -- | The ARN of the device the task was run on.
    deviceArn :: Prelude.Text,
    -- | The parameters for the device on which the task ran.
    deviceParameters :: Prelude.Text,
    -- | The S3 bucket where task results are stored.
    outputS3Bucket :: Prelude.Text,
    -- | The folder in the S3 bucket where task results are stored.
    outputS3Directory :: Prelude.Text,
    -- | The ARN of the task.
    quantumTaskArn :: Prelude.Text,
    -- | The number of shots used in the task.
    shots :: Prelude.Integer,
    -- | The status of the task.
    status :: QuantumTaskStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQuantumTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getQuantumTaskResponse_tags' - The tags that belong to this task.
--
-- 'endedAt', 'getQuantumTaskResponse_endedAt' - The time at which the task ended.
--
-- 'jobArn', 'getQuantumTaskResponse_jobArn' - The ARN of the Amazon Braket job associated with the quantum task.
--
-- 'failureReason', 'getQuantumTaskResponse_failureReason' - The reason that a task failed.
--
-- 'httpStatus', 'getQuantumTaskResponse_httpStatus' - The response's http status code.
--
-- 'createdAt', 'getQuantumTaskResponse_createdAt' - The time at which the task was created.
--
-- 'deviceArn', 'getQuantumTaskResponse_deviceArn' - The ARN of the device the task was run on.
--
-- 'deviceParameters', 'getQuantumTaskResponse_deviceParameters' - The parameters for the device on which the task ran.
--
-- 'outputS3Bucket', 'getQuantumTaskResponse_outputS3Bucket' - The S3 bucket where task results are stored.
--
-- 'outputS3Directory', 'getQuantumTaskResponse_outputS3Directory' - The folder in the S3 bucket where task results are stored.
--
-- 'quantumTaskArn', 'getQuantumTaskResponse_quantumTaskArn' - The ARN of the task.
--
-- 'shots', 'getQuantumTaskResponse_shots' - The number of shots used in the task.
--
-- 'status', 'getQuantumTaskResponse_status' - The status of the task.
newGetQuantumTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'deviceArn'
  Prelude.Text ->
  -- | 'deviceParameters'
  Prelude.Text ->
  -- | 'outputS3Bucket'
  Prelude.Text ->
  -- | 'outputS3Directory'
  Prelude.Text ->
  -- | 'quantumTaskArn'
  Prelude.Text ->
  -- | 'shots'
  Prelude.Integer ->
  -- | 'status'
  QuantumTaskStatus ->
  GetQuantumTaskResponse
newGetQuantumTaskResponse
  pHttpStatus_
  pCreatedAt_
  pDeviceArn_
  pDeviceParameters_
  pOutputS3Bucket_
  pOutputS3Directory_
  pQuantumTaskArn_
  pShots_
  pStatus_ =
    GetQuantumTaskResponse'
      { tags = Prelude.Nothing,
        endedAt = Prelude.Nothing,
        jobArn = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        deviceArn = pDeviceArn_,
        deviceParameters = pDeviceParameters_,
        outputS3Bucket = pOutputS3Bucket_,
        outputS3Directory = pOutputS3Directory_,
        quantumTaskArn = pQuantumTaskArn_,
        shots = pShots_,
        status = pStatus_
      }

-- | The tags that belong to this task.
getQuantumTaskResponse_tags :: Lens.Lens' GetQuantumTaskResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getQuantumTaskResponse_tags = Lens.lens (\GetQuantumTaskResponse' {tags} -> tags) (\s@GetQuantumTaskResponse' {} a -> s {tags = a} :: GetQuantumTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the task ended.
getQuantumTaskResponse_endedAt :: Lens.Lens' GetQuantumTaskResponse (Prelude.Maybe Prelude.UTCTime)
getQuantumTaskResponse_endedAt = Lens.lens (\GetQuantumTaskResponse' {endedAt} -> endedAt) (\s@GetQuantumTaskResponse' {} a -> s {endedAt = a} :: GetQuantumTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The ARN of the Amazon Braket job associated with the quantum task.
getQuantumTaskResponse_jobArn :: Lens.Lens' GetQuantumTaskResponse (Prelude.Maybe Prelude.Text)
getQuantumTaskResponse_jobArn = Lens.lens (\GetQuantumTaskResponse' {jobArn} -> jobArn) (\s@GetQuantumTaskResponse' {} a -> s {jobArn = a} :: GetQuantumTaskResponse)

-- | The reason that a task failed.
getQuantumTaskResponse_failureReason :: Lens.Lens' GetQuantumTaskResponse (Prelude.Maybe Prelude.Text)
getQuantumTaskResponse_failureReason = Lens.lens (\GetQuantumTaskResponse' {failureReason} -> failureReason) (\s@GetQuantumTaskResponse' {} a -> s {failureReason = a} :: GetQuantumTaskResponse)

-- | The response's http status code.
getQuantumTaskResponse_httpStatus :: Lens.Lens' GetQuantumTaskResponse Prelude.Int
getQuantumTaskResponse_httpStatus = Lens.lens (\GetQuantumTaskResponse' {httpStatus} -> httpStatus) (\s@GetQuantumTaskResponse' {} a -> s {httpStatus = a} :: GetQuantumTaskResponse)

-- | The time at which the task was created.
getQuantumTaskResponse_createdAt :: Lens.Lens' GetQuantumTaskResponse Prelude.UTCTime
getQuantumTaskResponse_createdAt = Lens.lens (\GetQuantumTaskResponse' {createdAt} -> createdAt) (\s@GetQuantumTaskResponse' {} a -> s {createdAt = a} :: GetQuantumTaskResponse) Prelude.. Core._Time

-- | The ARN of the device the task was run on.
getQuantumTaskResponse_deviceArn :: Lens.Lens' GetQuantumTaskResponse Prelude.Text
getQuantumTaskResponse_deviceArn = Lens.lens (\GetQuantumTaskResponse' {deviceArn} -> deviceArn) (\s@GetQuantumTaskResponse' {} a -> s {deviceArn = a} :: GetQuantumTaskResponse)

-- | The parameters for the device on which the task ran.
getQuantumTaskResponse_deviceParameters :: Lens.Lens' GetQuantumTaskResponse Prelude.Text
getQuantumTaskResponse_deviceParameters = Lens.lens (\GetQuantumTaskResponse' {deviceParameters} -> deviceParameters) (\s@GetQuantumTaskResponse' {} a -> s {deviceParameters = a} :: GetQuantumTaskResponse)

-- | The S3 bucket where task results are stored.
getQuantumTaskResponse_outputS3Bucket :: Lens.Lens' GetQuantumTaskResponse Prelude.Text
getQuantumTaskResponse_outputS3Bucket = Lens.lens (\GetQuantumTaskResponse' {outputS3Bucket} -> outputS3Bucket) (\s@GetQuantumTaskResponse' {} a -> s {outputS3Bucket = a} :: GetQuantumTaskResponse)

-- | The folder in the S3 bucket where task results are stored.
getQuantumTaskResponse_outputS3Directory :: Lens.Lens' GetQuantumTaskResponse Prelude.Text
getQuantumTaskResponse_outputS3Directory = Lens.lens (\GetQuantumTaskResponse' {outputS3Directory} -> outputS3Directory) (\s@GetQuantumTaskResponse' {} a -> s {outputS3Directory = a} :: GetQuantumTaskResponse)

-- | The ARN of the task.
getQuantumTaskResponse_quantumTaskArn :: Lens.Lens' GetQuantumTaskResponse Prelude.Text
getQuantumTaskResponse_quantumTaskArn = Lens.lens (\GetQuantumTaskResponse' {quantumTaskArn} -> quantumTaskArn) (\s@GetQuantumTaskResponse' {} a -> s {quantumTaskArn = a} :: GetQuantumTaskResponse)

-- | The number of shots used in the task.
getQuantumTaskResponse_shots :: Lens.Lens' GetQuantumTaskResponse Prelude.Integer
getQuantumTaskResponse_shots = Lens.lens (\GetQuantumTaskResponse' {shots} -> shots) (\s@GetQuantumTaskResponse' {} a -> s {shots = a} :: GetQuantumTaskResponse)

-- | The status of the task.
getQuantumTaskResponse_status :: Lens.Lens' GetQuantumTaskResponse QuantumTaskStatus
getQuantumTaskResponse_status = Lens.lens (\GetQuantumTaskResponse' {status} -> status) (\s@GetQuantumTaskResponse' {} a -> s {status = a} :: GetQuantumTaskResponse)

instance Prelude.NFData GetQuantumTaskResponse where
  rnf GetQuantumTaskResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf endedAt
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deviceArn
      `Prelude.seq` Prelude.rnf deviceParameters
      `Prelude.seq` Prelude.rnf outputS3Bucket
      `Prelude.seq` Prelude.rnf outputS3Directory
      `Prelude.seq` Prelude.rnf quantumTaskArn
      `Prelude.seq` Prelude.rnf shots
      `Prelude.seq` Prelude.rnf status
