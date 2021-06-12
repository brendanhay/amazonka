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
-- Module      : Network.AWS.IoTJobsData.DescribeJobExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of a job execution.
module Network.AWS.IoTJobsData.DescribeJobExecution
  ( -- * Creating a Request
    DescribeJobExecution (..),
    newDescribeJobExecution,

    -- * Request Lenses
    describeJobExecution_includeJobDocument,
    describeJobExecution_executionNumber,
    describeJobExecution_jobId,
    describeJobExecution_thingName,

    -- * Destructuring the Response
    DescribeJobExecutionResponse (..),
    newDescribeJobExecutionResponse,

    -- * Response Lenses
    describeJobExecutionResponse_execution,
    describeJobExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTJobsData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeJobExecution' smart constructor.
data DescribeJobExecution = DescribeJobExecution'
  { -- | Optional. When set to true, the response contains the job document. The
    -- default is false.
    includeJobDocument :: Core.Maybe Core.Bool,
    -- | Optional. A number that identifies a particular job execution on a
    -- particular device. If not specified, the latest job execution is
    -- returned.
    executionNumber :: Core.Maybe Core.Integer,
    -- | The unique identifier assigned to this job when it was created.
    jobId :: Core.Text,
    -- | The thing name associated with the device the job execution is running
    -- on.
    thingName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJobExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeJobDocument', 'describeJobExecution_includeJobDocument' - Optional. When set to true, the response contains the job document. The
-- default is false.
--
-- 'executionNumber', 'describeJobExecution_executionNumber' - Optional. A number that identifies a particular job execution on a
-- particular device. If not specified, the latest job execution is
-- returned.
--
-- 'jobId', 'describeJobExecution_jobId' - The unique identifier assigned to this job when it was created.
--
-- 'thingName', 'describeJobExecution_thingName' - The thing name associated with the device the job execution is running
-- on.
newDescribeJobExecution ::
  -- | 'jobId'
  Core.Text ->
  -- | 'thingName'
  Core.Text ->
  DescribeJobExecution
newDescribeJobExecution pJobId_ pThingName_ =
  DescribeJobExecution'
    { includeJobDocument =
        Core.Nothing,
      executionNumber = Core.Nothing,
      jobId = pJobId_,
      thingName = pThingName_
    }

-- | Optional. When set to true, the response contains the job document. The
-- default is false.
describeJobExecution_includeJobDocument :: Lens.Lens' DescribeJobExecution (Core.Maybe Core.Bool)
describeJobExecution_includeJobDocument = Lens.lens (\DescribeJobExecution' {includeJobDocument} -> includeJobDocument) (\s@DescribeJobExecution' {} a -> s {includeJobDocument = a} :: DescribeJobExecution)

-- | Optional. A number that identifies a particular job execution on a
-- particular device. If not specified, the latest job execution is
-- returned.
describeJobExecution_executionNumber :: Lens.Lens' DescribeJobExecution (Core.Maybe Core.Integer)
describeJobExecution_executionNumber = Lens.lens (\DescribeJobExecution' {executionNumber} -> executionNumber) (\s@DescribeJobExecution' {} a -> s {executionNumber = a} :: DescribeJobExecution)

-- | The unique identifier assigned to this job when it was created.
describeJobExecution_jobId :: Lens.Lens' DescribeJobExecution Core.Text
describeJobExecution_jobId = Lens.lens (\DescribeJobExecution' {jobId} -> jobId) (\s@DescribeJobExecution' {} a -> s {jobId = a} :: DescribeJobExecution)

-- | The thing name associated with the device the job execution is running
-- on.
describeJobExecution_thingName :: Lens.Lens' DescribeJobExecution Core.Text
describeJobExecution_thingName = Lens.lens (\DescribeJobExecution' {thingName} -> thingName) (\s@DescribeJobExecution' {} a -> s {thingName = a} :: DescribeJobExecution)

instance Core.AWSRequest DescribeJobExecution where
  type
    AWSResponse DescribeJobExecution =
      DescribeJobExecutionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobExecutionResponse'
            Core.<$> (x Core..?> "execution")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeJobExecution

instance Core.NFData DescribeJobExecution

instance Core.ToHeaders DescribeJobExecution where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeJobExecution where
  toPath DescribeJobExecution' {..} =
    Core.mconcat
      [ "/things/",
        Core.toBS thingName,
        "/jobs/",
        Core.toBS jobId
      ]

instance Core.ToQuery DescribeJobExecution where
  toQuery DescribeJobExecution' {..} =
    Core.mconcat
      [ "includeJobDocument" Core.=: includeJobDocument,
        "executionNumber" Core.=: executionNumber
      ]

-- | /See:/ 'newDescribeJobExecutionResponse' smart constructor.
data DescribeJobExecutionResponse = DescribeJobExecutionResponse'
  { -- | Contains data about a job execution.
    execution :: Core.Maybe JobExecution,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJobExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'execution', 'describeJobExecutionResponse_execution' - Contains data about a job execution.
--
-- 'httpStatus', 'describeJobExecutionResponse_httpStatus' - The response's http status code.
newDescribeJobExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeJobExecutionResponse
newDescribeJobExecutionResponse pHttpStatus_ =
  DescribeJobExecutionResponse'
    { execution =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains data about a job execution.
describeJobExecutionResponse_execution :: Lens.Lens' DescribeJobExecutionResponse (Core.Maybe JobExecution)
describeJobExecutionResponse_execution = Lens.lens (\DescribeJobExecutionResponse' {execution} -> execution) (\s@DescribeJobExecutionResponse' {} a -> s {execution = a} :: DescribeJobExecutionResponse)

-- | The response's http status code.
describeJobExecutionResponse_httpStatus :: Lens.Lens' DescribeJobExecutionResponse Core.Int
describeJobExecutionResponse_httpStatus = Lens.lens (\DescribeJobExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribeJobExecutionResponse' {} a -> s {httpStatus = a} :: DescribeJobExecutionResponse)

instance Core.NFData DescribeJobExecutionResponse
