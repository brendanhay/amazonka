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
-- Module      : Amazonka.IoTJobsData.DescribeJobExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of a job execution.
module Amazonka.IoTJobsData.DescribeJobExecution
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

import qualified Amazonka.Core as Core
import Amazonka.IoTJobsData.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeJobExecution' smart constructor.
data DescribeJobExecution = DescribeJobExecution'
  { -- | Optional. When set to true, the response contains the job document. The
    -- default is false.
    includeJobDocument :: Prelude.Maybe Prelude.Bool,
    -- | Optional. A number that identifies a particular job execution on a
    -- particular device. If not specified, the latest job execution is
    -- returned.
    executionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier assigned to this job when it was created.
    jobId :: Prelude.Text,
    -- | The thing name associated with the device the job execution is running
    -- on.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'thingName'
  Prelude.Text ->
  DescribeJobExecution
newDescribeJobExecution pJobId_ pThingName_ =
  DescribeJobExecution'
    { includeJobDocument =
        Prelude.Nothing,
      executionNumber = Prelude.Nothing,
      jobId = pJobId_,
      thingName = pThingName_
    }

-- | Optional. When set to true, the response contains the job document. The
-- default is false.
describeJobExecution_includeJobDocument :: Lens.Lens' DescribeJobExecution (Prelude.Maybe Prelude.Bool)
describeJobExecution_includeJobDocument = Lens.lens (\DescribeJobExecution' {includeJobDocument} -> includeJobDocument) (\s@DescribeJobExecution' {} a -> s {includeJobDocument = a} :: DescribeJobExecution)

-- | Optional. A number that identifies a particular job execution on a
-- particular device. If not specified, the latest job execution is
-- returned.
describeJobExecution_executionNumber :: Lens.Lens' DescribeJobExecution (Prelude.Maybe Prelude.Integer)
describeJobExecution_executionNumber = Lens.lens (\DescribeJobExecution' {executionNumber} -> executionNumber) (\s@DescribeJobExecution' {} a -> s {executionNumber = a} :: DescribeJobExecution)

-- | The unique identifier assigned to this job when it was created.
describeJobExecution_jobId :: Lens.Lens' DescribeJobExecution Prelude.Text
describeJobExecution_jobId = Lens.lens (\DescribeJobExecution' {jobId} -> jobId) (\s@DescribeJobExecution' {} a -> s {jobId = a} :: DescribeJobExecution)

-- | The thing name associated with the device the job execution is running
-- on.
describeJobExecution_thingName :: Lens.Lens' DescribeJobExecution Prelude.Text
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
            Prelude.<$> (x Core..?> "execution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJobExecution

instance Prelude.NFData DescribeJobExecution

instance Core.ToHeaders DescribeJobExecution where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeJobExecution where
  toPath DescribeJobExecution' {..} =
    Prelude.mconcat
      [ "/things/",
        Core.toBS thingName,
        "/jobs/",
        Core.toBS jobId
      ]

instance Core.ToQuery DescribeJobExecution where
  toQuery DescribeJobExecution' {..} =
    Prelude.mconcat
      [ "includeJobDocument" Core.=: includeJobDocument,
        "executionNumber" Core.=: executionNumber
      ]

-- | /See:/ 'newDescribeJobExecutionResponse' smart constructor.
data DescribeJobExecutionResponse = DescribeJobExecutionResponse'
  { -- | Contains data about a job execution.
    execution :: Prelude.Maybe JobExecution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeJobExecutionResponse
newDescribeJobExecutionResponse pHttpStatus_ =
  DescribeJobExecutionResponse'
    { execution =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains data about a job execution.
describeJobExecutionResponse_execution :: Lens.Lens' DescribeJobExecutionResponse (Prelude.Maybe JobExecution)
describeJobExecutionResponse_execution = Lens.lens (\DescribeJobExecutionResponse' {execution} -> execution) (\s@DescribeJobExecutionResponse' {} a -> s {execution = a} :: DescribeJobExecutionResponse)

-- | The response's http status code.
describeJobExecutionResponse_httpStatus :: Lens.Lens' DescribeJobExecutionResponse Prelude.Int
describeJobExecutionResponse_httpStatus = Lens.lens (\DescribeJobExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribeJobExecutionResponse' {} a -> s {httpStatus = a} :: DescribeJobExecutionResponse)

instance Prelude.NFData DescribeJobExecutionResponse
