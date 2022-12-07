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
-- Module      : Amazonka.IoT.DescribeJobExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a job execution.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeJobExecution>
-- action.
module Amazonka.IoT.DescribeJobExecution
  ( -- * Creating a Request
    DescribeJobExecution (..),
    newDescribeJobExecution,

    -- * Request Lenses
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeJobExecution' smart constructor.
data DescribeJobExecution = DescribeJobExecution'
  { -- | A string (consisting of the digits \"0\" through \"9\" which is used to
    -- specify a particular job execution on a particular device.
    executionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Text,
    -- | The name of the thing on which the job execution is running.
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
-- 'executionNumber', 'describeJobExecution_executionNumber' - A string (consisting of the digits \"0\" through \"9\" which is used to
-- specify a particular job execution on a particular device.
--
-- 'jobId', 'describeJobExecution_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'thingName', 'describeJobExecution_thingName' - The name of the thing on which the job execution is running.
newDescribeJobExecution ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'thingName'
  Prelude.Text ->
  DescribeJobExecution
newDescribeJobExecution pJobId_ pThingName_ =
  DescribeJobExecution'
    { executionNumber =
        Prelude.Nothing,
      jobId = pJobId_,
      thingName = pThingName_
    }

-- | A string (consisting of the digits \"0\" through \"9\" which is used to
-- specify a particular job execution on a particular device.
describeJobExecution_executionNumber :: Lens.Lens' DescribeJobExecution (Prelude.Maybe Prelude.Integer)
describeJobExecution_executionNumber = Lens.lens (\DescribeJobExecution' {executionNumber} -> executionNumber) (\s@DescribeJobExecution' {} a -> s {executionNumber = a} :: DescribeJobExecution)

-- | The unique identifier you assigned to this job when it was created.
describeJobExecution_jobId :: Lens.Lens' DescribeJobExecution Prelude.Text
describeJobExecution_jobId = Lens.lens (\DescribeJobExecution' {jobId} -> jobId) (\s@DescribeJobExecution' {} a -> s {jobId = a} :: DescribeJobExecution)

-- | The name of the thing on which the job execution is running.
describeJobExecution_thingName :: Lens.Lens' DescribeJobExecution Prelude.Text
describeJobExecution_thingName = Lens.lens (\DescribeJobExecution' {thingName} -> thingName) (\s@DescribeJobExecution' {} a -> s {thingName = a} :: DescribeJobExecution)

instance Core.AWSRequest DescribeJobExecution where
  type
    AWSResponse DescribeJobExecution =
      DescribeJobExecutionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobExecutionResponse'
            Prelude.<$> (x Data..?> "execution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJobExecution where
  hashWithSalt _salt DescribeJobExecution' {..} =
    _salt `Prelude.hashWithSalt` executionNumber
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData DescribeJobExecution where
  rnf DescribeJobExecution' {..} =
    Prelude.rnf executionNumber
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf thingName

instance Data.ToHeaders DescribeJobExecution where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeJobExecution where
  toPath DescribeJobExecution' {..} =
    Prelude.mconcat
      [ "/things/",
        Data.toBS thingName,
        "/jobs/",
        Data.toBS jobId
      ]

instance Data.ToQuery DescribeJobExecution where
  toQuery DescribeJobExecution' {..} =
    Prelude.mconcat
      ["executionNumber" Data.=: executionNumber]

-- | /See:/ 'newDescribeJobExecutionResponse' smart constructor.
data DescribeJobExecutionResponse = DescribeJobExecutionResponse'
  { -- | Information about the job execution.
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
-- 'execution', 'describeJobExecutionResponse_execution' - Information about the job execution.
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

-- | Information about the job execution.
describeJobExecutionResponse_execution :: Lens.Lens' DescribeJobExecutionResponse (Prelude.Maybe JobExecution)
describeJobExecutionResponse_execution = Lens.lens (\DescribeJobExecutionResponse' {execution} -> execution) (\s@DescribeJobExecutionResponse' {} a -> s {execution = a} :: DescribeJobExecutionResponse)

-- | The response's http status code.
describeJobExecutionResponse_httpStatus :: Lens.Lens' DescribeJobExecutionResponse Prelude.Int
describeJobExecutionResponse_httpStatus = Lens.lens (\DescribeJobExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribeJobExecutionResponse' {} a -> s {httpStatus = a} :: DescribeJobExecutionResponse)

instance Prelude.NFData DescribeJobExecutionResponse where
  rnf DescribeJobExecutionResponse' {..} =
    Prelude.rnf execution
      `Prelude.seq` Prelude.rnf httpStatus
