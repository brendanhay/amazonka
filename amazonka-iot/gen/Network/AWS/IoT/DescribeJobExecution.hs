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
-- Module      : Network.AWS.IoT.DescribeJobExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a job execution.
module Network.AWS.IoT.DescribeJobExecution
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
      ["executionNumber" Core.=: executionNumber]

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

instance Prelude.NFData DescribeJobExecutionResponse
