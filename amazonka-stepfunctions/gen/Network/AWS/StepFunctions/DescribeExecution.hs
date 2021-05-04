{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StepFunctions.DescribeExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an execution.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- This API action is not supported by @EXPRESS@ state machines.
module Network.AWS.StepFunctions.DescribeExecution
  ( -- * Creating a Request
    DescribeExecution (..),
    newDescribeExecution,

    -- * Request Lenses
    describeExecution_executionArn,

    -- * Destructuring the Response
    DescribeExecutionResponse (..),
    newDescribeExecutionResponse,

    -- * Response Lenses
    describeExecutionResponse_stopDate,
    describeExecutionResponse_inputDetails,
    describeExecutionResponse_input,
    describeExecutionResponse_name,
    describeExecutionResponse_output,
    describeExecutionResponse_traceHeader,
    describeExecutionResponse_outputDetails,
    describeExecutionResponse_httpStatus,
    describeExecutionResponse_executionArn,
    describeExecutionResponse_stateMachineArn,
    describeExecutionResponse_status,
    describeExecutionResponse_startDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newDescribeExecution' smart constructor.
data DescribeExecution = DescribeExecution'
  { -- | The Amazon Resource Name (ARN) of the execution to describe.
    executionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionArn', 'describeExecution_executionArn' - The Amazon Resource Name (ARN) of the execution to describe.
newDescribeExecution ::
  -- | 'executionArn'
  Prelude.Text ->
  DescribeExecution
newDescribeExecution pExecutionArn_ =
  DescribeExecution' {executionArn = pExecutionArn_}

-- | The Amazon Resource Name (ARN) of the execution to describe.
describeExecution_executionArn :: Lens.Lens' DescribeExecution Prelude.Text
describeExecution_executionArn = Lens.lens (\DescribeExecution' {executionArn} -> executionArn) (\s@DescribeExecution' {} a -> s {executionArn = a} :: DescribeExecution)

instance Prelude.AWSRequest DescribeExecution where
  type Rs DescribeExecution = DescribeExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExecutionResponse'
            Prelude.<$> (x Prelude..?> "stopDate")
            Prelude.<*> (x Prelude..?> "inputDetails")
            Prelude.<*> (x Prelude..?> "input")
            Prelude.<*> (x Prelude..?> "name")
            Prelude.<*> (x Prelude..?> "output")
            Prelude.<*> (x Prelude..?> "traceHeader")
            Prelude.<*> (x Prelude..?> "outputDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "executionArn")
            Prelude.<*> (x Prelude..:> "stateMachineArn")
            Prelude.<*> (x Prelude..:> "status")
            Prelude.<*> (x Prelude..:> "startDate")
      )

instance Prelude.Hashable DescribeExecution

instance Prelude.NFData DescribeExecution

instance Prelude.ToHeaders DescribeExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSStepFunctions.DescribeExecution" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeExecution where
  toJSON DescribeExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("executionArn" Prelude..= executionArn)
          ]
      )

instance Prelude.ToPath DescribeExecution where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExecutionResponse' smart constructor.
data DescribeExecutionResponse = DescribeExecutionResponse'
  { -- | If the execution has already ended, the date the execution stopped.
    stopDate :: Prelude.Maybe Prelude.POSIX,
    inputDetails :: Prelude.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The string that contains the JSON input data of the execution. Length
    -- constraints apply to the payload size, and are expressed as bytes in
    -- UTF-8 encoding.
    input :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The name of the execution.
    --
    -- A name must /not/ contain:
    --
    -- -   white space
    --
    -- -   brackets @\< > { } [ ]@
    --
    -- -   wildcard characters @? *@
    --
    -- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
    --
    -- -   control characters (@U+0000-001F@, @U+007F-009F@)
    --
    -- To enable logging with CloudWatch Logs, the name should only contain
    -- 0-9, A-Z, a-z, - and _.
    name :: Prelude.Maybe Prelude.Text,
    -- | The JSON output data of the execution. Length constraints apply to the
    -- payload size, and are expressed as bytes in UTF-8 encoding.
    --
    -- This field is set only if the execution succeeds. If the execution
    -- fails, this field is null.
    output :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The AWS X-Ray trace header that was passed to the execution.
    traceHeader :: Prelude.Maybe Prelude.Text,
    outputDetails :: Prelude.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the executed stated machine.
    stateMachineArn :: Prelude.Text,
    -- | The current status of the execution.
    status :: ExecutionStatus,
    -- | The date the execution is started.
    startDate :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopDate', 'describeExecutionResponse_stopDate' - If the execution has already ended, the date the execution stopped.
--
-- 'inputDetails', 'describeExecutionResponse_inputDetails' - Undocumented member.
--
-- 'input', 'describeExecutionResponse_input' - The string that contains the JSON input data of the execution. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
--
-- 'name', 'describeExecutionResponse_name' - The name of the execution.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
--
-- 'output', 'describeExecutionResponse_output' - The JSON output data of the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
--
-- This field is set only if the execution succeeds. If the execution
-- fails, this field is null.
--
-- 'traceHeader', 'describeExecutionResponse_traceHeader' - The AWS X-Ray trace header that was passed to the execution.
--
-- 'outputDetails', 'describeExecutionResponse_outputDetails' - Undocumented member.
--
-- 'httpStatus', 'describeExecutionResponse_httpStatus' - The response's http status code.
--
-- 'executionArn', 'describeExecutionResponse_executionArn' - The Amazon Resource Name (ARN) that identifies the execution.
--
-- 'stateMachineArn', 'describeExecutionResponse_stateMachineArn' - The Amazon Resource Name (ARN) of the executed stated machine.
--
-- 'status', 'describeExecutionResponse_status' - The current status of the execution.
--
-- 'startDate', 'describeExecutionResponse_startDate' - The date the execution is started.
newDescribeExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'executionArn'
  Prelude.Text ->
  -- | 'stateMachineArn'
  Prelude.Text ->
  -- | 'status'
  ExecutionStatus ->
  -- | 'startDate'
  Prelude.UTCTime ->
  DescribeExecutionResponse
newDescribeExecutionResponse
  pHttpStatus_
  pExecutionArn_
  pStateMachineArn_
  pStatus_
  pStartDate_ =
    DescribeExecutionResponse'
      { stopDate =
          Prelude.Nothing,
        inputDetails = Prelude.Nothing,
        input = Prelude.Nothing,
        name = Prelude.Nothing,
        output = Prelude.Nothing,
        traceHeader = Prelude.Nothing,
        outputDetails = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        executionArn = pExecutionArn_,
        stateMachineArn = pStateMachineArn_,
        status = pStatus_,
        startDate = Prelude._Time Lens.# pStartDate_
      }

-- | If the execution has already ended, the date the execution stopped.
describeExecutionResponse_stopDate :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describeExecutionResponse_stopDate = Lens.lens (\DescribeExecutionResponse' {stopDate} -> stopDate) (\s@DescribeExecutionResponse' {} a -> s {stopDate = a} :: DescribeExecutionResponse) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
describeExecutionResponse_inputDetails :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe CloudWatchEventsExecutionDataDetails)
describeExecutionResponse_inputDetails = Lens.lens (\DescribeExecutionResponse' {inputDetails} -> inputDetails) (\s@DescribeExecutionResponse' {} a -> s {inputDetails = a} :: DescribeExecutionResponse)

-- | The string that contains the JSON input data of the execution. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
describeExecutionResponse_input :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_input = Lens.lens (\DescribeExecutionResponse' {input} -> input) (\s@DescribeExecutionResponse' {} a -> s {input = a} :: DescribeExecutionResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The name of the execution.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
describeExecutionResponse_name :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_name = Lens.lens (\DescribeExecutionResponse' {name} -> name) (\s@DescribeExecutionResponse' {} a -> s {name = a} :: DescribeExecutionResponse)

-- | The JSON output data of the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
--
-- This field is set only if the execution succeeds. If the execution
-- fails, this field is null.
describeExecutionResponse_output :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_output = Lens.lens (\DescribeExecutionResponse' {output} -> output) (\s@DescribeExecutionResponse' {} a -> s {output = a} :: DescribeExecutionResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The AWS X-Ray trace header that was passed to the execution.
describeExecutionResponse_traceHeader :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_traceHeader = Lens.lens (\DescribeExecutionResponse' {traceHeader} -> traceHeader) (\s@DescribeExecutionResponse' {} a -> s {traceHeader = a} :: DescribeExecutionResponse)

-- | Undocumented member.
describeExecutionResponse_outputDetails :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe CloudWatchEventsExecutionDataDetails)
describeExecutionResponse_outputDetails = Lens.lens (\DescribeExecutionResponse' {outputDetails} -> outputDetails) (\s@DescribeExecutionResponse' {} a -> s {outputDetails = a} :: DescribeExecutionResponse)

-- | The response's http status code.
describeExecutionResponse_httpStatus :: Lens.Lens' DescribeExecutionResponse Prelude.Int
describeExecutionResponse_httpStatus = Lens.lens (\DescribeExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribeExecutionResponse' {} a -> s {httpStatus = a} :: DescribeExecutionResponse)

-- | The Amazon Resource Name (ARN) that identifies the execution.
describeExecutionResponse_executionArn :: Lens.Lens' DescribeExecutionResponse Prelude.Text
describeExecutionResponse_executionArn = Lens.lens (\DescribeExecutionResponse' {executionArn} -> executionArn) (\s@DescribeExecutionResponse' {} a -> s {executionArn = a} :: DescribeExecutionResponse)

-- | The Amazon Resource Name (ARN) of the executed stated machine.
describeExecutionResponse_stateMachineArn :: Lens.Lens' DescribeExecutionResponse Prelude.Text
describeExecutionResponse_stateMachineArn = Lens.lens (\DescribeExecutionResponse' {stateMachineArn} -> stateMachineArn) (\s@DescribeExecutionResponse' {} a -> s {stateMachineArn = a} :: DescribeExecutionResponse)

-- | The current status of the execution.
describeExecutionResponse_status :: Lens.Lens' DescribeExecutionResponse ExecutionStatus
describeExecutionResponse_status = Lens.lens (\DescribeExecutionResponse' {status} -> status) (\s@DescribeExecutionResponse' {} a -> s {status = a} :: DescribeExecutionResponse)

-- | The date the execution is started.
describeExecutionResponse_startDate :: Lens.Lens' DescribeExecutionResponse Prelude.UTCTime
describeExecutionResponse_startDate = Lens.lens (\DescribeExecutionResponse' {startDate} -> startDate) (\s@DescribeExecutionResponse' {} a -> s {startDate = a} :: DescribeExecutionResponse) Prelude.. Prelude._Time

instance Prelude.NFData DescribeExecutionResponse
