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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newDescribeExecution' smart constructor.
data DescribeExecution = DescribeExecution'
  { -- | The Amazon Resource Name (ARN) of the execution to describe.
    executionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeExecution
newDescribeExecution pExecutionArn_ =
  DescribeExecution' {executionArn = pExecutionArn_}

-- | The Amazon Resource Name (ARN) of the execution to describe.
describeExecution_executionArn :: Lens.Lens' DescribeExecution Core.Text
describeExecution_executionArn = Lens.lens (\DescribeExecution' {executionArn} -> executionArn) (\s@DescribeExecution' {} a -> s {executionArn = a} :: DescribeExecution)

instance Core.AWSRequest DescribeExecution where
  type
    AWSResponse DescribeExecution =
      DescribeExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExecutionResponse'
            Core.<$> (x Core..?> "stopDate")
            Core.<*> (x Core..?> "inputDetails")
            Core.<*> (x Core..?> "input")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "output")
            Core.<*> (x Core..?> "traceHeader")
            Core.<*> (x Core..?> "outputDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "executionArn")
            Core.<*> (x Core..:> "stateMachineArn")
            Core.<*> (x Core..:> "status")
            Core.<*> (x Core..:> "startDate")
      )

instance Core.Hashable DescribeExecution

instance Core.NFData DescribeExecution

instance Core.ToHeaders DescribeExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.DescribeExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeExecution where
  toJSON DescribeExecution' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("executionArn" Core..= executionArn)]
      )

instance Core.ToPath DescribeExecution where
  toPath = Core.const "/"

instance Core.ToQuery DescribeExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeExecutionResponse' smart constructor.
data DescribeExecutionResponse = DescribeExecutionResponse'
  { -- | If the execution has already ended, the date the execution stopped.
    stopDate :: Core.Maybe Core.POSIX,
    inputDetails :: Core.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The string that contains the JSON input data of the execution. Length
    -- constraints apply to the payload size, and are expressed as bytes in
    -- UTF-8 encoding.
    input :: Core.Maybe (Core.Sensitive Core.Text),
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
    name :: Core.Maybe Core.Text,
    -- | The JSON output data of the execution. Length constraints apply to the
    -- payload size, and are expressed as bytes in UTF-8 encoding.
    --
    -- This field is set only if the execution succeeds. If the execution
    -- fails, this field is null.
    output :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The AWS X-Ray trace header that was passed to the execution.
    traceHeader :: Core.Maybe Core.Text,
    outputDetails :: Core.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionArn :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the executed stated machine.
    stateMachineArn :: Core.Text,
    -- | The current status of the execution.
    status :: ExecutionStatus,
    -- | The date the execution is started.
    startDate :: Core.POSIX
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'executionArn'
  Core.Text ->
  -- | 'stateMachineArn'
  Core.Text ->
  -- | 'status'
  ExecutionStatus ->
  -- | 'startDate'
  Core.UTCTime ->
  DescribeExecutionResponse
newDescribeExecutionResponse
  pHttpStatus_
  pExecutionArn_
  pStateMachineArn_
  pStatus_
  pStartDate_ =
    DescribeExecutionResponse'
      { stopDate = Core.Nothing,
        inputDetails = Core.Nothing,
        input = Core.Nothing,
        name = Core.Nothing,
        output = Core.Nothing,
        traceHeader = Core.Nothing,
        outputDetails = Core.Nothing,
        httpStatus = pHttpStatus_,
        executionArn = pExecutionArn_,
        stateMachineArn = pStateMachineArn_,
        status = pStatus_,
        startDate = Core._Time Lens.# pStartDate_
      }

-- | If the execution has already ended, the date the execution stopped.
describeExecutionResponse_stopDate :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Core.UTCTime)
describeExecutionResponse_stopDate = Lens.lens (\DescribeExecutionResponse' {stopDate} -> stopDate) (\s@DescribeExecutionResponse' {} a -> s {stopDate = a} :: DescribeExecutionResponse) Core.. Lens.mapping Core._Time

-- | Undocumented member.
describeExecutionResponse_inputDetails :: Lens.Lens' DescribeExecutionResponse (Core.Maybe CloudWatchEventsExecutionDataDetails)
describeExecutionResponse_inputDetails = Lens.lens (\DescribeExecutionResponse' {inputDetails} -> inputDetails) (\s@DescribeExecutionResponse' {} a -> s {inputDetails = a} :: DescribeExecutionResponse)

-- | The string that contains the JSON input data of the execution. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
describeExecutionResponse_input :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Core.Text)
describeExecutionResponse_input = Lens.lens (\DescribeExecutionResponse' {input} -> input) (\s@DescribeExecutionResponse' {} a -> s {input = a} :: DescribeExecutionResponse) Core.. Lens.mapping Core._Sensitive

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
describeExecutionResponse_name :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Core.Text)
describeExecutionResponse_name = Lens.lens (\DescribeExecutionResponse' {name} -> name) (\s@DescribeExecutionResponse' {} a -> s {name = a} :: DescribeExecutionResponse)

-- | The JSON output data of the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
--
-- This field is set only if the execution succeeds. If the execution
-- fails, this field is null.
describeExecutionResponse_output :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Core.Text)
describeExecutionResponse_output = Lens.lens (\DescribeExecutionResponse' {output} -> output) (\s@DescribeExecutionResponse' {} a -> s {output = a} :: DescribeExecutionResponse) Core.. Lens.mapping Core._Sensitive

-- | The AWS X-Ray trace header that was passed to the execution.
describeExecutionResponse_traceHeader :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Core.Text)
describeExecutionResponse_traceHeader = Lens.lens (\DescribeExecutionResponse' {traceHeader} -> traceHeader) (\s@DescribeExecutionResponse' {} a -> s {traceHeader = a} :: DescribeExecutionResponse)

-- | Undocumented member.
describeExecutionResponse_outputDetails :: Lens.Lens' DescribeExecutionResponse (Core.Maybe CloudWatchEventsExecutionDataDetails)
describeExecutionResponse_outputDetails = Lens.lens (\DescribeExecutionResponse' {outputDetails} -> outputDetails) (\s@DescribeExecutionResponse' {} a -> s {outputDetails = a} :: DescribeExecutionResponse)

-- | The response's http status code.
describeExecutionResponse_httpStatus :: Lens.Lens' DescribeExecutionResponse Core.Int
describeExecutionResponse_httpStatus = Lens.lens (\DescribeExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribeExecutionResponse' {} a -> s {httpStatus = a} :: DescribeExecutionResponse)

-- | The Amazon Resource Name (ARN) that identifies the execution.
describeExecutionResponse_executionArn :: Lens.Lens' DescribeExecutionResponse Core.Text
describeExecutionResponse_executionArn = Lens.lens (\DescribeExecutionResponse' {executionArn} -> executionArn) (\s@DescribeExecutionResponse' {} a -> s {executionArn = a} :: DescribeExecutionResponse)

-- | The Amazon Resource Name (ARN) of the executed stated machine.
describeExecutionResponse_stateMachineArn :: Lens.Lens' DescribeExecutionResponse Core.Text
describeExecutionResponse_stateMachineArn = Lens.lens (\DescribeExecutionResponse' {stateMachineArn} -> stateMachineArn) (\s@DescribeExecutionResponse' {} a -> s {stateMachineArn = a} :: DescribeExecutionResponse)

-- | The current status of the execution.
describeExecutionResponse_status :: Lens.Lens' DescribeExecutionResponse ExecutionStatus
describeExecutionResponse_status = Lens.lens (\DescribeExecutionResponse' {status} -> status) (\s@DescribeExecutionResponse' {} a -> s {status = a} :: DescribeExecutionResponse)

-- | The date the execution is started.
describeExecutionResponse_startDate :: Lens.Lens' DescribeExecutionResponse Core.UTCTime
describeExecutionResponse_startDate = Lens.lens (\DescribeExecutionResponse' {startDate} -> startDate) (\s@DescribeExecutionResponse' {} a -> s {startDate = a} :: DescribeExecutionResponse) Core.. Core._Time

instance Core.NFData DescribeExecutionResponse
