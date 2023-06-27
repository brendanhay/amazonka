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
-- Module      : Amazonka.StepFunctions.DescribeExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a state machine execution, such as the state
-- machine associated with the execution, the execution input and output,
-- and relevant execution metadata. Use this API action to return the Map
-- Run Amazon Resource Name (ARN) if the execution was dispatched by a Map
-- Run.
--
-- If you specify a version or alias ARN when you call the StartExecution
-- API action, @DescribeExecution@ returns that ARN.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- Executions of an @EXPRESS@ state machinearen\'t supported by
-- @DescribeExecution@ unless a Map Run dispatched them.
module Amazonka.StepFunctions.DescribeExecution
  ( -- * Creating a Request
    DescribeExecution (..),
    newDescribeExecution,

    -- * Request Lenses
    describeExecution_executionArn,

    -- * Destructuring the Response
    DescribeExecutionResponse (..),
    newDescribeExecutionResponse,

    -- * Response Lenses
    describeExecutionResponse_cause,
    describeExecutionResponse_error,
    describeExecutionResponse_input,
    describeExecutionResponse_inputDetails,
    describeExecutionResponse_mapRunArn,
    describeExecutionResponse_name,
    describeExecutionResponse_output,
    describeExecutionResponse_outputDetails,
    describeExecutionResponse_stateMachineAliasArn,
    describeExecutionResponse_stateMachineVersionArn,
    describeExecutionResponse_stopDate,
    describeExecutionResponse_traceHeader,
    describeExecutionResponse_httpStatus,
    describeExecutionResponse_executionArn,
    describeExecutionResponse_stateMachineArn,
    describeExecutionResponse_status,
    describeExecutionResponse_startDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newDescribeExecution' smart constructor.
data DescribeExecution = DescribeExecution'
  { -- | The Amazon Resource Name (ARN) of the execution to describe.
    executionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DescribeExecution where
  type
    AWSResponse DescribeExecution =
      DescribeExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExecutionResponse'
            Prelude.<$> (x Data..?> "cause")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "input")
            Prelude.<*> (x Data..?> "inputDetails")
            Prelude.<*> (x Data..?> "mapRunArn")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "output")
            Prelude.<*> (x Data..?> "outputDetails")
            Prelude.<*> (x Data..?> "stateMachineAliasArn")
            Prelude.<*> (x Data..?> "stateMachineVersionArn")
            Prelude.<*> (x Data..?> "stopDate")
            Prelude.<*> (x Data..?> "traceHeader")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "executionArn")
            Prelude.<*> (x Data..:> "stateMachineArn")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "startDate")
      )

instance Prelude.Hashable DescribeExecution where
  hashWithSalt _salt DescribeExecution' {..} =
    _salt `Prelude.hashWithSalt` executionArn

instance Prelude.NFData DescribeExecution where
  rnf DescribeExecution' {..} = Prelude.rnf executionArn

instance Data.ToHeaders DescribeExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.DescribeExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeExecution where
  toJSON DescribeExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("executionArn" Data..= executionArn)]
      )

instance Data.ToPath DescribeExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExecutionResponse' smart constructor.
data DescribeExecutionResponse = DescribeExecutionResponse'
  { -- | The cause string if the state machine execution failed.
    cause :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The error string if the state machine execution failed.
    error :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The string that contains the JSON input data of the execution. Length
    -- constraints apply to the payload size, and are expressed as bytes in
    -- UTF-8 encoding.
    input :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    inputDetails :: Prelude.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The Amazon Resource Name (ARN) that identifies a Map Run, which
    -- dispatched this execution.
    mapRunArn :: Prelude.Maybe Prelude.Text,
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
    output :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    outputDetails :: Prelude.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The Amazon Resource Name (ARN) of the state machine alias associated
    -- with the execution. The alias ARN is a combination of state machine ARN
    -- and the alias name separated by a colon (:). For example,
    -- @stateMachineARN:PROD@.
    --
    -- If you start an execution from a @StartExecution@ request with a state
    -- machine version ARN, this field will be null.
    stateMachineAliasArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the state machine version associated
    -- with the execution. The version ARN is a combination of state machine
    -- ARN and the version number separated by a colon (:). For example,
    -- @stateMachineARN:1@.
    --
    -- If you start an execution from a @StartExecution@ request without
    -- specifying a state machine version or alias ARN, Step Functions returns
    -- a null value.
    stateMachineVersionArn :: Prelude.Maybe Prelude.Text,
    -- | If the execution ended, the date the execution stopped.
    stopDate :: Prelude.Maybe Data.POSIX,
    -- | The X-Ray trace header that was passed to the execution.
    traceHeader :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the executed stated machine.
    stateMachineArn :: Prelude.Text,
    -- | The current status of the execution.
    status :: ExecutionStatus,
    -- | The date the execution is started.
    startDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'describeExecutionResponse_cause' - The cause string if the state machine execution failed.
--
-- 'error', 'describeExecutionResponse_error' - The error string if the state machine execution failed.
--
-- 'input', 'describeExecutionResponse_input' - The string that contains the JSON input data of the execution. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
--
-- 'inputDetails', 'describeExecutionResponse_inputDetails' - Undocumented member.
--
-- 'mapRunArn', 'describeExecutionResponse_mapRunArn' - The Amazon Resource Name (ARN) that identifies a Map Run, which
-- dispatched this execution.
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
-- 'outputDetails', 'describeExecutionResponse_outputDetails' - Undocumented member.
--
-- 'stateMachineAliasArn', 'describeExecutionResponse_stateMachineAliasArn' - The Amazon Resource Name (ARN) of the state machine alias associated
-- with the execution. The alias ARN is a combination of state machine ARN
-- and the alias name separated by a colon (:). For example,
-- @stateMachineARN:PROD@.
--
-- If you start an execution from a @StartExecution@ request with a state
-- machine version ARN, this field will be null.
--
-- 'stateMachineVersionArn', 'describeExecutionResponse_stateMachineVersionArn' - The Amazon Resource Name (ARN) of the state machine version associated
-- with the execution. The version ARN is a combination of state machine
-- ARN and the version number separated by a colon (:). For example,
-- @stateMachineARN:1@.
--
-- If you start an execution from a @StartExecution@ request without
-- specifying a state machine version or alias ARN, Step Functions returns
-- a null value.
--
-- 'stopDate', 'describeExecutionResponse_stopDate' - If the execution ended, the date the execution stopped.
--
-- 'traceHeader', 'describeExecutionResponse_traceHeader' - The X-Ray trace header that was passed to the execution.
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
      { cause = Prelude.Nothing,
        error = Prelude.Nothing,
        input = Prelude.Nothing,
        inputDetails = Prelude.Nothing,
        mapRunArn = Prelude.Nothing,
        name = Prelude.Nothing,
        output = Prelude.Nothing,
        outputDetails = Prelude.Nothing,
        stateMachineAliasArn = Prelude.Nothing,
        stateMachineVersionArn = Prelude.Nothing,
        stopDate = Prelude.Nothing,
        traceHeader = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        executionArn = pExecutionArn_,
        stateMachineArn = pStateMachineArn_,
        status = pStatus_,
        startDate = Data._Time Lens.# pStartDate_
      }

-- | The cause string if the state machine execution failed.
describeExecutionResponse_cause :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_cause = Lens.lens (\DescribeExecutionResponse' {cause} -> cause) (\s@DescribeExecutionResponse' {} a -> s {cause = a} :: DescribeExecutionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The error string if the state machine execution failed.
describeExecutionResponse_error :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_error = Lens.lens (\DescribeExecutionResponse' {error} -> error) (\s@DescribeExecutionResponse' {} a -> s {error = a} :: DescribeExecutionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The string that contains the JSON input data of the execution. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
describeExecutionResponse_input :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_input = Lens.lens (\DescribeExecutionResponse' {input} -> input) (\s@DescribeExecutionResponse' {} a -> s {input = a} :: DescribeExecutionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
describeExecutionResponse_inputDetails :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe CloudWatchEventsExecutionDataDetails)
describeExecutionResponse_inputDetails = Lens.lens (\DescribeExecutionResponse' {inputDetails} -> inputDetails) (\s@DescribeExecutionResponse' {} a -> s {inputDetails = a} :: DescribeExecutionResponse)

-- | The Amazon Resource Name (ARN) that identifies a Map Run, which
-- dispatched this execution.
describeExecutionResponse_mapRunArn :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_mapRunArn = Lens.lens (\DescribeExecutionResponse' {mapRunArn} -> mapRunArn) (\s@DescribeExecutionResponse' {} a -> s {mapRunArn = a} :: DescribeExecutionResponse)

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
describeExecutionResponse_output = Lens.lens (\DescribeExecutionResponse' {output} -> output) (\s@DescribeExecutionResponse' {} a -> s {output = a} :: DescribeExecutionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
describeExecutionResponse_outputDetails :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe CloudWatchEventsExecutionDataDetails)
describeExecutionResponse_outputDetails = Lens.lens (\DescribeExecutionResponse' {outputDetails} -> outputDetails) (\s@DescribeExecutionResponse' {} a -> s {outputDetails = a} :: DescribeExecutionResponse)

-- | The Amazon Resource Name (ARN) of the state machine alias associated
-- with the execution. The alias ARN is a combination of state machine ARN
-- and the alias name separated by a colon (:). For example,
-- @stateMachineARN:PROD@.
--
-- If you start an execution from a @StartExecution@ request with a state
-- machine version ARN, this field will be null.
describeExecutionResponse_stateMachineAliasArn :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_stateMachineAliasArn = Lens.lens (\DescribeExecutionResponse' {stateMachineAliasArn} -> stateMachineAliasArn) (\s@DescribeExecutionResponse' {} a -> s {stateMachineAliasArn = a} :: DescribeExecutionResponse)

-- | The Amazon Resource Name (ARN) of the state machine version associated
-- with the execution. The version ARN is a combination of state machine
-- ARN and the version number separated by a colon (:). For example,
-- @stateMachineARN:1@.
--
-- If you start an execution from a @StartExecution@ request without
-- specifying a state machine version or alias ARN, Step Functions returns
-- a null value.
describeExecutionResponse_stateMachineVersionArn :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_stateMachineVersionArn = Lens.lens (\DescribeExecutionResponse' {stateMachineVersionArn} -> stateMachineVersionArn) (\s@DescribeExecutionResponse' {} a -> s {stateMachineVersionArn = a} :: DescribeExecutionResponse)

-- | If the execution ended, the date the execution stopped.
describeExecutionResponse_stopDate :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describeExecutionResponse_stopDate = Lens.lens (\DescribeExecutionResponse' {stopDate} -> stopDate) (\s@DescribeExecutionResponse' {} a -> s {stopDate = a} :: DescribeExecutionResponse) Prelude.. Lens.mapping Data._Time

-- | The X-Ray trace header that was passed to the execution.
describeExecutionResponse_traceHeader :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_traceHeader = Lens.lens (\DescribeExecutionResponse' {traceHeader} -> traceHeader) (\s@DescribeExecutionResponse' {} a -> s {traceHeader = a} :: DescribeExecutionResponse)

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
describeExecutionResponse_startDate = Lens.lens (\DescribeExecutionResponse' {startDate} -> startDate) (\s@DescribeExecutionResponse' {} a -> s {startDate = a} :: DescribeExecutionResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeExecutionResponse where
  rnf DescribeExecutionResponse' {..} =
    Prelude.rnf cause
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf inputDetails
      `Prelude.seq` Prelude.rnf mapRunArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf output
      `Prelude.seq` Prelude.rnf outputDetails
      `Prelude.seq` Prelude.rnf stateMachineAliasArn
      `Prelude.seq` Prelude.rnf stateMachineVersionArn
      `Prelude.seq` Prelude.rnf stopDate
      `Prelude.seq` Prelude.rnf traceHeader
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf executionArn
      `Prelude.seq` Prelude.rnf stateMachineArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf startDate
