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
-- Module      : Amazonka.StepFunctions.DescribeStateMachineForExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state machine associated with a specific execution.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- This API action is not supported by @EXPRESS@ state machines.
module Amazonka.StepFunctions.DescribeStateMachineForExecution
  ( -- * Creating a Request
    DescribeStateMachineForExecution (..),
    newDescribeStateMachineForExecution,

    -- * Request Lenses
    describeStateMachineForExecution_executionArn,

    -- * Destructuring the Response
    DescribeStateMachineForExecutionResponse (..),
    newDescribeStateMachineForExecutionResponse,

    -- * Response Lenses
    describeStateMachineForExecutionResponse_tracingConfiguration,
    describeStateMachineForExecutionResponse_loggingConfiguration,
    describeStateMachineForExecutionResponse_httpStatus,
    describeStateMachineForExecutionResponse_stateMachineArn,
    describeStateMachineForExecutionResponse_name,
    describeStateMachineForExecutionResponse_definition,
    describeStateMachineForExecutionResponse_roleArn,
    describeStateMachineForExecutionResponse_updateDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newDescribeStateMachineForExecution' smart constructor.
data DescribeStateMachineForExecution = DescribeStateMachineForExecution'
  { -- | The Amazon Resource Name (ARN) of the execution you want state machine
    -- information for.
    executionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStateMachineForExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionArn', 'describeStateMachineForExecution_executionArn' - The Amazon Resource Name (ARN) of the execution you want state machine
-- information for.
newDescribeStateMachineForExecution ::
  -- | 'executionArn'
  Prelude.Text ->
  DescribeStateMachineForExecution
newDescribeStateMachineForExecution pExecutionArn_ =
  DescribeStateMachineForExecution'
    { executionArn =
        pExecutionArn_
    }

-- | The Amazon Resource Name (ARN) of the execution you want state machine
-- information for.
describeStateMachineForExecution_executionArn :: Lens.Lens' DescribeStateMachineForExecution Prelude.Text
describeStateMachineForExecution_executionArn = Lens.lens (\DescribeStateMachineForExecution' {executionArn} -> executionArn) (\s@DescribeStateMachineForExecution' {} a -> s {executionArn = a} :: DescribeStateMachineForExecution)

instance
  Core.AWSRequest
    DescribeStateMachineForExecution
  where
  type
    AWSResponse DescribeStateMachineForExecution =
      DescribeStateMachineForExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStateMachineForExecutionResponse'
            Prelude.<$> (x Data..?> "tracingConfiguration")
            Prelude.<*> (x Data..?> "loggingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "stateMachineArn")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "definition")
            Prelude.<*> (x Data..:> "roleArn")
            Prelude.<*> (x Data..:> "updateDate")
      )

instance
  Prelude.Hashable
    DescribeStateMachineForExecution
  where
  hashWithSalt
    _salt
    DescribeStateMachineForExecution' {..} =
      _salt `Prelude.hashWithSalt` executionArn

instance
  Prelude.NFData
    DescribeStateMachineForExecution
  where
  rnf DescribeStateMachineForExecution' {..} =
    Prelude.rnf executionArn

instance
  Data.ToHeaders
    DescribeStateMachineForExecution
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.DescribeStateMachineForExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStateMachineForExecution where
  toJSON DescribeStateMachineForExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("executionArn" Data..= executionArn)]
      )

instance Data.ToPath DescribeStateMachineForExecution where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeStateMachineForExecution
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStateMachineForExecutionResponse' smart constructor.
data DescribeStateMachineForExecutionResponse = DescribeStateMachineForExecutionResponse'
  { -- | Selects whether X-Ray tracing is enabled.
    tracingConfiguration :: Prelude.Maybe TracingConfiguration,
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the state machine associated with the
    -- execution.
    stateMachineArn :: Prelude.Text,
    -- | The name of the state machine associated with the execution.
    name :: Prelude.Text,
    -- | The Amazon States Language definition of the state machine. See
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
    definition :: Data.Sensitive Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role of the State Machine for
    -- the execution.
    roleArn :: Prelude.Text,
    -- | The date and time the state machine associated with an execution was
    -- updated. For a newly created state machine, this is the creation date.
    updateDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStateMachineForExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tracingConfiguration', 'describeStateMachineForExecutionResponse_tracingConfiguration' - Selects whether X-Ray tracing is enabled.
--
-- 'loggingConfiguration', 'describeStateMachineForExecutionResponse_loggingConfiguration' - Undocumented member.
--
-- 'httpStatus', 'describeStateMachineForExecutionResponse_httpStatus' - The response's http status code.
--
-- 'stateMachineArn', 'describeStateMachineForExecutionResponse_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine associated with the
-- execution.
--
-- 'name', 'describeStateMachineForExecutionResponse_name' - The name of the state machine associated with the execution.
--
-- 'definition', 'describeStateMachineForExecutionResponse_definition' - The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
--
-- 'roleArn', 'describeStateMachineForExecutionResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role of the State Machine for
-- the execution.
--
-- 'updateDate', 'describeStateMachineForExecutionResponse_updateDate' - The date and time the state machine associated with an execution was
-- updated. For a newly created state machine, this is the creation date.
newDescribeStateMachineForExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'stateMachineArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'definition'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'updateDate'
  Prelude.UTCTime ->
  DescribeStateMachineForExecutionResponse
newDescribeStateMachineForExecutionResponse
  pHttpStatus_
  pStateMachineArn_
  pName_
  pDefinition_
  pRoleArn_
  pUpdateDate_ =
    DescribeStateMachineForExecutionResponse'
      { tracingConfiguration =
          Prelude.Nothing,
        loggingConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        stateMachineArn =
          pStateMachineArn_,
        name = pName_,
        definition =
          Data._Sensitive
            Lens.# pDefinition_,
        roleArn = pRoleArn_,
        updateDate =
          Data._Time Lens.# pUpdateDate_
      }

-- | Selects whether X-Ray tracing is enabled.
describeStateMachineForExecutionResponse_tracingConfiguration :: Lens.Lens' DescribeStateMachineForExecutionResponse (Prelude.Maybe TracingConfiguration)
describeStateMachineForExecutionResponse_tracingConfiguration = Lens.lens (\DescribeStateMachineForExecutionResponse' {tracingConfiguration} -> tracingConfiguration) (\s@DescribeStateMachineForExecutionResponse' {} a -> s {tracingConfiguration = a} :: DescribeStateMachineForExecutionResponse)

-- | Undocumented member.
describeStateMachineForExecutionResponse_loggingConfiguration :: Lens.Lens' DescribeStateMachineForExecutionResponse (Prelude.Maybe LoggingConfiguration)
describeStateMachineForExecutionResponse_loggingConfiguration = Lens.lens (\DescribeStateMachineForExecutionResponse' {loggingConfiguration} -> loggingConfiguration) (\s@DescribeStateMachineForExecutionResponse' {} a -> s {loggingConfiguration = a} :: DescribeStateMachineForExecutionResponse)

-- | The response's http status code.
describeStateMachineForExecutionResponse_httpStatus :: Lens.Lens' DescribeStateMachineForExecutionResponse Prelude.Int
describeStateMachineForExecutionResponse_httpStatus = Lens.lens (\DescribeStateMachineForExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribeStateMachineForExecutionResponse' {} a -> s {httpStatus = a} :: DescribeStateMachineForExecutionResponse)

-- | The Amazon Resource Name (ARN) of the state machine associated with the
-- execution.
describeStateMachineForExecutionResponse_stateMachineArn :: Lens.Lens' DescribeStateMachineForExecutionResponse Prelude.Text
describeStateMachineForExecutionResponse_stateMachineArn = Lens.lens (\DescribeStateMachineForExecutionResponse' {stateMachineArn} -> stateMachineArn) (\s@DescribeStateMachineForExecutionResponse' {} a -> s {stateMachineArn = a} :: DescribeStateMachineForExecutionResponse)

-- | The name of the state machine associated with the execution.
describeStateMachineForExecutionResponse_name :: Lens.Lens' DescribeStateMachineForExecutionResponse Prelude.Text
describeStateMachineForExecutionResponse_name = Lens.lens (\DescribeStateMachineForExecutionResponse' {name} -> name) (\s@DescribeStateMachineForExecutionResponse' {} a -> s {name = a} :: DescribeStateMachineForExecutionResponse)

-- | The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
describeStateMachineForExecutionResponse_definition :: Lens.Lens' DescribeStateMachineForExecutionResponse Prelude.Text
describeStateMachineForExecutionResponse_definition = Lens.lens (\DescribeStateMachineForExecutionResponse' {definition} -> definition) (\s@DescribeStateMachineForExecutionResponse' {} a -> s {definition = a} :: DescribeStateMachineForExecutionResponse) Prelude.. Data._Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role of the State Machine for
-- the execution.
describeStateMachineForExecutionResponse_roleArn :: Lens.Lens' DescribeStateMachineForExecutionResponse Prelude.Text
describeStateMachineForExecutionResponse_roleArn = Lens.lens (\DescribeStateMachineForExecutionResponse' {roleArn} -> roleArn) (\s@DescribeStateMachineForExecutionResponse' {} a -> s {roleArn = a} :: DescribeStateMachineForExecutionResponse)

-- | The date and time the state machine associated with an execution was
-- updated. For a newly created state machine, this is the creation date.
describeStateMachineForExecutionResponse_updateDate :: Lens.Lens' DescribeStateMachineForExecutionResponse Prelude.UTCTime
describeStateMachineForExecutionResponse_updateDate = Lens.lens (\DescribeStateMachineForExecutionResponse' {updateDate} -> updateDate) (\s@DescribeStateMachineForExecutionResponse' {} a -> s {updateDate = a} :: DescribeStateMachineForExecutionResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    DescribeStateMachineForExecutionResponse
  where
  rnf DescribeStateMachineForExecutionResponse' {..} =
    Prelude.rnf tracingConfiguration
      `Prelude.seq` Prelude.rnf loggingConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stateMachineArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf updateDate
