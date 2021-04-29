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
-- Module      : Network.AWS.StepFunctions.DescribeStateMachine
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a state machine.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
module Network.AWS.StepFunctions.DescribeStateMachine
  ( -- * Creating a Request
    DescribeStateMachine (..),
    newDescribeStateMachine,

    -- * Request Lenses
    describeStateMachine_stateMachineArn,

    -- * Destructuring the Response
    DescribeStateMachineResponse (..),
    newDescribeStateMachineResponse,

    -- * Response Lenses
    describeStateMachineResponse_status,
    describeStateMachineResponse_tracingConfiguration,
    describeStateMachineResponse_loggingConfiguration,
    describeStateMachineResponse_httpStatus,
    describeStateMachineResponse_stateMachineArn,
    describeStateMachineResponse_name,
    describeStateMachineResponse_definition,
    describeStateMachineResponse_roleArn,
    describeStateMachineResponse_type,
    describeStateMachineResponse_creationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newDescribeStateMachine' smart constructor.
data DescribeStateMachine = DescribeStateMachine'
  { -- | The Amazon Resource Name (ARN) of the state machine to describe.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeStateMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineArn', 'describeStateMachine_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine to describe.
newDescribeStateMachine ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  DescribeStateMachine
newDescribeStateMachine pStateMachineArn_ =
  DescribeStateMachine'
    { stateMachineArn =
        pStateMachineArn_
    }

-- | The Amazon Resource Name (ARN) of the state machine to describe.
describeStateMachine_stateMachineArn :: Lens.Lens' DescribeStateMachine Prelude.Text
describeStateMachine_stateMachineArn = Lens.lens (\DescribeStateMachine' {stateMachineArn} -> stateMachineArn) (\s@DescribeStateMachine' {} a -> s {stateMachineArn = a} :: DescribeStateMachine)

instance Prelude.AWSRequest DescribeStateMachine where
  type
    Rs DescribeStateMachine =
      DescribeStateMachineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStateMachineResponse'
            Prelude.<$> (x Prelude..?> "status")
            Prelude.<*> (x Prelude..?> "tracingConfiguration")
            Prelude.<*> (x Prelude..?> "loggingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "stateMachineArn")
            Prelude.<*> (x Prelude..:> "name")
            Prelude.<*> (x Prelude..:> "definition")
            Prelude.<*> (x Prelude..:> "roleArn")
            Prelude.<*> (x Prelude..:> "type")
            Prelude.<*> (x Prelude..:> "creationDate")
      )

instance Prelude.Hashable DescribeStateMachine

instance Prelude.NFData DescribeStateMachine

instance Prelude.ToHeaders DescribeStateMachine where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSStepFunctions.DescribeStateMachine" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeStateMachine where
  toJSON DescribeStateMachine' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("stateMachineArn" Prelude..= stateMachineArn)
          ]
      )

instance Prelude.ToPath DescribeStateMachine where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeStateMachine where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStateMachineResponse' smart constructor.
data DescribeStateMachineResponse = DescribeStateMachineResponse'
  { -- | The current status of the state machine.
    status :: Prelude.Maybe StateMachineStatus,
    -- | Selects whether AWS X-Ray tracing is enabled.
    tracingConfiguration :: Prelude.Maybe TracingConfiguration,
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies the state machine.
    stateMachineArn :: Prelude.Text,
    -- | The name of the state machine.
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
    name :: Prelude.Text,
    -- | The Amazon States Language definition of the state machine. See
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
    definition :: Prelude.Sensitive Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used when creating this
    -- state machine. (The IAM role maintains security by granting Step
    -- Functions access to AWS resources.)
    roleArn :: Prelude.Text,
    -- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@).
    type' :: StateMachineType,
    -- | The date the state machine is created.
    creationDate :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeStateMachineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeStateMachineResponse_status' - The current status of the state machine.
--
-- 'tracingConfiguration', 'describeStateMachineResponse_tracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
--
-- 'loggingConfiguration', 'describeStateMachineResponse_loggingConfiguration' - Undocumented member.
--
-- 'httpStatus', 'describeStateMachineResponse_httpStatus' - The response's http status code.
--
-- 'stateMachineArn', 'describeStateMachineResponse_stateMachineArn' - The Amazon Resource Name (ARN) that identifies the state machine.
--
-- 'name', 'describeStateMachineResponse_name' - The name of the state machine.
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
-- 'definition', 'describeStateMachineResponse_definition' - The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
--
-- 'roleArn', 'describeStateMachineResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role used when creating this
-- state machine. (The IAM role maintains security by granting Step
-- Functions access to AWS resources.)
--
-- 'type'', 'describeStateMachineResponse_type' - The @type@ of the state machine (@STANDARD@ or @EXPRESS@).
--
-- 'creationDate', 'describeStateMachineResponse_creationDate' - The date the state machine is created.
newDescribeStateMachineResponse ::
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
  -- | 'type''
  StateMachineType ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  DescribeStateMachineResponse
newDescribeStateMachineResponse
  pHttpStatus_
  pStateMachineArn_
  pName_
  pDefinition_
  pRoleArn_
  pType_
  pCreationDate_ =
    DescribeStateMachineResponse'
      { status =
          Prelude.Nothing,
        tracingConfiguration = Prelude.Nothing,
        loggingConfiguration = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        stateMachineArn = pStateMachineArn_,
        name = pName_,
        definition =
          Prelude._Sensitive Lens.# pDefinition_,
        roleArn = pRoleArn_,
        type' = pType_,
        creationDate =
          Prelude._Time Lens.# pCreationDate_
      }

-- | The current status of the state machine.
describeStateMachineResponse_status :: Lens.Lens' DescribeStateMachineResponse (Prelude.Maybe StateMachineStatus)
describeStateMachineResponse_status = Lens.lens (\DescribeStateMachineResponse' {status} -> status) (\s@DescribeStateMachineResponse' {} a -> s {status = a} :: DescribeStateMachineResponse)

-- | Selects whether AWS X-Ray tracing is enabled.
describeStateMachineResponse_tracingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Prelude.Maybe TracingConfiguration)
describeStateMachineResponse_tracingConfiguration = Lens.lens (\DescribeStateMachineResponse' {tracingConfiguration} -> tracingConfiguration) (\s@DescribeStateMachineResponse' {} a -> s {tracingConfiguration = a} :: DescribeStateMachineResponse)

-- | Undocumented member.
describeStateMachineResponse_loggingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Prelude.Maybe LoggingConfiguration)
describeStateMachineResponse_loggingConfiguration = Lens.lens (\DescribeStateMachineResponse' {loggingConfiguration} -> loggingConfiguration) (\s@DescribeStateMachineResponse' {} a -> s {loggingConfiguration = a} :: DescribeStateMachineResponse)

-- | The response's http status code.
describeStateMachineResponse_httpStatus :: Lens.Lens' DescribeStateMachineResponse Prelude.Int
describeStateMachineResponse_httpStatus = Lens.lens (\DescribeStateMachineResponse' {httpStatus} -> httpStatus) (\s@DescribeStateMachineResponse' {} a -> s {httpStatus = a} :: DescribeStateMachineResponse)

-- | The Amazon Resource Name (ARN) that identifies the state machine.
describeStateMachineResponse_stateMachineArn :: Lens.Lens' DescribeStateMachineResponse Prelude.Text
describeStateMachineResponse_stateMachineArn = Lens.lens (\DescribeStateMachineResponse' {stateMachineArn} -> stateMachineArn) (\s@DescribeStateMachineResponse' {} a -> s {stateMachineArn = a} :: DescribeStateMachineResponse)

-- | The name of the state machine.
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
describeStateMachineResponse_name :: Lens.Lens' DescribeStateMachineResponse Prelude.Text
describeStateMachineResponse_name = Lens.lens (\DescribeStateMachineResponse' {name} -> name) (\s@DescribeStateMachineResponse' {} a -> s {name = a} :: DescribeStateMachineResponse)

-- | The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
describeStateMachineResponse_definition :: Lens.Lens' DescribeStateMachineResponse Prelude.Text
describeStateMachineResponse_definition = Lens.lens (\DescribeStateMachineResponse' {definition} -> definition) (\s@DescribeStateMachineResponse' {} a -> s {definition = a} :: DescribeStateMachineResponse) Prelude.. Prelude._Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role used when creating this
-- state machine. (The IAM role maintains security by granting Step
-- Functions access to AWS resources.)
describeStateMachineResponse_roleArn :: Lens.Lens' DescribeStateMachineResponse Prelude.Text
describeStateMachineResponse_roleArn = Lens.lens (\DescribeStateMachineResponse' {roleArn} -> roleArn) (\s@DescribeStateMachineResponse' {} a -> s {roleArn = a} :: DescribeStateMachineResponse)

-- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@).
describeStateMachineResponse_type :: Lens.Lens' DescribeStateMachineResponse StateMachineType
describeStateMachineResponse_type = Lens.lens (\DescribeStateMachineResponse' {type'} -> type') (\s@DescribeStateMachineResponse' {} a -> s {type' = a} :: DescribeStateMachineResponse)

-- | The date the state machine is created.
describeStateMachineResponse_creationDate :: Lens.Lens' DescribeStateMachineResponse Prelude.UTCTime
describeStateMachineResponse_creationDate = Lens.lens (\DescribeStateMachineResponse' {creationDate} -> creationDate) (\s@DescribeStateMachineResponse' {} a -> s {creationDate = a} :: DescribeStateMachineResponse) Prelude.. Prelude._Time

instance Prelude.NFData DescribeStateMachineResponse
