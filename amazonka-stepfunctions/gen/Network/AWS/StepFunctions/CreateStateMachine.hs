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
-- Module      : Network.AWS.StepFunctions.CreateStateMachine
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a state machine. A state machine consists of a collection of
-- states that can do work (@Task@ states), determine to which states to
-- transition next (@Choice@ states), stop an execution with an error
-- (@Fail@ states), and so on. State machines are specified using a
-- JSON-based, structured language. For more information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>
-- in the AWS Step Functions User Guide.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- @CreateStateMachine@ is an idempotent API. Subsequent requests won’t
-- create a duplicate resource if it was already created.
-- @CreateStateMachine@\'s idempotency check is based on the state machine
-- @name@, @definition@, @type@, @LoggingConfiguration@ and
-- @TracingConfiguration@. If a following request has a different @roleArn@
-- or @tags@, Step Functions will ignore these differences and treat it as
-- an idempotent request of the previous. In this case, @roleArn@ and
-- @tags@ will not be updated, even if they are different.
module Network.AWS.StepFunctions.CreateStateMachine
  ( -- * Creating a Request
    CreateStateMachine (..),
    newCreateStateMachine,

    -- * Request Lenses
    createStateMachine_tracingConfiguration,
    createStateMachine_tags,
    createStateMachine_loggingConfiguration,
    createStateMachine_type,
    createStateMachine_name,
    createStateMachine_definition,
    createStateMachine_roleArn,

    -- * Destructuring the Response
    CreateStateMachineResponse (..),
    newCreateStateMachineResponse,

    -- * Response Lenses
    createStateMachineResponse_httpStatus,
    createStateMachineResponse_stateMachineArn,
    createStateMachineResponse_creationDate,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newCreateStateMachine' smart constructor.
data CreateStateMachine = CreateStateMachine'
  { -- | Selects whether AWS X-Ray tracing is enabled.
    tracingConfiguration :: Core.Maybe TracingConfiguration,
    -- | Tags to be added when creating a state machine.
    --
    -- An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/, and
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
    --
    -- Tags may only contain Unicode letters, digits, white space, or these
    -- symbols: @_ . : \/ = + - \@@.
    tags :: Core.Maybe [Tag],
    -- | Defines what execution history events are logged and where they are
    -- logged.
    --
    -- By default, the @level@ is set to @OFF@. For more information see
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/cloudwatch-log-level.html Log Levels>
    -- in the AWS Step Functions User Guide.
    loggingConfiguration :: Core.Maybe LoggingConfiguration,
    -- | Determines whether a Standard or Express state machine is created. The
    -- default is @STANDARD@. You cannot update the @type@ of a state machine
    -- once it has been created.
    type' :: Core.Maybe StateMachineType,
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
    name :: Core.Text,
    -- | The Amazon States Language definition of the state machine. See
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
    definition :: Core.Sensitive Core.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to use for this state
    -- machine.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStateMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tracingConfiguration', 'createStateMachine_tracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
--
-- 'tags', 'createStateMachine_tags' - Tags to be added when creating a state machine.
--
-- An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/, and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
--
-- Tags may only contain Unicode letters, digits, white space, or these
-- symbols: @_ . : \/ = + - \@@.
--
-- 'loggingConfiguration', 'createStateMachine_loggingConfiguration' - Defines what execution history events are logged and where they are
-- logged.
--
-- By default, the @level@ is set to @OFF@. For more information see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/cloudwatch-log-level.html Log Levels>
-- in the AWS Step Functions User Guide.
--
-- 'type'', 'createStateMachine_type' - Determines whether a Standard or Express state machine is created. The
-- default is @STANDARD@. You cannot update the @type@ of a state machine
-- once it has been created.
--
-- 'name', 'createStateMachine_name' - The name of the state machine.
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
-- 'definition', 'createStateMachine_definition' - The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
--
-- 'roleArn', 'createStateMachine_roleArn' - The Amazon Resource Name (ARN) of the IAM role to use for this state
-- machine.
newCreateStateMachine ::
  -- | 'name'
  Core.Text ->
  -- | 'definition'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  CreateStateMachine
newCreateStateMachine pName_ pDefinition_ pRoleArn_ =
  CreateStateMachine'
    { tracingConfiguration =
        Core.Nothing,
      tags = Core.Nothing,
      loggingConfiguration = Core.Nothing,
      type' = Core.Nothing,
      name = pName_,
      definition = Core._Sensitive Lens.# pDefinition_,
      roleArn = pRoleArn_
    }

-- | Selects whether AWS X-Ray tracing is enabled.
createStateMachine_tracingConfiguration :: Lens.Lens' CreateStateMachine (Core.Maybe TracingConfiguration)
createStateMachine_tracingConfiguration = Lens.lens (\CreateStateMachine' {tracingConfiguration} -> tracingConfiguration) (\s@CreateStateMachine' {} a -> s {tracingConfiguration = a} :: CreateStateMachine)

-- | Tags to be added when creating a state machine.
--
-- An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/, and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
--
-- Tags may only contain Unicode letters, digits, white space, or these
-- symbols: @_ . : \/ = + - \@@.
createStateMachine_tags :: Lens.Lens' CreateStateMachine (Core.Maybe [Tag])
createStateMachine_tags = Lens.lens (\CreateStateMachine' {tags} -> tags) (\s@CreateStateMachine' {} a -> s {tags = a} :: CreateStateMachine) Core.. Lens.mapping Lens._Coerce

-- | Defines what execution history events are logged and where they are
-- logged.
--
-- By default, the @level@ is set to @OFF@. For more information see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/cloudwatch-log-level.html Log Levels>
-- in the AWS Step Functions User Guide.
createStateMachine_loggingConfiguration :: Lens.Lens' CreateStateMachine (Core.Maybe LoggingConfiguration)
createStateMachine_loggingConfiguration = Lens.lens (\CreateStateMachine' {loggingConfiguration} -> loggingConfiguration) (\s@CreateStateMachine' {} a -> s {loggingConfiguration = a} :: CreateStateMachine)

-- | Determines whether a Standard or Express state machine is created. The
-- default is @STANDARD@. You cannot update the @type@ of a state machine
-- once it has been created.
createStateMachine_type :: Lens.Lens' CreateStateMachine (Core.Maybe StateMachineType)
createStateMachine_type = Lens.lens (\CreateStateMachine' {type'} -> type') (\s@CreateStateMachine' {} a -> s {type' = a} :: CreateStateMachine)

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
createStateMachine_name :: Lens.Lens' CreateStateMachine Core.Text
createStateMachine_name = Lens.lens (\CreateStateMachine' {name} -> name) (\s@CreateStateMachine' {} a -> s {name = a} :: CreateStateMachine)

-- | The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
createStateMachine_definition :: Lens.Lens' CreateStateMachine Core.Text
createStateMachine_definition = Lens.lens (\CreateStateMachine' {definition} -> definition) (\s@CreateStateMachine' {} a -> s {definition = a} :: CreateStateMachine) Core.. Core._Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role to use for this state
-- machine.
createStateMachine_roleArn :: Lens.Lens' CreateStateMachine Core.Text
createStateMachine_roleArn = Lens.lens (\CreateStateMachine' {roleArn} -> roleArn) (\s@CreateStateMachine' {} a -> s {roleArn = a} :: CreateStateMachine)

instance Core.AWSRequest CreateStateMachine where
  type
    AWSResponse CreateStateMachine =
      CreateStateMachineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStateMachineResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "stateMachineArn")
            Core.<*> (x Core..:> "creationDate")
      )

instance Core.Hashable CreateStateMachine

instance Core.NFData CreateStateMachine

instance Core.ToHeaders CreateStateMachine where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.CreateStateMachine" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateStateMachine where
  toJSON CreateStateMachine' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tracingConfiguration" Core..=)
              Core.<$> tracingConfiguration,
            ("tags" Core..=) Core.<$> tags,
            ("loggingConfiguration" Core..=)
              Core.<$> loggingConfiguration,
            ("type" Core..=) Core.<$> type',
            Core.Just ("name" Core..= name),
            Core.Just ("definition" Core..= definition),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateStateMachine where
  toPath = Core.const "/"

instance Core.ToQuery CreateStateMachine where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateStateMachineResponse' smart constructor.
data CreateStateMachineResponse = CreateStateMachineResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) that identifies the created state
    -- machine.
    stateMachineArn :: Core.Text,
    -- | The date the state machine is created.
    creationDate :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStateMachineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createStateMachineResponse_httpStatus' - The response's http status code.
--
-- 'stateMachineArn', 'createStateMachineResponse_stateMachineArn' - The Amazon Resource Name (ARN) that identifies the created state
-- machine.
--
-- 'creationDate', 'createStateMachineResponse_creationDate' - The date the state machine is created.
newCreateStateMachineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'stateMachineArn'
  Core.Text ->
  -- | 'creationDate'
  Core.UTCTime ->
  CreateStateMachineResponse
newCreateStateMachineResponse
  pHttpStatus_
  pStateMachineArn_
  pCreationDate_ =
    CreateStateMachineResponse'
      { httpStatus =
          pHttpStatus_,
        stateMachineArn = pStateMachineArn_,
        creationDate = Core._Time Lens.# pCreationDate_
      }

-- | The response's http status code.
createStateMachineResponse_httpStatus :: Lens.Lens' CreateStateMachineResponse Core.Int
createStateMachineResponse_httpStatus = Lens.lens (\CreateStateMachineResponse' {httpStatus} -> httpStatus) (\s@CreateStateMachineResponse' {} a -> s {httpStatus = a} :: CreateStateMachineResponse)

-- | The Amazon Resource Name (ARN) that identifies the created state
-- machine.
createStateMachineResponse_stateMachineArn :: Lens.Lens' CreateStateMachineResponse Core.Text
createStateMachineResponse_stateMachineArn = Lens.lens (\CreateStateMachineResponse' {stateMachineArn} -> stateMachineArn) (\s@CreateStateMachineResponse' {} a -> s {stateMachineArn = a} :: CreateStateMachineResponse)

-- | The date the state machine is created.
createStateMachineResponse_creationDate :: Lens.Lens' CreateStateMachineResponse Core.UTCTime
createStateMachineResponse_creationDate = Lens.lens (\CreateStateMachineResponse' {creationDate} -> creationDate) (\s@CreateStateMachineResponse' {} a -> s {creationDate = a} :: CreateStateMachineResponse) Core.. Core._Time

instance Core.NFData CreateStateMachineResponse
