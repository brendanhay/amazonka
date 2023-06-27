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
-- Module      : Amazonka.StepFunctions.DescribeStateMachine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a state machine\'s definition, its IAM role
-- Amazon Resource Name (ARN), and configuration.
--
-- A qualified state machine ARN can either refer to a /Distributed Map
-- state/ defined within a state machine, a version ARN, or an alias ARN.
--
-- The following are some examples of qualified and unqualified state
-- machine ARNs:
--
-- -   The following qualified state machine ARN refers to a /Distributed
--     Map state/ with a label @mapStateLabel@ in a state machine named
--     @myStateMachine@.
--
--     @arn:partition:states:region:account-id:stateMachine:myStateMachine\/mapStateLabel@
--
--     If you provide a qualified state machine ARN that refers to a
--     /Distributed Map state/, the request fails with
--     @ValidationException@.
--
-- -   The following qualified state machine ARN refers to an alias named
--     @PROD@.
--
--     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine:PROD>@
--
--     If you provide a qualified state machine ARN that refers to a
--     version ARN or an alias ARN, the request starts execution for that
--     version or alias.
--
-- -   The following unqualified state machine ARN refers to a state
--     machine named @myStateMachine@.
--
--     @arn:\<partition>:states:\<region>:\<account-id>:stateMachine:\<myStateMachine>@
--
-- This API action returns the details for a state machine version if the
-- @stateMachineArn@ you specify is a state machine version ARN.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
module Amazonka.StepFunctions.DescribeStateMachine
  ( -- * Creating a Request
    DescribeStateMachine (..),
    newDescribeStateMachine,

    -- * Request Lenses
    describeStateMachine_stateMachineArn,

    -- * Destructuring the Response
    DescribeStateMachineResponse (..),
    newDescribeStateMachineResponse,

    -- * Response Lenses
    describeStateMachineResponse_description,
    describeStateMachineResponse_label,
    describeStateMachineResponse_loggingConfiguration,
    describeStateMachineResponse_revisionId,
    describeStateMachineResponse_status,
    describeStateMachineResponse_tracingConfiguration,
    describeStateMachineResponse_httpStatus,
    describeStateMachineResponse_stateMachineArn,
    describeStateMachineResponse_name,
    describeStateMachineResponse_definition,
    describeStateMachineResponse_roleArn,
    describeStateMachineResponse_type,
    describeStateMachineResponse_creationDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newDescribeStateMachine' smart constructor.
data DescribeStateMachine = DescribeStateMachine'
  { -- | The Amazon Resource Name (ARN) of the state machine for which you want
    -- the information.
    --
    -- If you specify a state machine version ARN, this API returns details
    -- about that version. The version ARN is a combination of state machine
    -- ARN and the version number separated by a colon (:). For example,
    -- @stateMachineARN:1@.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStateMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineArn', 'describeStateMachine_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine for which you want
-- the information.
--
-- If you specify a state machine version ARN, this API returns details
-- about that version. The version ARN is a combination of state machine
-- ARN and the version number separated by a colon (:). For example,
-- @stateMachineARN:1@.
newDescribeStateMachine ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  DescribeStateMachine
newDescribeStateMachine pStateMachineArn_ =
  DescribeStateMachine'
    { stateMachineArn =
        pStateMachineArn_
    }

-- | The Amazon Resource Name (ARN) of the state machine for which you want
-- the information.
--
-- If you specify a state machine version ARN, this API returns details
-- about that version. The version ARN is a combination of state machine
-- ARN and the version number separated by a colon (:). For example,
-- @stateMachineARN:1@.
describeStateMachine_stateMachineArn :: Lens.Lens' DescribeStateMachine Prelude.Text
describeStateMachine_stateMachineArn = Lens.lens (\DescribeStateMachine' {stateMachineArn} -> stateMachineArn) (\s@DescribeStateMachine' {} a -> s {stateMachineArn = a} :: DescribeStateMachine)

instance Core.AWSRequest DescribeStateMachine where
  type
    AWSResponse DescribeStateMachine =
      DescribeStateMachineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStateMachineResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "label")
            Prelude.<*> (x Data..?> "loggingConfiguration")
            Prelude.<*> (x Data..?> "revisionId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "tracingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "stateMachineArn")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "definition")
            Prelude.<*> (x Data..:> "roleArn")
            Prelude.<*> (x Data..:> "type")
            Prelude.<*> (x Data..:> "creationDate")
      )

instance Prelude.Hashable DescribeStateMachine where
  hashWithSalt _salt DescribeStateMachine' {..} =
    _salt `Prelude.hashWithSalt` stateMachineArn

instance Prelude.NFData DescribeStateMachine where
  rnf DescribeStateMachine' {..} =
    Prelude.rnf stateMachineArn

instance Data.ToHeaders DescribeStateMachine where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.DescribeStateMachine" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStateMachine where
  toJSON DescribeStateMachine' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("stateMachineArn" Data..= stateMachineArn)
          ]
      )

instance Data.ToPath DescribeStateMachine where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStateMachine where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStateMachineResponse' smart constructor.
data DescribeStateMachineResponse = DescribeStateMachineResponse'
  { -- | The description of the state machine version.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A user-defined or an auto-generated string that identifies a @Map@
    -- state. This parameter is present only if the @stateMachineArn@ specified
    -- in input is a qualified state machine ARN.
    label :: Prelude.Maybe Prelude.Text,
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The revision identifier for the state machine.
    --
    -- Use the @revisionId@ parameter to compare between versions of a state
    -- machine configuration used for executions without performing a diff of
    -- the properties, such as @definition@ and @roleArn@.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the state machine.
    status :: Prelude.Maybe StateMachineStatus,
    -- | Selects whether X-Ray tracing is enabled.
    tracingConfiguration :: Prelude.Maybe TracingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies the state machine.
    --
    -- If you specified a state machine version ARN in your request, the API
    -- returns the version ARN. The version ARN is a combination of state
    -- machine ARN and the version number separated by a colon (:). For
    -- example, @stateMachineARN:1@.
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
    definition :: Data.Sensitive Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used when creating this
    -- state machine. (The IAM role maintains security by granting Step
    -- Functions access to Amazon Web Services resources.)
    roleArn :: Prelude.Text,
    -- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@).
    type' :: StateMachineType,
    -- | The date the state machine is created.
    --
    -- For a state machine version, @creationDate@ is the date the version was
    -- created.
    creationDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStateMachineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'describeStateMachineResponse_description' - The description of the state machine version.
--
-- 'label', 'describeStateMachineResponse_label' - A user-defined or an auto-generated string that identifies a @Map@
-- state. This parameter is present only if the @stateMachineArn@ specified
-- in input is a qualified state machine ARN.
--
-- 'loggingConfiguration', 'describeStateMachineResponse_loggingConfiguration' - Undocumented member.
--
-- 'revisionId', 'describeStateMachineResponse_revisionId' - The revision identifier for the state machine.
--
-- Use the @revisionId@ parameter to compare between versions of a state
-- machine configuration used for executions without performing a diff of
-- the properties, such as @definition@ and @roleArn@.
--
-- 'status', 'describeStateMachineResponse_status' - The current status of the state machine.
--
-- 'tracingConfiguration', 'describeStateMachineResponse_tracingConfiguration' - Selects whether X-Ray tracing is enabled.
--
-- 'httpStatus', 'describeStateMachineResponse_httpStatus' - The response's http status code.
--
-- 'stateMachineArn', 'describeStateMachineResponse_stateMachineArn' - The Amazon Resource Name (ARN) that identifies the state machine.
--
-- If you specified a state machine version ARN in your request, the API
-- returns the version ARN. The version ARN is a combination of state
-- machine ARN and the version number separated by a colon (:). For
-- example, @stateMachineARN:1@.
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
-- Functions access to Amazon Web Services resources.)
--
-- 'type'', 'describeStateMachineResponse_type' - The @type@ of the state machine (@STANDARD@ or @EXPRESS@).
--
-- 'creationDate', 'describeStateMachineResponse_creationDate' - The date the state machine is created.
--
-- For a state machine version, @creationDate@ is the date the version was
-- created.
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
      { description =
          Prelude.Nothing,
        label = Prelude.Nothing,
        loggingConfiguration = Prelude.Nothing,
        revisionId = Prelude.Nothing,
        status = Prelude.Nothing,
        tracingConfiguration = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        stateMachineArn = pStateMachineArn_,
        name = pName_,
        definition =
          Data._Sensitive Lens.# pDefinition_,
        roleArn = pRoleArn_,
        type' = pType_,
        creationDate =
          Data._Time Lens.# pCreationDate_
      }

-- | The description of the state machine version.
describeStateMachineResponse_description :: Lens.Lens' DescribeStateMachineResponse (Prelude.Maybe Prelude.Text)
describeStateMachineResponse_description = Lens.lens (\DescribeStateMachineResponse' {description} -> description) (\s@DescribeStateMachineResponse' {} a -> s {description = a} :: DescribeStateMachineResponse) Prelude.. Lens.mapping Data._Sensitive

-- | A user-defined or an auto-generated string that identifies a @Map@
-- state. This parameter is present only if the @stateMachineArn@ specified
-- in input is a qualified state machine ARN.
describeStateMachineResponse_label :: Lens.Lens' DescribeStateMachineResponse (Prelude.Maybe Prelude.Text)
describeStateMachineResponse_label = Lens.lens (\DescribeStateMachineResponse' {label} -> label) (\s@DescribeStateMachineResponse' {} a -> s {label = a} :: DescribeStateMachineResponse)

-- | Undocumented member.
describeStateMachineResponse_loggingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Prelude.Maybe LoggingConfiguration)
describeStateMachineResponse_loggingConfiguration = Lens.lens (\DescribeStateMachineResponse' {loggingConfiguration} -> loggingConfiguration) (\s@DescribeStateMachineResponse' {} a -> s {loggingConfiguration = a} :: DescribeStateMachineResponse)

-- | The revision identifier for the state machine.
--
-- Use the @revisionId@ parameter to compare between versions of a state
-- machine configuration used for executions without performing a diff of
-- the properties, such as @definition@ and @roleArn@.
describeStateMachineResponse_revisionId :: Lens.Lens' DescribeStateMachineResponse (Prelude.Maybe Prelude.Text)
describeStateMachineResponse_revisionId = Lens.lens (\DescribeStateMachineResponse' {revisionId} -> revisionId) (\s@DescribeStateMachineResponse' {} a -> s {revisionId = a} :: DescribeStateMachineResponse)

-- | The current status of the state machine.
describeStateMachineResponse_status :: Lens.Lens' DescribeStateMachineResponse (Prelude.Maybe StateMachineStatus)
describeStateMachineResponse_status = Lens.lens (\DescribeStateMachineResponse' {status} -> status) (\s@DescribeStateMachineResponse' {} a -> s {status = a} :: DescribeStateMachineResponse)

-- | Selects whether X-Ray tracing is enabled.
describeStateMachineResponse_tracingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Prelude.Maybe TracingConfiguration)
describeStateMachineResponse_tracingConfiguration = Lens.lens (\DescribeStateMachineResponse' {tracingConfiguration} -> tracingConfiguration) (\s@DescribeStateMachineResponse' {} a -> s {tracingConfiguration = a} :: DescribeStateMachineResponse)

-- | The response's http status code.
describeStateMachineResponse_httpStatus :: Lens.Lens' DescribeStateMachineResponse Prelude.Int
describeStateMachineResponse_httpStatus = Lens.lens (\DescribeStateMachineResponse' {httpStatus} -> httpStatus) (\s@DescribeStateMachineResponse' {} a -> s {httpStatus = a} :: DescribeStateMachineResponse)

-- | The Amazon Resource Name (ARN) that identifies the state machine.
--
-- If you specified a state machine version ARN in your request, the API
-- returns the version ARN. The version ARN is a combination of state
-- machine ARN and the version number separated by a colon (:). For
-- example, @stateMachineARN:1@.
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
describeStateMachineResponse_definition = Lens.lens (\DescribeStateMachineResponse' {definition} -> definition) (\s@DescribeStateMachineResponse' {} a -> s {definition = a} :: DescribeStateMachineResponse) Prelude.. Data._Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role used when creating this
-- state machine. (The IAM role maintains security by granting Step
-- Functions access to Amazon Web Services resources.)
describeStateMachineResponse_roleArn :: Lens.Lens' DescribeStateMachineResponse Prelude.Text
describeStateMachineResponse_roleArn = Lens.lens (\DescribeStateMachineResponse' {roleArn} -> roleArn) (\s@DescribeStateMachineResponse' {} a -> s {roleArn = a} :: DescribeStateMachineResponse)

-- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@).
describeStateMachineResponse_type :: Lens.Lens' DescribeStateMachineResponse StateMachineType
describeStateMachineResponse_type = Lens.lens (\DescribeStateMachineResponse' {type'} -> type') (\s@DescribeStateMachineResponse' {} a -> s {type' = a} :: DescribeStateMachineResponse)

-- | The date the state machine is created.
--
-- For a state machine version, @creationDate@ is the date the version was
-- created.
describeStateMachineResponse_creationDate :: Lens.Lens' DescribeStateMachineResponse Prelude.UTCTime
describeStateMachineResponse_creationDate = Lens.lens (\DescribeStateMachineResponse' {creationDate} -> creationDate) (\s@DescribeStateMachineResponse' {} a -> s {creationDate = a} :: DescribeStateMachineResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeStateMachineResponse where
  rnf DescribeStateMachineResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf loggingConfiguration
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tracingConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stateMachineArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf creationDate
