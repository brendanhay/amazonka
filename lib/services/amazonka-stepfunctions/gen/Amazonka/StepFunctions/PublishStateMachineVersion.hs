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
-- Module      : Amazonka.StepFunctions.PublishStateMachineVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-version.html version>
-- from the current revision of a state machine. Use versions to create
-- immutable snapshots of your state machine. You can start executions from
-- versions either directly or with an alias. To create an alias, use
-- CreateStateMachineAlias.
--
-- You can publish up to 1000 versions for each state machine. You must
-- manually delete unused versions using the DeleteStateMachineVersion API
-- action.
--
-- @PublishStateMachineVersion@ is an idempotent API. It doesn\'t create a
-- duplicate state machine version if it already exists for the current
-- revision. Step Functions bases @PublishStateMachineVersion@\'s
-- idempotency check on the @stateMachineArn@, @name@, and @revisionId@
-- parameters. Requests with the same parameters return a successful
-- idempotent response. If you don\'t specify a @revisionId@, Step
-- Functions checks for a previously published version of the state
-- machine\'s current revision.
--
-- __Related operations:__
--
-- -   DeleteStateMachineVersion
--
-- -   ListStateMachineVersions
module Amazonka.StepFunctions.PublishStateMachineVersion
  ( -- * Creating a Request
    PublishStateMachineVersion (..),
    newPublishStateMachineVersion,

    -- * Request Lenses
    publishStateMachineVersion_description,
    publishStateMachineVersion_revisionId,
    publishStateMachineVersion_stateMachineArn,

    -- * Destructuring the Response
    PublishStateMachineVersionResponse (..),
    newPublishStateMachineVersionResponse,

    -- * Response Lenses
    publishStateMachineVersionResponse_httpStatus,
    publishStateMachineVersionResponse_creationDate,
    publishStateMachineVersionResponse_stateMachineVersionArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newPublishStateMachineVersion' smart constructor.
data PublishStateMachineVersion = PublishStateMachineVersion'
  { -- | An optional description of the state machine version.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Only publish the state machine version if the current state machine\'s
    -- revision ID matches the specified ID.
    --
    -- Use this option to avoid publishing a version if the state machine
    -- changed since you last updated it. If the specified revision ID doesn\'t
    -- match the state machine\'s current revision ID, the API returns
    -- @ConflictException@.
    --
    -- To specify an initial revision ID for a state machine with no revision
    -- ID assigned, specify the string @INITIAL@ for the @revisionId@
    -- parameter. For example, you can specify a @revisionID@ of @INITIAL@ when
    -- you create a state machine using the CreateStateMachine API action.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the state machine.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishStateMachineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'publishStateMachineVersion_description' - An optional description of the state machine version.
--
-- 'revisionId', 'publishStateMachineVersion_revisionId' - Only publish the state machine version if the current state machine\'s
-- revision ID matches the specified ID.
--
-- Use this option to avoid publishing a version if the state machine
-- changed since you last updated it. If the specified revision ID doesn\'t
-- match the state machine\'s current revision ID, the API returns
-- @ConflictException@.
--
-- To specify an initial revision ID for a state machine with no revision
-- ID assigned, specify the string @INITIAL@ for the @revisionId@
-- parameter. For example, you can specify a @revisionID@ of @INITIAL@ when
-- you create a state machine using the CreateStateMachine API action.
--
-- 'stateMachineArn', 'publishStateMachineVersion_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine.
newPublishStateMachineVersion ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  PublishStateMachineVersion
newPublishStateMachineVersion pStateMachineArn_ =
  PublishStateMachineVersion'
    { description =
        Prelude.Nothing,
      revisionId = Prelude.Nothing,
      stateMachineArn = pStateMachineArn_
    }

-- | An optional description of the state machine version.
publishStateMachineVersion_description :: Lens.Lens' PublishStateMachineVersion (Prelude.Maybe Prelude.Text)
publishStateMachineVersion_description = Lens.lens (\PublishStateMachineVersion' {description} -> description) (\s@PublishStateMachineVersion' {} a -> s {description = a} :: PublishStateMachineVersion) Prelude.. Lens.mapping Data._Sensitive

-- | Only publish the state machine version if the current state machine\'s
-- revision ID matches the specified ID.
--
-- Use this option to avoid publishing a version if the state machine
-- changed since you last updated it. If the specified revision ID doesn\'t
-- match the state machine\'s current revision ID, the API returns
-- @ConflictException@.
--
-- To specify an initial revision ID for a state machine with no revision
-- ID assigned, specify the string @INITIAL@ for the @revisionId@
-- parameter. For example, you can specify a @revisionID@ of @INITIAL@ when
-- you create a state machine using the CreateStateMachine API action.
publishStateMachineVersion_revisionId :: Lens.Lens' PublishStateMachineVersion (Prelude.Maybe Prelude.Text)
publishStateMachineVersion_revisionId = Lens.lens (\PublishStateMachineVersion' {revisionId} -> revisionId) (\s@PublishStateMachineVersion' {} a -> s {revisionId = a} :: PublishStateMachineVersion)

-- | The Amazon Resource Name (ARN) of the state machine.
publishStateMachineVersion_stateMachineArn :: Lens.Lens' PublishStateMachineVersion Prelude.Text
publishStateMachineVersion_stateMachineArn = Lens.lens (\PublishStateMachineVersion' {stateMachineArn} -> stateMachineArn) (\s@PublishStateMachineVersion' {} a -> s {stateMachineArn = a} :: PublishStateMachineVersion)

instance Core.AWSRequest PublishStateMachineVersion where
  type
    AWSResponse PublishStateMachineVersion =
      PublishStateMachineVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PublishStateMachineVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "creationDate")
            Prelude.<*> (x Data..:> "stateMachineVersionArn")
      )

instance Prelude.Hashable PublishStateMachineVersion where
  hashWithSalt _salt PublishStateMachineVersion' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` stateMachineArn

instance Prelude.NFData PublishStateMachineVersion where
  rnf PublishStateMachineVersion' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf stateMachineArn

instance Data.ToHeaders PublishStateMachineVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.PublishStateMachineVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PublishStateMachineVersion where
  toJSON PublishStateMachineVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("revisionId" Data..=) Prelude.<$> revisionId,
            Prelude.Just
              ("stateMachineArn" Data..= stateMachineArn)
          ]
      )

instance Data.ToPath PublishStateMachineVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery PublishStateMachineVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPublishStateMachineVersionResponse' smart constructor.
data PublishStateMachineVersionResponse = PublishStateMachineVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The date the version was created.
    creationDate :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) (ARN) that identifies the state machine
    -- version.
    stateMachineVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishStateMachineVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'publishStateMachineVersionResponse_httpStatus' - The response's http status code.
--
-- 'creationDate', 'publishStateMachineVersionResponse_creationDate' - The date the version was created.
--
-- 'stateMachineVersionArn', 'publishStateMachineVersionResponse_stateMachineVersionArn' - The Amazon Resource Name (ARN) (ARN) that identifies the state machine
-- version.
newPublishStateMachineVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  -- | 'stateMachineVersionArn'
  Prelude.Text ->
  PublishStateMachineVersionResponse
newPublishStateMachineVersionResponse
  pHttpStatus_
  pCreationDate_
  pStateMachineVersionArn_ =
    PublishStateMachineVersionResponse'
      { httpStatus =
          pHttpStatus_,
        creationDate =
          Data._Time Lens.# pCreationDate_,
        stateMachineVersionArn =
          pStateMachineVersionArn_
      }

-- | The response's http status code.
publishStateMachineVersionResponse_httpStatus :: Lens.Lens' PublishStateMachineVersionResponse Prelude.Int
publishStateMachineVersionResponse_httpStatus = Lens.lens (\PublishStateMachineVersionResponse' {httpStatus} -> httpStatus) (\s@PublishStateMachineVersionResponse' {} a -> s {httpStatus = a} :: PublishStateMachineVersionResponse)

-- | The date the version was created.
publishStateMachineVersionResponse_creationDate :: Lens.Lens' PublishStateMachineVersionResponse Prelude.UTCTime
publishStateMachineVersionResponse_creationDate = Lens.lens (\PublishStateMachineVersionResponse' {creationDate} -> creationDate) (\s@PublishStateMachineVersionResponse' {} a -> s {creationDate = a} :: PublishStateMachineVersionResponse) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) (ARN) that identifies the state machine
-- version.
publishStateMachineVersionResponse_stateMachineVersionArn :: Lens.Lens' PublishStateMachineVersionResponse Prelude.Text
publishStateMachineVersionResponse_stateMachineVersionArn = Lens.lens (\PublishStateMachineVersionResponse' {stateMachineVersionArn} -> stateMachineVersionArn) (\s@PublishStateMachineVersionResponse' {} a -> s {stateMachineVersionArn = a} :: PublishStateMachineVersionResponse)

instance
  Prelude.NFData
    PublishStateMachineVersionResponse
  where
  rnf PublishStateMachineVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf stateMachineVersionArn
