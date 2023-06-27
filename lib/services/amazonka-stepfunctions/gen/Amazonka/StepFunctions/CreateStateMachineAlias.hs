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
-- Module      : Amazonka.StepFunctions.CreateStateMachineAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-alias.html alias>
-- for a state machine that points to one or two
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-version.html versions>
-- of the same state machine. You can set your application to call
-- StartExecution with an alias and update the version the alias uses
-- without changing the client\'s code.
--
-- You can also map an alias to split StartExecution requests between two
-- versions of a state machine. To do this, add a second @RoutingConfig@
-- object in the @routingConfiguration@ parameter. You must also specify
-- the percentage of execution run requests each version should receive in
-- both @RoutingConfig@ objects. Step Functions randomly chooses which
-- version runs a given execution based on the percentage you specify.
--
-- To create an alias that points to a single version, specify a single
-- @RoutingConfig@ object with a @weight@ set to 100.
--
-- You can create up to 100 aliases for each state machine. You must delete
-- unused aliases using the DeleteStateMachineAlias API action.
--
-- @CreateStateMachineAlias@ is an idempotent API. Step Functions bases the
-- idempotency check on the @stateMachineArn@, @description@, @name@, and
-- @routingConfiguration@ parameters. Requests that contain the same values
-- for these parameters return a successful idempotent response without
-- creating a duplicate resource.
--
-- __Related operations:__
--
-- -   DescribeStateMachineAlias
--
-- -   ListStateMachineAliases
--
-- -   UpdateStateMachineAlias
--
-- -   DeleteStateMachineAlias
module Amazonka.StepFunctions.CreateStateMachineAlias
  ( -- * Creating a Request
    CreateStateMachineAlias (..),
    newCreateStateMachineAlias,

    -- * Request Lenses
    createStateMachineAlias_description,
    createStateMachineAlias_name,
    createStateMachineAlias_routingConfiguration,

    -- * Destructuring the Response
    CreateStateMachineAliasResponse (..),
    newCreateStateMachineAliasResponse,

    -- * Response Lenses
    createStateMachineAliasResponse_httpStatus,
    createStateMachineAliasResponse_stateMachineAliasArn,
    createStateMachineAliasResponse_creationDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newCreateStateMachineAlias' smart constructor.
data CreateStateMachineAlias = CreateStateMachineAlias'
  { -- | A description for the state machine alias.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the state machine alias.
    --
    -- To avoid conflict with version ARNs, don\'t use an integer in the name
    -- of the alias.
    name :: Prelude.Text,
    -- | The routing configuration of a state machine alias. The routing
    -- configuration shifts execution traffic between two state machine
    -- versions. @routingConfiguration@ contains an array of @RoutingConfig@
    -- objects that specify up to two state machine versions. Step Functions
    -- then randomly choses which version to run an execution with based on the
    -- weight assigned to each @RoutingConfig@.
    routingConfiguration :: Prelude.NonEmpty RoutingConfigurationListItem
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStateMachineAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createStateMachineAlias_description' - A description for the state machine alias.
--
-- 'name', 'createStateMachineAlias_name' - The name of the state machine alias.
--
-- To avoid conflict with version ARNs, don\'t use an integer in the name
-- of the alias.
--
-- 'routingConfiguration', 'createStateMachineAlias_routingConfiguration' - The routing configuration of a state machine alias. The routing
-- configuration shifts execution traffic between two state machine
-- versions. @routingConfiguration@ contains an array of @RoutingConfig@
-- objects that specify up to two state machine versions. Step Functions
-- then randomly choses which version to run an execution with based on the
-- weight assigned to each @RoutingConfig@.
newCreateStateMachineAlias ::
  -- | 'name'
  Prelude.Text ->
  -- | 'routingConfiguration'
  Prelude.NonEmpty RoutingConfigurationListItem ->
  CreateStateMachineAlias
newCreateStateMachineAlias
  pName_
  pRoutingConfiguration_ =
    CreateStateMachineAlias'
      { description =
          Prelude.Nothing,
        name = pName_,
        routingConfiguration =
          Lens.coerced Lens.# pRoutingConfiguration_
      }

-- | A description for the state machine alias.
createStateMachineAlias_description :: Lens.Lens' CreateStateMachineAlias (Prelude.Maybe Prelude.Text)
createStateMachineAlias_description = Lens.lens (\CreateStateMachineAlias' {description} -> description) (\s@CreateStateMachineAlias' {} a -> s {description = a} :: CreateStateMachineAlias) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the state machine alias.
--
-- To avoid conflict with version ARNs, don\'t use an integer in the name
-- of the alias.
createStateMachineAlias_name :: Lens.Lens' CreateStateMachineAlias Prelude.Text
createStateMachineAlias_name = Lens.lens (\CreateStateMachineAlias' {name} -> name) (\s@CreateStateMachineAlias' {} a -> s {name = a} :: CreateStateMachineAlias)

-- | The routing configuration of a state machine alias. The routing
-- configuration shifts execution traffic between two state machine
-- versions. @routingConfiguration@ contains an array of @RoutingConfig@
-- objects that specify up to two state machine versions. Step Functions
-- then randomly choses which version to run an execution with based on the
-- weight assigned to each @RoutingConfig@.
createStateMachineAlias_routingConfiguration :: Lens.Lens' CreateStateMachineAlias (Prelude.NonEmpty RoutingConfigurationListItem)
createStateMachineAlias_routingConfiguration = Lens.lens (\CreateStateMachineAlias' {routingConfiguration} -> routingConfiguration) (\s@CreateStateMachineAlias' {} a -> s {routingConfiguration = a} :: CreateStateMachineAlias) Prelude.. Lens.coerced

instance Core.AWSRequest CreateStateMachineAlias where
  type
    AWSResponse CreateStateMachineAlias =
      CreateStateMachineAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStateMachineAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "stateMachineAliasArn")
            Prelude.<*> (x Data..:> "creationDate")
      )

instance Prelude.Hashable CreateStateMachineAlias where
  hashWithSalt _salt CreateStateMachineAlias' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` routingConfiguration

instance Prelude.NFData CreateStateMachineAlias where
  rnf CreateStateMachineAlias' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf routingConfiguration

instance Data.ToHeaders CreateStateMachineAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.CreateStateMachineAlias" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStateMachineAlias where
  toJSON CreateStateMachineAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ( "routingConfiguration"
                  Data..= routingConfiguration
              )
          ]
      )

instance Data.ToPath CreateStateMachineAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateStateMachineAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStateMachineAliasResponse' smart constructor.
data CreateStateMachineAliasResponse = CreateStateMachineAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies the created state machine
    -- alias.
    stateMachineAliasArn :: Prelude.Text,
    -- | The date the state machine alias was created.
    creationDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStateMachineAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createStateMachineAliasResponse_httpStatus' - The response's http status code.
--
-- 'stateMachineAliasArn', 'createStateMachineAliasResponse_stateMachineAliasArn' - The Amazon Resource Name (ARN) that identifies the created state machine
-- alias.
--
-- 'creationDate', 'createStateMachineAliasResponse_creationDate' - The date the state machine alias was created.
newCreateStateMachineAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'stateMachineAliasArn'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  CreateStateMachineAliasResponse
newCreateStateMachineAliasResponse
  pHttpStatus_
  pStateMachineAliasArn_
  pCreationDate_ =
    CreateStateMachineAliasResponse'
      { httpStatus =
          pHttpStatus_,
        stateMachineAliasArn =
          pStateMachineAliasArn_,
        creationDate =
          Data._Time Lens.# pCreationDate_
      }

-- | The response's http status code.
createStateMachineAliasResponse_httpStatus :: Lens.Lens' CreateStateMachineAliasResponse Prelude.Int
createStateMachineAliasResponse_httpStatus = Lens.lens (\CreateStateMachineAliasResponse' {httpStatus} -> httpStatus) (\s@CreateStateMachineAliasResponse' {} a -> s {httpStatus = a} :: CreateStateMachineAliasResponse)

-- | The Amazon Resource Name (ARN) that identifies the created state machine
-- alias.
createStateMachineAliasResponse_stateMachineAliasArn :: Lens.Lens' CreateStateMachineAliasResponse Prelude.Text
createStateMachineAliasResponse_stateMachineAliasArn = Lens.lens (\CreateStateMachineAliasResponse' {stateMachineAliasArn} -> stateMachineAliasArn) (\s@CreateStateMachineAliasResponse' {} a -> s {stateMachineAliasArn = a} :: CreateStateMachineAliasResponse)

-- | The date the state machine alias was created.
createStateMachineAliasResponse_creationDate :: Lens.Lens' CreateStateMachineAliasResponse Prelude.UTCTime
createStateMachineAliasResponse_creationDate = Lens.lens (\CreateStateMachineAliasResponse' {creationDate} -> creationDate) (\s@CreateStateMachineAliasResponse' {} a -> s {creationDate = a} :: CreateStateMachineAliasResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    CreateStateMachineAliasResponse
  where
  rnf CreateStateMachineAliasResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stateMachineAliasArn
      `Prelude.seq` Prelude.rnf creationDate
