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
-- Module      : Amazonka.StepFunctions.UpdateStateMachineAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing state machine
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-alias.html alias>
-- by modifying its @description@ or @routingConfiguration@.
--
-- You must specify at least one of the @description@ or
-- @routingConfiguration@ parameters to update a state machine alias.
--
-- @UpdateStateMachineAlias@ is an idempotent API. Step Functions bases the
-- idempotency check on the @stateMachineAliasArn@, @description@, and
-- @routingConfiguration@ parameters. Requests with the same parameters
-- return an idempotent response.
--
-- This operation is eventually consistent. All StartExecution requests
-- made within a few seconds use the latest alias configuration. Executions
-- started immediately after calling @UpdateStateMachineAlias@ may use the
-- previous routing configuration.
--
-- __Related operations:__
--
-- -   CreateStateMachineAlias
--
-- -   DescribeStateMachineAlias
--
-- -   ListStateMachineAliases
--
-- -   DeleteStateMachineAlias
module Amazonka.StepFunctions.UpdateStateMachineAlias
  ( -- * Creating a Request
    UpdateStateMachineAlias (..),
    newUpdateStateMachineAlias,

    -- * Request Lenses
    updateStateMachineAlias_description,
    updateStateMachineAlias_routingConfiguration,
    updateStateMachineAlias_stateMachineAliasArn,

    -- * Destructuring the Response
    UpdateStateMachineAliasResponse (..),
    newUpdateStateMachineAliasResponse,

    -- * Response Lenses
    updateStateMachineAliasResponse_httpStatus,
    updateStateMachineAliasResponse_updateDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newUpdateStateMachineAlias' smart constructor.
data UpdateStateMachineAlias = UpdateStateMachineAlias'
  { -- | A description of the state machine alias.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The routing configuration of the state machine alias.
    --
    -- An array of @RoutingConfig@ objects that specifies up to two state
    -- machine versions that the alias starts executions for.
    routingConfiguration :: Prelude.Maybe (Prelude.NonEmpty RoutingConfigurationListItem),
    -- | The Amazon Resource Name (ARN) of the state machine alias.
    stateMachineAliasArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStateMachineAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateStateMachineAlias_description' - A description of the state machine alias.
--
-- 'routingConfiguration', 'updateStateMachineAlias_routingConfiguration' - The routing configuration of the state machine alias.
--
-- An array of @RoutingConfig@ objects that specifies up to two state
-- machine versions that the alias starts executions for.
--
-- 'stateMachineAliasArn', 'updateStateMachineAlias_stateMachineAliasArn' - The Amazon Resource Name (ARN) of the state machine alias.
newUpdateStateMachineAlias ::
  -- | 'stateMachineAliasArn'
  Prelude.Text ->
  UpdateStateMachineAlias
newUpdateStateMachineAlias pStateMachineAliasArn_ =
  UpdateStateMachineAlias'
    { description =
        Prelude.Nothing,
      routingConfiguration = Prelude.Nothing,
      stateMachineAliasArn = pStateMachineAliasArn_
    }

-- | A description of the state machine alias.
updateStateMachineAlias_description :: Lens.Lens' UpdateStateMachineAlias (Prelude.Maybe Prelude.Text)
updateStateMachineAlias_description = Lens.lens (\UpdateStateMachineAlias' {description} -> description) (\s@UpdateStateMachineAlias' {} a -> s {description = a} :: UpdateStateMachineAlias) Prelude.. Lens.mapping Data._Sensitive

-- | The routing configuration of the state machine alias.
--
-- An array of @RoutingConfig@ objects that specifies up to two state
-- machine versions that the alias starts executions for.
updateStateMachineAlias_routingConfiguration :: Lens.Lens' UpdateStateMachineAlias (Prelude.Maybe (Prelude.NonEmpty RoutingConfigurationListItem))
updateStateMachineAlias_routingConfiguration = Lens.lens (\UpdateStateMachineAlias' {routingConfiguration} -> routingConfiguration) (\s@UpdateStateMachineAlias' {} a -> s {routingConfiguration = a} :: UpdateStateMachineAlias) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the state machine alias.
updateStateMachineAlias_stateMachineAliasArn :: Lens.Lens' UpdateStateMachineAlias Prelude.Text
updateStateMachineAlias_stateMachineAliasArn = Lens.lens (\UpdateStateMachineAlias' {stateMachineAliasArn} -> stateMachineAliasArn) (\s@UpdateStateMachineAlias' {} a -> s {stateMachineAliasArn = a} :: UpdateStateMachineAlias)

instance Core.AWSRequest UpdateStateMachineAlias where
  type
    AWSResponse UpdateStateMachineAlias =
      UpdateStateMachineAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStateMachineAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "updateDate")
      )

instance Prelude.Hashable UpdateStateMachineAlias where
  hashWithSalt _salt UpdateStateMachineAlias' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` routingConfiguration
      `Prelude.hashWithSalt` stateMachineAliasArn

instance Prelude.NFData UpdateStateMachineAlias where
  rnf UpdateStateMachineAlias' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf routingConfiguration
      `Prelude.seq` Prelude.rnf stateMachineAliasArn

instance Data.ToHeaders UpdateStateMachineAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.UpdateStateMachineAlias" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStateMachineAlias where
  toJSON UpdateStateMachineAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("routingConfiguration" Data..=)
              Prelude.<$> routingConfiguration,
            Prelude.Just
              ( "stateMachineAliasArn"
                  Data..= stateMachineAliasArn
              )
          ]
      )

instance Data.ToPath UpdateStateMachineAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStateMachineAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStateMachineAliasResponse' smart constructor.
data UpdateStateMachineAliasResponse = UpdateStateMachineAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The date and time the state machine alias was updated.
    updateDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStateMachineAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateStateMachineAliasResponse_httpStatus' - The response's http status code.
--
-- 'updateDate', 'updateStateMachineAliasResponse_updateDate' - The date and time the state machine alias was updated.
newUpdateStateMachineAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateDate'
  Prelude.UTCTime ->
  UpdateStateMachineAliasResponse
newUpdateStateMachineAliasResponse
  pHttpStatus_
  pUpdateDate_ =
    UpdateStateMachineAliasResponse'
      { httpStatus =
          pHttpStatus_,
        updateDate =
          Data._Time Lens.# pUpdateDate_
      }

-- | The response's http status code.
updateStateMachineAliasResponse_httpStatus :: Lens.Lens' UpdateStateMachineAliasResponse Prelude.Int
updateStateMachineAliasResponse_httpStatus = Lens.lens (\UpdateStateMachineAliasResponse' {httpStatus} -> httpStatus) (\s@UpdateStateMachineAliasResponse' {} a -> s {httpStatus = a} :: UpdateStateMachineAliasResponse)

-- | The date and time the state machine alias was updated.
updateStateMachineAliasResponse_updateDate :: Lens.Lens' UpdateStateMachineAliasResponse Prelude.UTCTime
updateStateMachineAliasResponse_updateDate = Lens.lens (\UpdateStateMachineAliasResponse' {updateDate} -> updateDate) (\s@UpdateStateMachineAliasResponse' {} a -> s {updateDate = a} :: UpdateStateMachineAliasResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    UpdateStateMachineAliasResponse
  where
  rnf UpdateStateMachineAliasResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateDate
