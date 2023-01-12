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
-- Module      : Amazonka.StepFunctions.UpdateStateMachine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing state machine by modifying its @definition@,
-- @roleArn@, or @loggingConfiguration@. Running executions will continue
-- to use the previous @definition@ and @roleArn@. You must include at
-- least one of @definition@ or @roleArn@ or you will receive a
-- @MissingRequiredParameter@ error.
--
-- If the given state machine Amazon Resource Name (ARN) is a qualified
-- state machine ARN, it will fail with ValidationException.
--
-- A qualified state machine ARN refers to a /Distributed Map state/
-- defined within a state machine. For example, the qualified state machine
-- ARN
-- @arn:partition:states:region:account-id:stateMachine:stateMachineName\/mapStateLabel@
-- refers to a /Distributed Map state/ with a label @mapStateLabel@ in the
-- state machine named @stateMachineName@.
--
-- All @StartExecution@ calls within a few seconds will use the updated
-- @definition@ and @roleArn@. Executions started immediately after calling
-- @UpdateStateMachine@ may use the previous state machine @definition@ and
-- @roleArn@.
module Amazonka.StepFunctions.UpdateStateMachine
  ( -- * Creating a Request
    UpdateStateMachine (..),
    newUpdateStateMachine,

    -- * Request Lenses
    updateStateMachine_definition,
    updateStateMachine_loggingConfiguration,
    updateStateMachine_roleArn,
    updateStateMachine_tracingConfiguration,
    updateStateMachine_stateMachineArn,

    -- * Destructuring the Response
    UpdateStateMachineResponse (..),
    newUpdateStateMachineResponse,

    -- * Response Lenses
    updateStateMachineResponse_httpStatus,
    updateStateMachineResponse_updateDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newUpdateStateMachine' smart constructor.
data UpdateStateMachine = UpdateStateMachine'
  { -- | The Amazon States Language definition of the state machine. See
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
    definition :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs
    -- options.
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The Amazon Resource Name (ARN) of the IAM role of the state machine.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Selects whether X-Ray tracing is enabled.
    tracingConfiguration :: Prelude.Maybe TracingConfiguration,
    -- | The Amazon Resource Name (ARN) of the state machine.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStateMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'updateStateMachine_definition' - The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
--
-- 'loggingConfiguration', 'updateStateMachine_loggingConfiguration' - The @LoggingConfiguration@ data type is used to set CloudWatch Logs
-- options.
--
-- 'roleArn', 'updateStateMachine_roleArn' - The Amazon Resource Name (ARN) of the IAM role of the state machine.
--
-- 'tracingConfiguration', 'updateStateMachine_tracingConfiguration' - Selects whether X-Ray tracing is enabled.
--
-- 'stateMachineArn', 'updateStateMachine_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine.
newUpdateStateMachine ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  UpdateStateMachine
newUpdateStateMachine pStateMachineArn_ =
  UpdateStateMachine'
    { definition = Prelude.Nothing,
      loggingConfiguration = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      tracingConfiguration = Prelude.Nothing,
      stateMachineArn = pStateMachineArn_
    }

-- | The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
updateStateMachine_definition :: Lens.Lens' UpdateStateMachine (Prelude.Maybe Prelude.Text)
updateStateMachine_definition = Lens.lens (\UpdateStateMachine' {definition} -> definition) (\s@UpdateStateMachine' {} a -> s {definition = a} :: UpdateStateMachine) Prelude.. Lens.mapping Data._Sensitive

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs
-- options.
updateStateMachine_loggingConfiguration :: Lens.Lens' UpdateStateMachine (Prelude.Maybe LoggingConfiguration)
updateStateMachine_loggingConfiguration = Lens.lens (\UpdateStateMachine' {loggingConfiguration} -> loggingConfiguration) (\s@UpdateStateMachine' {} a -> s {loggingConfiguration = a} :: UpdateStateMachine)

-- | The Amazon Resource Name (ARN) of the IAM role of the state machine.
updateStateMachine_roleArn :: Lens.Lens' UpdateStateMachine (Prelude.Maybe Prelude.Text)
updateStateMachine_roleArn = Lens.lens (\UpdateStateMachine' {roleArn} -> roleArn) (\s@UpdateStateMachine' {} a -> s {roleArn = a} :: UpdateStateMachine)

-- | Selects whether X-Ray tracing is enabled.
updateStateMachine_tracingConfiguration :: Lens.Lens' UpdateStateMachine (Prelude.Maybe TracingConfiguration)
updateStateMachine_tracingConfiguration = Lens.lens (\UpdateStateMachine' {tracingConfiguration} -> tracingConfiguration) (\s@UpdateStateMachine' {} a -> s {tracingConfiguration = a} :: UpdateStateMachine)

-- | The Amazon Resource Name (ARN) of the state machine.
updateStateMachine_stateMachineArn :: Lens.Lens' UpdateStateMachine Prelude.Text
updateStateMachine_stateMachineArn = Lens.lens (\UpdateStateMachine' {stateMachineArn} -> stateMachineArn) (\s@UpdateStateMachine' {} a -> s {stateMachineArn = a} :: UpdateStateMachine)

instance Core.AWSRequest UpdateStateMachine where
  type
    AWSResponse UpdateStateMachine =
      UpdateStateMachineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStateMachineResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "updateDate")
      )

instance Prelude.Hashable UpdateStateMachine where
  hashWithSalt _salt UpdateStateMachine' {..} =
    _salt `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` loggingConfiguration
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` tracingConfiguration
      `Prelude.hashWithSalt` stateMachineArn

instance Prelude.NFData UpdateStateMachine where
  rnf UpdateStateMachine' {..} =
    Prelude.rnf definition
      `Prelude.seq` Prelude.rnf loggingConfiguration
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf tracingConfiguration
      `Prelude.seq` Prelude.rnf stateMachineArn

instance Data.ToHeaders UpdateStateMachine where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.UpdateStateMachine" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStateMachine where
  toJSON UpdateStateMachine' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("definition" Data..=) Prelude.<$> definition,
            ("loggingConfiguration" Data..=)
              Prelude.<$> loggingConfiguration,
            ("roleArn" Data..=) Prelude.<$> roleArn,
            ("tracingConfiguration" Data..=)
              Prelude.<$> tracingConfiguration,
            Prelude.Just
              ("stateMachineArn" Data..= stateMachineArn)
          ]
      )

instance Data.ToPath UpdateStateMachine where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStateMachine where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStateMachineResponse' smart constructor.
data UpdateStateMachineResponse = UpdateStateMachineResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The date and time the state machine was updated.
    updateDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStateMachineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateStateMachineResponse_httpStatus' - The response's http status code.
--
-- 'updateDate', 'updateStateMachineResponse_updateDate' - The date and time the state machine was updated.
newUpdateStateMachineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateDate'
  Prelude.UTCTime ->
  UpdateStateMachineResponse
newUpdateStateMachineResponse
  pHttpStatus_
  pUpdateDate_ =
    UpdateStateMachineResponse'
      { httpStatus =
          pHttpStatus_,
        updateDate = Data._Time Lens.# pUpdateDate_
      }

-- | The response's http status code.
updateStateMachineResponse_httpStatus :: Lens.Lens' UpdateStateMachineResponse Prelude.Int
updateStateMachineResponse_httpStatus = Lens.lens (\UpdateStateMachineResponse' {httpStatus} -> httpStatus) (\s@UpdateStateMachineResponse' {} a -> s {httpStatus = a} :: UpdateStateMachineResponse)

-- | The date and time the state machine was updated.
updateStateMachineResponse_updateDate :: Lens.Lens' UpdateStateMachineResponse Prelude.UTCTime
updateStateMachineResponse_updateDate = Lens.lens (\UpdateStateMachineResponse' {updateDate} -> updateDate) (\s@UpdateStateMachineResponse' {} a -> s {updateDate = a} :: UpdateStateMachineResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateStateMachineResponse where
  rnf UpdateStateMachineResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateDate
