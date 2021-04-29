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
-- Module      : Network.AWS.StepFunctions.UpdateStateMachine
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- All @StartExecution@ calls within a few seconds will use the updated
-- @definition@ and @roleArn@. Executions started immediately after calling
-- @UpdateStateMachine@ may use the previous state machine @definition@ and
-- @roleArn@.
module Network.AWS.StepFunctions.UpdateStateMachine
  ( -- * Creating a Request
    UpdateStateMachine (..),
    newUpdateStateMachine,

    -- * Request Lenses
    updateStateMachine_roleArn,
    updateStateMachine_tracingConfiguration,
    updateStateMachine_loggingConfiguration,
    updateStateMachine_definition,
    updateStateMachine_stateMachineArn,

    -- * Destructuring the Response
    UpdateStateMachineResponse (..),
    newUpdateStateMachineResponse,

    -- * Response Lenses
    updateStateMachineResponse_httpStatus,
    updateStateMachineResponse_updateDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newUpdateStateMachine' smart constructor.
data UpdateStateMachine = UpdateStateMachine'
  { -- | The Amazon Resource Name (ARN) of the IAM role of the state machine.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Selects whether AWS X-Ray tracing is enabled.
    tracingConfiguration :: Prelude.Maybe TracingConfiguration,
    -- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs
    -- options.
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The Amazon States Language definition of the state machine. See
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
    definition :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the state machine.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateStateMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateStateMachine_roleArn' - The Amazon Resource Name (ARN) of the IAM role of the state machine.
--
-- 'tracingConfiguration', 'updateStateMachine_tracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
--
-- 'loggingConfiguration', 'updateStateMachine_loggingConfiguration' - The @LoggingConfiguration@ data type is used to set CloudWatch Logs
-- options.
--
-- 'definition', 'updateStateMachine_definition' - The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
--
-- 'stateMachineArn', 'updateStateMachine_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine.
newUpdateStateMachine ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  UpdateStateMachine
newUpdateStateMachine pStateMachineArn_ =
  UpdateStateMachine'
    { roleArn = Prelude.Nothing,
      tracingConfiguration = Prelude.Nothing,
      loggingConfiguration = Prelude.Nothing,
      definition = Prelude.Nothing,
      stateMachineArn = pStateMachineArn_
    }

-- | The Amazon Resource Name (ARN) of the IAM role of the state machine.
updateStateMachine_roleArn :: Lens.Lens' UpdateStateMachine (Prelude.Maybe Prelude.Text)
updateStateMachine_roleArn = Lens.lens (\UpdateStateMachine' {roleArn} -> roleArn) (\s@UpdateStateMachine' {} a -> s {roleArn = a} :: UpdateStateMachine)

-- | Selects whether AWS X-Ray tracing is enabled.
updateStateMachine_tracingConfiguration :: Lens.Lens' UpdateStateMachine (Prelude.Maybe TracingConfiguration)
updateStateMachine_tracingConfiguration = Lens.lens (\UpdateStateMachine' {tracingConfiguration} -> tracingConfiguration) (\s@UpdateStateMachine' {} a -> s {tracingConfiguration = a} :: UpdateStateMachine)

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs
-- options.
updateStateMachine_loggingConfiguration :: Lens.Lens' UpdateStateMachine (Prelude.Maybe LoggingConfiguration)
updateStateMachine_loggingConfiguration = Lens.lens (\UpdateStateMachine' {loggingConfiguration} -> loggingConfiguration) (\s@UpdateStateMachine' {} a -> s {loggingConfiguration = a} :: UpdateStateMachine)

-- | The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
updateStateMachine_definition :: Lens.Lens' UpdateStateMachine (Prelude.Maybe Prelude.Text)
updateStateMachine_definition = Lens.lens (\UpdateStateMachine' {definition} -> definition) (\s@UpdateStateMachine' {} a -> s {definition = a} :: UpdateStateMachine) Prelude.. Lens.mapping Prelude._Sensitive

-- | The Amazon Resource Name (ARN) of the state machine.
updateStateMachine_stateMachineArn :: Lens.Lens' UpdateStateMachine Prelude.Text
updateStateMachine_stateMachineArn = Lens.lens (\UpdateStateMachine' {stateMachineArn} -> stateMachineArn) (\s@UpdateStateMachine' {} a -> s {stateMachineArn = a} :: UpdateStateMachine)

instance Prelude.AWSRequest UpdateStateMachine where
  type
    Rs UpdateStateMachine =
      UpdateStateMachineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStateMachineResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "updateDate")
      )

instance Prelude.Hashable UpdateStateMachine

instance Prelude.NFData UpdateStateMachine

instance Prelude.ToHeaders UpdateStateMachine where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSStepFunctions.UpdateStateMachine" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateStateMachine where
  toJSON UpdateStateMachine' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("roleArn" Prelude..=) Prelude.<$> roleArn,
            ("tracingConfiguration" Prelude..=)
              Prelude.<$> tracingConfiguration,
            ("loggingConfiguration" Prelude..=)
              Prelude.<$> loggingConfiguration,
            ("definition" Prelude..=) Prelude.<$> definition,
            Prelude.Just
              ("stateMachineArn" Prelude..= stateMachineArn)
          ]
      )

instance Prelude.ToPath UpdateStateMachine where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateStateMachine where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStateMachineResponse' smart constructor.
data UpdateStateMachineResponse = UpdateStateMachineResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The date and time the state machine was updated.
    updateDate :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        updateDate = Prelude._Time Lens.# pUpdateDate_
      }

-- | The response's http status code.
updateStateMachineResponse_httpStatus :: Lens.Lens' UpdateStateMachineResponse Prelude.Int
updateStateMachineResponse_httpStatus = Lens.lens (\UpdateStateMachineResponse' {httpStatus} -> httpStatus) (\s@UpdateStateMachineResponse' {} a -> s {httpStatus = a} :: UpdateStateMachineResponse)

-- | The date and time the state machine was updated.
updateStateMachineResponse_updateDate :: Lens.Lens' UpdateStateMachineResponse Prelude.UTCTime
updateStateMachineResponse_updateDate = Lens.lens (\UpdateStateMachineResponse' {updateDate} -> updateDate) (\s@UpdateStateMachineResponse' {} a -> s {updateDate = a} :: UpdateStateMachineResponse) Prelude.. Prelude._Time

instance Prelude.NFData UpdateStateMachineResponse
