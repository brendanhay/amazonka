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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newUpdateStateMachine' smart constructor.
data UpdateStateMachine = UpdateStateMachine'
  { -- | The Amazon Resource Name (ARN) of the IAM role of the state machine.
    roleArn :: Core.Maybe Core.Text,
    -- | Selects whether AWS X-Ray tracing is enabled.
    tracingConfiguration :: Core.Maybe TracingConfiguration,
    -- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs
    -- options.
    loggingConfiguration :: Core.Maybe LoggingConfiguration,
    -- | The Amazon States Language definition of the state machine. See
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
    definition :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The Amazon Resource Name (ARN) of the state machine.
    stateMachineArn :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateStateMachine
newUpdateStateMachine pStateMachineArn_ =
  UpdateStateMachine'
    { roleArn = Core.Nothing,
      tracingConfiguration = Core.Nothing,
      loggingConfiguration = Core.Nothing,
      definition = Core.Nothing,
      stateMachineArn = pStateMachineArn_
    }

-- | The Amazon Resource Name (ARN) of the IAM role of the state machine.
updateStateMachine_roleArn :: Lens.Lens' UpdateStateMachine (Core.Maybe Core.Text)
updateStateMachine_roleArn = Lens.lens (\UpdateStateMachine' {roleArn} -> roleArn) (\s@UpdateStateMachine' {} a -> s {roleArn = a} :: UpdateStateMachine)

-- | Selects whether AWS X-Ray tracing is enabled.
updateStateMachine_tracingConfiguration :: Lens.Lens' UpdateStateMachine (Core.Maybe TracingConfiguration)
updateStateMachine_tracingConfiguration = Lens.lens (\UpdateStateMachine' {tracingConfiguration} -> tracingConfiguration) (\s@UpdateStateMachine' {} a -> s {tracingConfiguration = a} :: UpdateStateMachine)

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs
-- options.
updateStateMachine_loggingConfiguration :: Lens.Lens' UpdateStateMachine (Core.Maybe LoggingConfiguration)
updateStateMachine_loggingConfiguration = Lens.lens (\UpdateStateMachine' {loggingConfiguration} -> loggingConfiguration) (\s@UpdateStateMachine' {} a -> s {loggingConfiguration = a} :: UpdateStateMachine)

-- | The Amazon States Language definition of the state machine. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language>.
updateStateMachine_definition :: Lens.Lens' UpdateStateMachine (Core.Maybe Core.Text)
updateStateMachine_definition = Lens.lens (\UpdateStateMachine' {definition} -> definition) (\s@UpdateStateMachine' {} a -> s {definition = a} :: UpdateStateMachine) Core.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the state machine.
updateStateMachine_stateMachineArn :: Lens.Lens' UpdateStateMachine Core.Text
updateStateMachine_stateMachineArn = Lens.lens (\UpdateStateMachine' {stateMachineArn} -> stateMachineArn) (\s@UpdateStateMachine' {} a -> s {stateMachineArn = a} :: UpdateStateMachine)

instance Core.AWSRequest UpdateStateMachine where
  type
    AWSResponse UpdateStateMachine =
      UpdateStateMachineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStateMachineResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "updateDate")
      )

instance Core.Hashable UpdateStateMachine

instance Core.NFData UpdateStateMachine

instance Core.ToHeaders UpdateStateMachine where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.UpdateStateMachine" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateStateMachine where
  toJSON UpdateStateMachine' {..} =
    Core.object
      ( Core.catMaybes
          [ ("roleArn" Core..=) Core.<$> roleArn,
            ("tracingConfiguration" Core..=)
              Core.<$> tracingConfiguration,
            ("loggingConfiguration" Core..=)
              Core.<$> loggingConfiguration,
            ("definition" Core..=) Core.<$> definition,
            Core.Just
              ("stateMachineArn" Core..= stateMachineArn)
          ]
      )

instance Core.ToPath UpdateStateMachine where
  toPath = Core.const "/"

instance Core.ToQuery UpdateStateMachine where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateStateMachineResponse' smart constructor.
data UpdateStateMachineResponse = UpdateStateMachineResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The date and time the state machine was updated.
    updateDate :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'updateDate'
  Core.UTCTime ->
  UpdateStateMachineResponse
newUpdateStateMachineResponse
  pHttpStatus_
  pUpdateDate_ =
    UpdateStateMachineResponse'
      { httpStatus =
          pHttpStatus_,
        updateDate = Core._Time Lens.# pUpdateDate_
      }

-- | The response's http status code.
updateStateMachineResponse_httpStatus :: Lens.Lens' UpdateStateMachineResponse Core.Int
updateStateMachineResponse_httpStatus = Lens.lens (\UpdateStateMachineResponse' {httpStatus} -> httpStatus) (\s@UpdateStateMachineResponse' {} a -> s {httpStatus = a} :: UpdateStateMachineResponse)

-- | The date and time the state machine was updated.
updateStateMachineResponse_updateDate :: Lens.Lens' UpdateStateMachineResponse Core.UTCTime
updateStateMachineResponse_updateDate = Lens.lens (\UpdateStateMachineResponse' {updateDate} -> updateDate) (\s@UpdateStateMachineResponse' {} a -> s {updateDate = a} :: UpdateStateMachineResponse) Core.. Core._Time

instance Core.NFData UpdateStateMachineResponse
