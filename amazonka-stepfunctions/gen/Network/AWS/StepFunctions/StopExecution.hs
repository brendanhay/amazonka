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
-- Module      : Network.AWS.StepFunctions.StopExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an execution.
--
-- This API action is not supported by @EXPRESS@ state machines.
module Network.AWS.StepFunctions.StopExecution
  ( -- * Creating a Request
    StopExecution (..),
    newStopExecution,

    -- * Request Lenses
    stopExecution_cause,
    stopExecution_error,
    stopExecution_executionArn,

    -- * Destructuring the Response
    StopExecutionResponse (..),
    newStopExecutionResponse,

    -- * Response Lenses
    stopExecutionResponse_httpStatus,
    stopExecutionResponse_stopDate,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newStopExecution' smart constructor.
data StopExecution = StopExecution'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The error code of the failure.
    error :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The Amazon Resource Name (ARN) of the execution to stop.
    executionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'stopExecution_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'stopExecution_error' - The error code of the failure.
--
-- 'executionArn', 'stopExecution_executionArn' - The Amazon Resource Name (ARN) of the execution to stop.
newStopExecution ::
  -- | 'executionArn'
  Core.Text ->
  StopExecution
newStopExecution pExecutionArn_ =
  StopExecution'
    { cause = Core.Nothing,
      error = Core.Nothing,
      executionArn = pExecutionArn_
    }

-- | A more detailed explanation of the cause of the failure.
stopExecution_cause :: Lens.Lens' StopExecution (Core.Maybe Core.Text)
stopExecution_cause = Lens.lens (\StopExecution' {cause} -> cause) (\s@StopExecution' {} a -> s {cause = a} :: StopExecution) Core.. Lens.mapping Core._Sensitive

-- | The error code of the failure.
stopExecution_error :: Lens.Lens' StopExecution (Core.Maybe Core.Text)
stopExecution_error = Lens.lens (\StopExecution' {error} -> error) (\s@StopExecution' {} a -> s {error = a} :: StopExecution) Core.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the execution to stop.
stopExecution_executionArn :: Lens.Lens' StopExecution Core.Text
stopExecution_executionArn = Lens.lens (\StopExecution' {executionArn} -> executionArn) (\s@StopExecution' {} a -> s {executionArn = a} :: StopExecution)

instance Core.AWSRequest StopExecution where
  type
    AWSResponse StopExecution =
      StopExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopExecutionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "stopDate")
      )

instance Core.Hashable StopExecution

instance Core.NFData StopExecution

instance Core.ToHeaders StopExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.StopExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopExecution where
  toJSON StopExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("cause" Core..=) Core.<$> cause,
            ("error" Core..=) Core.<$> error,
            Core.Just ("executionArn" Core..= executionArn)
          ]
      )

instance Core.ToPath StopExecution where
  toPath = Core.const "/"

instance Core.ToQuery StopExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopExecutionResponse' smart constructor.
data StopExecutionResponse = StopExecutionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The date the execution is stopped.
    stopDate :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopExecutionResponse_httpStatus' - The response's http status code.
--
-- 'stopDate', 'stopExecutionResponse_stopDate' - The date the execution is stopped.
newStopExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'stopDate'
  Core.UTCTime ->
  StopExecutionResponse
newStopExecutionResponse pHttpStatus_ pStopDate_ =
  StopExecutionResponse'
    { httpStatus = pHttpStatus_,
      stopDate = Core._Time Lens.# pStopDate_
    }

-- | The response's http status code.
stopExecutionResponse_httpStatus :: Lens.Lens' StopExecutionResponse Core.Int
stopExecutionResponse_httpStatus = Lens.lens (\StopExecutionResponse' {httpStatus} -> httpStatus) (\s@StopExecutionResponse' {} a -> s {httpStatus = a} :: StopExecutionResponse)

-- | The date the execution is stopped.
stopExecutionResponse_stopDate :: Lens.Lens' StopExecutionResponse Core.UTCTime
stopExecutionResponse_stopDate = Lens.lens (\StopExecutionResponse' {stopDate} -> stopDate) (\s@StopExecutionResponse' {} a -> s {stopDate = a} :: StopExecutionResponse) Core.. Core._Time

instance Core.NFData StopExecutionResponse
