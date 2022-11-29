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
-- Module      : Amazonka.StepFunctions.StopExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an execution.
--
-- This API action is not supported by @EXPRESS@ state machines.
module Amazonka.StepFunctions.StopExecution
  ( -- * Creating a Request
    StopExecution (..),
    newStopExecution,

    -- * Request Lenses
    stopExecution_error,
    stopExecution_cause,
    stopExecution_executionArn,

    -- * Destructuring the Response
    StopExecutionResponse (..),
    newStopExecutionResponse,

    -- * Response Lenses
    stopExecutionResponse_httpStatus,
    stopExecutionResponse_stopDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newStopExecution' smart constructor.
data StopExecution = StopExecution'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the execution to stop.
    executionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'stopExecution_error' - The error code of the failure.
--
-- 'cause', 'stopExecution_cause' - A more detailed explanation of the cause of the failure.
--
-- 'executionArn', 'stopExecution_executionArn' - The Amazon Resource Name (ARN) of the execution to stop.
newStopExecution ::
  -- | 'executionArn'
  Prelude.Text ->
  StopExecution
newStopExecution pExecutionArn_ =
  StopExecution'
    { error = Prelude.Nothing,
      cause = Prelude.Nothing,
      executionArn = pExecutionArn_
    }

-- | The error code of the failure.
stopExecution_error :: Lens.Lens' StopExecution (Prelude.Maybe Prelude.Text)
stopExecution_error = Lens.lens (\StopExecution' {error} -> error) (\s@StopExecution' {} a -> s {error = a} :: StopExecution) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
stopExecution_cause :: Lens.Lens' StopExecution (Prelude.Maybe Prelude.Text)
stopExecution_cause = Lens.lens (\StopExecution' {cause} -> cause) (\s@StopExecution' {} a -> s {cause = a} :: StopExecution) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the execution to stop.
stopExecution_executionArn :: Lens.Lens' StopExecution Prelude.Text
stopExecution_executionArn = Lens.lens (\StopExecution' {executionArn} -> executionArn) (\s@StopExecution' {} a -> s {executionArn = a} :: StopExecution)

instance Core.AWSRequest StopExecution where
  type
    AWSResponse StopExecution =
      StopExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopExecutionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "stopDate")
      )

instance Prelude.Hashable StopExecution where
  hashWithSalt _salt StopExecution' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` executionArn

instance Prelude.NFData StopExecution where
  rnf StopExecution' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf cause
      `Prelude.seq` Prelude.rnf executionArn

instance Core.ToHeaders StopExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.StopExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopExecution where
  toJSON StopExecution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("error" Core..=) Prelude.<$> error,
            ("cause" Core..=) Prelude.<$> cause,
            Prelude.Just ("executionArn" Core..= executionArn)
          ]
      )

instance Core.ToPath StopExecution where
  toPath = Prelude.const "/"

instance Core.ToQuery StopExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopExecutionResponse' smart constructor.
data StopExecutionResponse = StopExecutionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The date the execution is stopped.
    stopDate :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'stopDate'
  Prelude.UTCTime ->
  StopExecutionResponse
newStopExecutionResponse pHttpStatus_ pStopDate_ =
  StopExecutionResponse'
    { httpStatus = pHttpStatus_,
      stopDate = Core._Time Lens.# pStopDate_
    }

-- | The response's http status code.
stopExecutionResponse_httpStatus :: Lens.Lens' StopExecutionResponse Prelude.Int
stopExecutionResponse_httpStatus = Lens.lens (\StopExecutionResponse' {httpStatus} -> httpStatus) (\s@StopExecutionResponse' {} a -> s {httpStatus = a} :: StopExecutionResponse)

-- | The date the execution is stopped.
stopExecutionResponse_stopDate :: Lens.Lens' StopExecutionResponse Prelude.UTCTime
stopExecutionResponse_stopDate = Lens.lens (\StopExecutionResponse' {stopDate} -> stopDate) (\s@StopExecutionResponse' {} a -> s {stopDate = a} :: StopExecutionResponse) Prelude.. Core._Time

instance Prelude.NFData StopExecutionResponse where
  rnf StopExecutionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stopDate
