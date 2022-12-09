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
-- Module      : Amazonka.Athena.StopCalculationExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the cancellation of a calculation. A @StopCalculationExecution@
-- call on a calculation that is already in a terminal state (for example,
-- @STOPPED@, @FAILED@, or @COMPLETED@) succeeds but has no effect.
--
-- Cancelling a calculation is done on a best effort basis. If a
-- calculation cannot be cancelled, you can be charged for its completion.
-- If you are concerned about being charged for a calculation that cannot
-- be cancelled, consider terminating the session in which the calculation
-- is running.
module Amazonka.Athena.StopCalculationExecution
  ( -- * Creating a Request
    StopCalculationExecution (..),
    newStopCalculationExecution,

    -- * Request Lenses
    stopCalculationExecution_calculationExecutionId,

    -- * Destructuring the Response
    StopCalculationExecutionResponse (..),
    newStopCalculationExecutionResponse,

    -- * Response Lenses
    stopCalculationExecutionResponse_state,
    stopCalculationExecutionResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopCalculationExecution' smart constructor.
data StopCalculationExecution = StopCalculationExecution'
  { -- | The calculation execution UUID.
    calculationExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopCalculationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculationExecutionId', 'stopCalculationExecution_calculationExecutionId' - The calculation execution UUID.
newStopCalculationExecution ::
  -- | 'calculationExecutionId'
  Prelude.Text ->
  StopCalculationExecution
newStopCalculationExecution pCalculationExecutionId_ =
  StopCalculationExecution'
    { calculationExecutionId =
        pCalculationExecutionId_
    }

-- | The calculation execution UUID.
stopCalculationExecution_calculationExecutionId :: Lens.Lens' StopCalculationExecution Prelude.Text
stopCalculationExecution_calculationExecutionId = Lens.lens (\StopCalculationExecution' {calculationExecutionId} -> calculationExecutionId) (\s@StopCalculationExecution' {} a -> s {calculationExecutionId = a} :: StopCalculationExecution)

instance Core.AWSRequest StopCalculationExecution where
  type
    AWSResponse StopCalculationExecution =
      StopCalculationExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopCalculationExecutionResponse'
            Prelude.<$> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopCalculationExecution where
  hashWithSalt _salt StopCalculationExecution' {..} =
    _salt `Prelude.hashWithSalt` calculationExecutionId

instance Prelude.NFData StopCalculationExecution where
  rnf StopCalculationExecution' {..} =
    Prelude.rnf calculationExecutionId

instance Data.ToHeaders StopCalculationExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.StopCalculationExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopCalculationExecution where
  toJSON StopCalculationExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CalculationExecutionId"
                  Data..= calculationExecutionId
              )
          ]
      )

instance Data.ToPath StopCalculationExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StopCalculationExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopCalculationExecutionResponse' smart constructor.
data StopCalculationExecutionResponse = StopCalculationExecutionResponse'
  { -- | @CREATING@ - The calculation is in the process of being created.
    --
    -- @CREATED@ - The calculation has been created and is ready to run.
    --
    -- @QUEUED@ - The calculation has been queued for processing.
    --
    -- @RUNNING@ - The calculation is running.
    --
    -- @CANCELING@ - A request to cancel the calculation has been received and
    -- the system is working to stop it.
    --
    -- @CANCELED@ - The calculation is no longer running as the result of a
    -- cancel request.
    --
    -- @COMPLETED@ - The calculation has completed without error.
    --
    -- @FAILED@ - The calculation failed and is no longer running.
    state :: Prelude.Maybe CalculationExecutionState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopCalculationExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'stopCalculationExecutionResponse_state' - @CREATING@ - The calculation is in the process of being created.
--
-- @CREATED@ - The calculation has been created and is ready to run.
--
-- @QUEUED@ - The calculation has been queued for processing.
--
-- @RUNNING@ - The calculation is running.
--
-- @CANCELING@ - A request to cancel the calculation has been received and
-- the system is working to stop it.
--
-- @CANCELED@ - The calculation is no longer running as the result of a
-- cancel request.
--
-- @COMPLETED@ - The calculation has completed without error.
--
-- @FAILED@ - The calculation failed and is no longer running.
--
-- 'httpStatus', 'stopCalculationExecutionResponse_httpStatus' - The response's http status code.
newStopCalculationExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopCalculationExecutionResponse
newStopCalculationExecutionResponse pHttpStatus_ =
  StopCalculationExecutionResponse'
    { state =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | @CREATING@ - The calculation is in the process of being created.
--
-- @CREATED@ - The calculation has been created and is ready to run.
--
-- @QUEUED@ - The calculation has been queued for processing.
--
-- @RUNNING@ - The calculation is running.
--
-- @CANCELING@ - A request to cancel the calculation has been received and
-- the system is working to stop it.
--
-- @CANCELED@ - The calculation is no longer running as the result of a
-- cancel request.
--
-- @COMPLETED@ - The calculation has completed without error.
--
-- @FAILED@ - The calculation failed and is no longer running.
stopCalculationExecutionResponse_state :: Lens.Lens' StopCalculationExecutionResponse (Prelude.Maybe CalculationExecutionState)
stopCalculationExecutionResponse_state = Lens.lens (\StopCalculationExecutionResponse' {state} -> state) (\s@StopCalculationExecutionResponse' {} a -> s {state = a} :: StopCalculationExecutionResponse)

-- | The response's http status code.
stopCalculationExecutionResponse_httpStatus :: Lens.Lens' StopCalculationExecutionResponse Prelude.Int
stopCalculationExecutionResponse_httpStatus = Lens.lens (\StopCalculationExecutionResponse' {httpStatus} -> httpStatus) (\s@StopCalculationExecutionResponse' {} a -> s {httpStatus = a} :: StopCalculationExecutionResponse)

instance
  Prelude.NFData
    StopCalculationExecutionResponse
  where
  rnf StopCalculationExecutionResponse' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
