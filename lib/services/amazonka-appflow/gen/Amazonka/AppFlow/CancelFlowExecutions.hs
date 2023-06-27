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
-- Module      : Amazonka.AppFlow.CancelFlowExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels active runs for a flow.
--
-- You can cancel all of the active runs for a flow, or you can cancel
-- specific runs by providing their IDs.
--
-- You can cancel a flow run only when the run is in progress. You can\'t
-- cancel a run that has already completed or failed. You also can\'t
-- cancel a run that\'s scheduled to occur but hasn\'t started yet. To
-- prevent a scheduled run, you can deactivate the flow with the @StopFlow@
-- action.
--
-- You cannot resume a run after you cancel it.
--
-- When you send your request, the status for each run becomes
-- @CancelStarted@. When the cancellation completes, the status becomes
-- @Canceled@.
--
-- When you cancel a run, you still incur charges for any data that the run
-- already processed before the cancellation. If the run had already
-- written some data to the flow destination, then that data remains in the
-- destination. If you configured the flow to use a batch API (such as the
-- Salesforce Bulk API 2.0), then the run will finish reading or writing
-- its entire batch of data after the cancellation. For these operations,
-- the data processing charges for Amazon AppFlow apply. For the pricing
-- information, see
-- <http://aws.amazon.com/appflow/pricing/ Amazon AppFlow pricing>.
module Amazonka.AppFlow.CancelFlowExecutions
  ( -- * Creating a Request
    CancelFlowExecutions (..),
    newCancelFlowExecutions,

    -- * Request Lenses
    cancelFlowExecutions_executionIds,
    cancelFlowExecutions_flowName,

    -- * Destructuring the Response
    CancelFlowExecutionsResponse (..),
    newCancelFlowExecutionsResponse,

    -- * Response Lenses
    cancelFlowExecutionsResponse_invalidExecutions,
    cancelFlowExecutionsResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelFlowExecutions' smart constructor.
data CancelFlowExecutions = CancelFlowExecutions'
  { -- | The ID of each active run to cancel. These runs must belong to the flow
    -- you specify in your request.
    --
    -- If you omit this parameter, your request ends all active runs that
    -- belong to the flow.
    executionIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of a flow with active runs that you want to cancel.
    flowName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelFlowExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionIds', 'cancelFlowExecutions_executionIds' - The ID of each active run to cancel. These runs must belong to the flow
-- you specify in your request.
--
-- If you omit this parameter, your request ends all active runs that
-- belong to the flow.
--
-- 'flowName', 'cancelFlowExecutions_flowName' - The name of a flow with active runs that you want to cancel.
newCancelFlowExecutions ::
  -- | 'flowName'
  Prelude.Text ->
  CancelFlowExecutions
newCancelFlowExecutions pFlowName_ =
  CancelFlowExecutions'
    { executionIds =
        Prelude.Nothing,
      flowName = pFlowName_
    }

-- | The ID of each active run to cancel. These runs must belong to the flow
-- you specify in your request.
--
-- If you omit this parameter, your request ends all active runs that
-- belong to the flow.
cancelFlowExecutions_executionIds :: Lens.Lens' CancelFlowExecutions (Prelude.Maybe [Prelude.Text])
cancelFlowExecutions_executionIds = Lens.lens (\CancelFlowExecutions' {executionIds} -> executionIds) (\s@CancelFlowExecutions' {} a -> s {executionIds = a} :: CancelFlowExecutions) Prelude.. Lens.mapping Lens.coerced

-- | The name of a flow with active runs that you want to cancel.
cancelFlowExecutions_flowName :: Lens.Lens' CancelFlowExecutions Prelude.Text
cancelFlowExecutions_flowName = Lens.lens (\CancelFlowExecutions' {flowName} -> flowName) (\s@CancelFlowExecutions' {} a -> s {flowName = a} :: CancelFlowExecutions)

instance Core.AWSRequest CancelFlowExecutions where
  type
    AWSResponse CancelFlowExecutions =
      CancelFlowExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelFlowExecutionsResponse'
            Prelude.<$> ( x
                            Data..?> "invalidExecutions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelFlowExecutions where
  hashWithSalt _salt CancelFlowExecutions' {..} =
    _salt
      `Prelude.hashWithSalt` executionIds
      `Prelude.hashWithSalt` flowName

instance Prelude.NFData CancelFlowExecutions where
  rnf CancelFlowExecutions' {..} =
    Prelude.rnf executionIds
      `Prelude.seq` Prelude.rnf flowName

instance Data.ToHeaders CancelFlowExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelFlowExecutions where
  toJSON CancelFlowExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("executionIds" Data..=) Prelude.<$> executionIds,
            Prelude.Just ("flowName" Data..= flowName)
          ]
      )

instance Data.ToPath CancelFlowExecutions where
  toPath = Prelude.const "/cancel-flow-executions"

instance Data.ToQuery CancelFlowExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelFlowExecutionsResponse' smart constructor.
data CancelFlowExecutionsResponse = CancelFlowExecutionsResponse'
  { -- | The IDs of runs that Amazon AppFlow couldn\'t cancel. These runs might
    -- be ineligible for canceling because they haven\'t started yet or have
    -- already completed.
    invalidExecutions :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelFlowExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invalidExecutions', 'cancelFlowExecutionsResponse_invalidExecutions' - The IDs of runs that Amazon AppFlow couldn\'t cancel. These runs might
-- be ineligible for canceling because they haven\'t started yet or have
-- already completed.
--
-- 'httpStatus', 'cancelFlowExecutionsResponse_httpStatus' - The response's http status code.
newCancelFlowExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelFlowExecutionsResponse
newCancelFlowExecutionsResponse pHttpStatus_ =
  CancelFlowExecutionsResponse'
    { invalidExecutions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of runs that Amazon AppFlow couldn\'t cancel. These runs might
-- be ineligible for canceling because they haven\'t started yet or have
-- already completed.
cancelFlowExecutionsResponse_invalidExecutions :: Lens.Lens' CancelFlowExecutionsResponse (Prelude.Maybe [Prelude.Text])
cancelFlowExecutionsResponse_invalidExecutions = Lens.lens (\CancelFlowExecutionsResponse' {invalidExecutions} -> invalidExecutions) (\s@CancelFlowExecutionsResponse' {} a -> s {invalidExecutions = a} :: CancelFlowExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
cancelFlowExecutionsResponse_httpStatus :: Lens.Lens' CancelFlowExecutionsResponse Prelude.Int
cancelFlowExecutionsResponse_httpStatus = Lens.lens (\CancelFlowExecutionsResponse' {httpStatus} -> httpStatus) (\s@CancelFlowExecutionsResponse' {} a -> s {httpStatus = a} :: CancelFlowExecutionsResponse)

instance Prelude.NFData CancelFlowExecutionsResponse where
  rnf CancelFlowExecutionsResponse' {..} =
    Prelude.rnf invalidExecutions
      `Prelude.seq` Prelude.rnf httpStatus
