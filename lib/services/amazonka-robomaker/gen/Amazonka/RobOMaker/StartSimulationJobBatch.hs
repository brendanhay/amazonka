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
-- Module      : Amazonka.RobOMaker.StartSimulationJobBatch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new simulation job batch. The batch is defined using one or
-- more @SimulationJobRequest@ objects.
module Amazonka.RobOMaker.StartSimulationJobBatch
  ( -- * Creating a Request
    StartSimulationJobBatch (..),
    newStartSimulationJobBatch,

    -- * Request Lenses
    startSimulationJobBatch_tags,
    startSimulationJobBatch_clientRequestToken,
    startSimulationJobBatch_batchPolicy,
    startSimulationJobBatch_createSimulationJobRequests,

    -- * Destructuring the Response
    StartSimulationJobBatchResponse (..),
    newStartSimulationJobBatchResponse,

    -- * Response Lenses
    startSimulationJobBatchResponse_tags,
    startSimulationJobBatchResponse_failureCode,
    startSimulationJobBatchResponse_clientRequestToken,
    startSimulationJobBatchResponse_arn,
    startSimulationJobBatchResponse_status,
    startSimulationJobBatchResponse_failedRequests,
    startSimulationJobBatchResponse_pendingRequests,
    startSimulationJobBatchResponse_createdRequests,
    startSimulationJobBatchResponse_batchPolicy,
    startSimulationJobBatchResponse_createdAt,
    startSimulationJobBatchResponse_failureReason,
    startSimulationJobBatchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newStartSimulationJobBatch' smart constructor.
data StartSimulationJobBatch = StartSimulationJobBatch'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- deployment job batch.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The batch policy.
    batchPolicy :: Prelude.Maybe BatchPolicy,
    -- | A list of simulation job requests to create in the batch.
    createSimulationJobRequests :: Prelude.NonEmpty SimulationJobRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSimulationJobBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startSimulationJobBatch_tags' - A map that contains tag keys and tag values that are attached to the
-- deployment job batch.
--
-- 'clientRequestToken', 'startSimulationJobBatch_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'batchPolicy', 'startSimulationJobBatch_batchPolicy' - The batch policy.
--
-- 'createSimulationJobRequests', 'startSimulationJobBatch_createSimulationJobRequests' - A list of simulation job requests to create in the batch.
newStartSimulationJobBatch ::
  -- | 'createSimulationJobRequests'
  Prelude.NonEmpty SimulationJobRequest ->
  StartSimulationJobBatch
newStartSimulationJobBatch
  pCreateSimulationJobRequests_ =
    StartSimulationJobBatch'
      { tags = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        batchPolicy = Prelude.Nothing,
        createSimulationJobRequests =
          Lens.coerced
            Lens.# pCreateSimulationJobRequests_
      }

-- | A map that contains tag keys and tag values that are attached to the
-- deployment job batch.
startSimulationJobBatch_tags :: Lens.Lens' StartSimulationJobBatch (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startSimulationJobBatch_tags = Lens.lens (\StartSimulationJobBatch' {tags} -> tags) (\s@StartSimulationJobBatch' {} a -> s {tags = a} :: StartSimulationJobBatch) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
startSimulationJobBatch_clientRequestToken :: Lens.Lens' StartSimulationJobBatch (Prelude.Maybe Prelude.Text)
startSimulationJobBatch_clientRequestToken = Lens.lens (\StartSimulationJobBatch' {clientRequestToken} -> clientRequestToken) (\s@StartSimulationJobBatch' {} a -> s {clientRequestToken = a} :: StartSimulationJobBatch)

-- | The batch policy.
startSimulationJobBatch_batchPolicy :: Lens.Lens' StartSimulationJobBatch (Prelude.Maybe BatchPolicy)
startSimulationJobBatch_batchPolicy = Lens.lens (\StartSimulationJobBatch' {batchPolicy} -> batchPolicy) (\s@StartSimulationJobBatch' {} a -> s {batchPolicy = a} :: StartSimulationJobBatch)

-- | A list of simulation job requests to create in the batch.
startSimulationJobBatch_createSimulationJobRequests :: Lens.Lens' StartSimulationJobBatch (Prelude.NonEmpty SimulationJobRequest)
startSimulationJobBatch_createSimulationJobRequests = Lens.lens (\StartSimulationJobBatch' {createSimulationJobRequests} -> createSimulationJobRequests) (\s@StartSimulationJobBatch' {} a -> s {createSimulationJobRequests = a} :: StartSimulationJobBatch) Prelude.. Lens.coerced

instance Core.AWSRequest StartSimulationJobBatch where
  type
    AWSResponse StartSimulationJobBatch =
      StartSimulationJobBatchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSimulationJobBatchResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "failureCode")
            Prelude.<*> (x Data..?> "clientRequestToken")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "failedRequests" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "pendingRequests")
            Prelude.<*> ( x Data..?> "createdRequests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "batchPolicy")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "failureReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSimulationJobBatch where
  hashWithSalt _salt StartSimulationJobBatch' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` batchPolicy
      `Prelude.hashWithSalt` createSimulationJobRequests

instance Prelude.NFData StartSimulationJobBatch where
  rnf StartSimulationJobBatch' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf batchPolicy
      `Prelude.seq` Prelude.rnf createSimulationJobRequests

instance Data.ToHeaders StartSimulationJobBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSimulationJobBatch where
  toJSON StartSimulationJobBatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("batchPolicy" Data..=) Prelude.<$> batchPolicy,
            Prelude.Just
              ( "createSimulationJobRequests"
                  Data..= createSimulationJobRequests
              )
          ]
      )

instance Data.ToPath StartSimulationJobBatch where
  toPath = Prelude.const "/startSimulationJobBatch"

instance Data.ToQuery StartSimulationJobBatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSimulationJobBatchResponse' smart constructor.
data StartSimulationJobBatchResponse = StartSimulationJobBatchResponse'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- deployment job batch.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The failure code if the simulation job batch failed.
    failureCode :: Prelude.Maybe SimulationJobBatchErrorCode,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (arn) of the batch.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the simulation job batch.
    --
    -- [Pending]
    --     The simulation job batch request is pending.
    --
    -- [InProgress]
    --     The simulation job batch is in progress.
    --
    -- [Failed]
    --     The simulation job batch failed. One or more simulation job requests
    --     could not be completed due to an internal failure (like
    --     @InternalServiceError@). See @failureCode@ and @failureReason@ for
    --     more information.
    --
    -- [Completed]
    --     The simulation batch job completed. A batch is complete when (1)
    --     there are no pending simulation job requests in the batch and none
    --     of the failed simulation job requests are due to
    --     @InternalServiceError@ and (2) when all created simulation jobs have
    --     reached a terminal state (for example, @Completed@ or @Failed@).
    --
    -- [Canceled]
    --     The simulation batch job was cancelled.
    --
    -- [Canceling]
    --     The simulation batch job is being cancelled.
    --
    -- [Completing]
    --     The simulation batch job is completing.
    --
    -- [TimingOut]
    --     The simulation job batch is timing out.
    --
    --     If a batch timing out, and there are pending requests that were
    --     failing due to an internal failure (like @InternalServiceError@),
    --     the batch status will be @Failed@. If there are no such failing
    --     request, the batch status will be @TimedOut@.
    --
    -- [TimedOut]
    --     The simulation batch job timed out.
    status :: Prelude.Maybe SimulationJobBatchStatus,
    -- | A list of failed simulation job requests. The request failed to be
    -- created into a simulation job. Failed requests do not have a simulation
    -- job ID.
    failedRequests :: Prelude.Maybe [FailedCreateSimulationJobRequest],
    -- | A list of pending simulation job requests. These requests have not yet
    -- been created into simulation jobs.
    pendingRequests :: Prelude.Maybe (Prelude.NonEmpty SimulationJobRequest),
    -- | A list of created simulation job request summaries.
    createdRequests :: Prelude.Maybe [SimulationJobSummary],
    -- | The batch policy.
    batchPolicy :: Prelude.Maybe BatchPolicy,
    -- | The time, in milliseconds since the epoch, when the simulation job batch
    -- was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The reason the simulation job batch failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSimulationJobBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startSimulationJobBatchResponse_tags' - A map that contains tag keys and tag values that are attached to the
-- deployment job batch.
--
-- 'failureCode', 'startSimulationJobBatchResponse_failureCode' - The failure code if the simulation job batch failed.
--
-- 'clientRequestToken', 'startSimulationJobBatchResponse_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'arn', 'startSimulationJobBatchResponse_arn' - The Amazon Resource Name (arn) of the batch.
--
-- 'status', 'startSimulationJobBatchResponse_status' - The status of the simulation job batch.
--
-- [Pending]
--     The simulation job batch request is pending.
--
-- [InProgress]
--     The simulation job batch is in progress.
--
-- [Failed]
--     The simulation job batch failed. One or more simulation job requests
--     could not be completed due to an internal failure (like
--     @InternalServiceError@). See @failureCode@ and @failureReason@ for
--     more information.
--
-- [Completed]
--     The simulation batch job completed. A batch is complete when (1)
--     there are no pending simulation job requests in the batch and none
--     of the failed simulation job requests are due to
--     @InternalServiceError@ and (2) when all created simulation jobs have
--     reached a terminal state (for example, @Completed@ or @Failed@).
--
-- [Canceled]
--     The simulation batch job was cancelled.
--
-- [Canceling]
--     The simulation batch job is being cancelled.
--
-- [Completing]
--     The simulation batch job is completing.
--
-- [TimingOut]
--     The simulation job batch is timing out.
--
--     If a batch timing out, and there are pending requests that were
--     failing due to an internal failure (like @InternalServiceError@),
--     the batch status will be @Failed@. If there are no such failing
--     request, the batch status will be @TimedOut@.
--
-- [TimedOut]
--     The simulation batch job timed out.
--
-- 'failedRequests', 'startSimulationJobBatchResponse_failedRequests' - A list of failed simulation job requests. The request failed to be
-- created into a simulation job. Failed requests do not have a simulation
-- job ID.
--
-- 'pendingRequests', 'startSimulationJobBatchResponse_pendingRequests' - A list of pending simulation job requests. These requests have not yet
-- been created into simulation jobs.
--
-- 'createdRequests', 'startSimulationJobBatchResponse_createdRequests' - A list of created simulation job request summaries.
--
-- 'batchPolicy', 'startSimulationJobBatchResponse_batchPolicy' - The batch policy.
--
-- 'createdAt', 'startSimulationJobBatchResponse_createdAt' - The time, in milliseconds since the epoch, when the simulation job batch
-- was created.
--
-- 'failureReason', 'startSimulationJobBatchResponse_failureReason' - The reason the simulation job batch failed.
--
-- 'httpStatus', 'startSimulationJobBatchResponse_httpStatus' - The response's http status code.
newStartSimulationJobBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSimulationJobBatchResponse
newStartSimulationJobBatchResponse pHttpStatus_ =
  StartSimulationJobBatchResponse'
    { tags =
        Prelude.Nothing,
      failureCode = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      failedRequests = Prelude.Nothing,
      pendingRequests = Prelude.Nothing,
      createdRequests = Prelude.Nothing,
      batchPolicy = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map that contains tag keys and tag values that are attached to the
-- deployment job batch.
startSimulationJobBatchResponse_tags :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startSimulationJobBatchResponse_tags = Lens.lens (\StartSimulationJobBatchResponse' {tags} -> tags) (\s@StartSimulationJobBatchResponse' {} a -> s {tags = a} :: StartSimulationJobBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | The failure code if the simulation job batch failed.
startSimulationJobBatchResponse_failureCode :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe SimulationJobBatchErrorCode)
startSimulationJobBatchResponse_failureCode = Lens.lens (\StartSimulationJobBatchResponse' {failureCode} -> failureCode) (\s@StartSimulationJobBatchResponse' {} a -> s {failureCode = a} :: StartSimulationJobBatchResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
startSimulationJobBatchResponse_clientRequestToken :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe Prelude.Text)
startSimulationJobBatchResponse_clientRequestToken = Lens.lens (\StartSimulationJobBatchResponse' {clientRequestToken} -> clientRequestToken) (\s@StartSimulationJobBatchResponse' {} a -> s {clientRequestToken = a} :: StartSimulationJobBatchResponse)

-- | The Amazon Resource Name (arn) of the batch.
startSimulationJobBatchResponse_arn :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe Prelude.Text)
startSimulationJobBatchResponse_arn = Lens.lens (\StartSimulationJobBatchResponse' {arn} -> arn) (\s@StartSimulationJobBatchResponse' {} a -> s {arn = a} :: StartSimulationJobBatchResponse)

-- | The status of the simulation job batch.
--
-- [Pending]
--     The simulation job batch request is pending.
--
-- [InProgress]
--     The simulation job batch is in progress.
--
-- [Failed]
--     The simulation job batch failed. One or more simulation job requests
--     could not be completed due to an internal failure (like
--     @InternalServiceError@). See @failureCode@ and @failureReason@ for
--     more information.
--
-- [Completed]
--     The simulation batch job completed. A batch is complete when (1)
--     there are no pending simulation job requests in the batch and none
--     of the failed simulation job requests are due to
--     @InternalServiceError@ and (2) when all created simulation jobs have
--     reached a terminal state (for example, @Completed@ or @Failed@).
--
-- [Canceled]
--     The simulation batch job was cancelled.
--
-- [Canceling]
--     The simulation batch job is being cancelled.
--
-- [Completing]
--     The simulation batch job is completing.
--
-- [TimingOut]
--     The simulation job batch is timing out.
--
--     If a batch timing out, and there are pending requests that were
--     failing due to an internal failure (like @InternalServiceError@),
--     the batch status will be @Failed@. If there are no such failing
--     request, the batch status will be @TimedOut@.
--
-- [TimedOut]
--     The simulation batch job timed out.
startSimulationJobBatchResponse_status :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe SimulationJobBatchStatus)
startSimulationJobBatchResponse_status = Lens.lens (\StartSimulationJobBatchResponse' {status} -> status) (\s@StartSimulationJobBatchResponse' {} a -> s {status = a} :: StartSimulationJobBatchResponse)

-- | A list of failed simulation job requests. The request failed to be
-- created into a simulation job. Failed requests do not have a simulation
-- job ID.
startSimulationJobBatchResponse_failedRequests :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe [FailedCreateSimulationJobRequest])
startSimulationJobBatchResponse_failedRequests = Lens.lens (\StartSimulationJobBatchResponse' {failedRequests} -> failedRequests) (\s@StartSimulationJobBatchResponse' {} a -> s {failedRequests = a} :: StartSimulationJobBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of pending simulation job requests. These requests have not yet
-- been created into simulation jobs.
startSimulationJobBatchResponse_pendingRequests :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe (Prelude.NonEmpty SimulationJobRequest))
startSimulationJobBatchResponse_pendingRequests = Lens.lens (\StartSimulationJobBatchResponse' {pendingRequests} -> pendingRequests) (\s@StartSimulationJobBatchResponse' {} a -> s {pendingRequests = a} :: StartSimulationJobBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of created simulation job request summaries.
startSimulationJobBatchResponse_createdRequests :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe [SimulationJobSummary])
startSimulationJobBatchResponse_createdRequests = Lens.lens (\StartSimulationJobBatchResponse' {createdRequests} -> createdRequests) (\s@StartSimulationJobBatchResponse' {} a -> s {createdRequests = a} :: StartSimulationJobBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | The batch policy.
startSimulationJobBatchResponse_batchPolicy :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe BatchPolicy)
startSimulationJobBatchResponse_batchPolicy = Lens.lens (\StartSimulationJobBatchResponse' {batchPolicy} -> batchPolicy) (\s@StartSimulationJobBatchResponse' {} a -> s {batchPolicy = a} :: StartSimulationJobBatchResponse)

-- | The time, in milliseconds since the epoch, when the simulation job batch
-- was created.
startSimulationJobBatchResponse_createdAt :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe Prelude.UTCTime)
startSimulationJobBatchResponse_createdAt = Lens.lens (\StartSimulationJobBatchResponse' {createdAt} -> createdAt) (\s@StartSimulationJobBatchResponse' {} a -> s {createdAt = a} :: StartSimulationJobBatchResponse) Prelude.. Lens.mapping Data._Time

-- | The reason the simulation job batch failed.
startSimulationJobBatchResponse_failureReason :: Lens.Lens' StartSimulationJobBatchResponse (Prelude.Maybe Prelude.Text)
startSimulationJobBatchResponse_failureReason = Lens.lens (\StartSimulationJobBatchResponse' {failureReason} -> failureReason) (\s@StartSimulationJobBatchResponse' {} a -> s {failureReason = a} :: StartSimulationJobBatchResponse)

-- | The response's http status code.
startSimulationJobBatchResponse_httpStatus :: Lens.Lens' StartSimulationJobBatchResponse Prelude.Int
startSimulationJobBatchResponse_httpStatus = Lens.lens (\StartSimulationJobBatchResponse' {httpStatus} -> httpStatus) (\s@StartSimulationJobBatchResponse' {} a -> s {httpStatus = a} :: StartSimulationJobBatchResponse)

instance
  Prelude.NFData
    StartSimulationJobBatchResponse
  where
  rnf StartSimulationJobBatchResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf failedRequests
      `Prelude.seq` Prelude.rnf pendingRequests
      `Prelude.seq` Prelude.rnf createdRequests
      `Prelude.seq` Prelude.rnf batchPolicy
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
