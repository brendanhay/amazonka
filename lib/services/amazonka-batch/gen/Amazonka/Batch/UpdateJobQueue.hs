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
-- Module      : Amazonka.Batch.UpdateJobQueue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a job queue.
module Amazonka.Batch.UpdateJobQueue
  ( -- * Creating a Request
    UpdateJobQueue (..),
    newUpdateJobQueue,

    -- * Request Lenses
    updateJobQueue_computeEnvironmentOrder,
    updateJobQueue_state,
    updateJobQueue_priority,
    updateJobQueue_schedulingPolicyArn,
    updateJobQueue_jobQueue,

    -- * Destructuring the Response
    UpdateJobQueueResponse (..),
    newUpdateJobQueueResponse,

    -- * Response Lenses
    updateJobQueueResponse_jobQueueArn,
    updateJobQueueResponse_jobQueueName,
    updateJobQueueResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @UpdateJobQueue@.
--
-- /See:/ 'newUpdateJobQueue' smart constructor.
data UpdateJobQueue = UpdateJobQueue'
  { -- | Details the set of compute environments mapped to a job queue and their
    -- order relative to each other. This is one of the parameters used by the
    -- job scheduler to determine which compute environment runs a given job.
    -- Compute environments must be in the @VALID@ state before you can
    -- associate them with a job queue. All of the compute environments must be
    -- either EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@).
    -- EC2 and Fargate compute environments can\'t be mixed.
    --
    -- All compute environments that are associated with a job queue must share
    -- the same architecture. Batch doesn\'t support mixing compute environment
    -- architecture types in a single job queue.
    computeEnvironmentOrder :: Prelude.Maybe [ComputeEnvironmentOrder],
    -- | Describes the queue\'s ability to accept new jobs. If the job queue
    -- state is @ENABLED@, it can accept jobs. If the job queue state is
    -- @DISABLED@, new jobs can\'t be added to the queue, but jobs already in
    -- the queue can finish.
    state :: Prelude.Maybe JQState,
    -- | The priority of the job queue. Job queues with a higher priority (or a
    -- higher integer value for the @priority@ parameter) are evaluated first
    -- when associated with the same compute environment. Priority is
    -- determined in descending order. For example, a job queue with a priority
    -- value of @10@ is given scheduling preference over a job queue with a
    -- priority value of @1@. All of the compute environments must be either
    -- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@). EC2 and
    -- Fargate compute environments can\'t be mixed.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Amazon Resource Name (ARN) of the fair share scheduling policy. Once a
    -- job queue is created, the fair share scheduling policy can be replaced
    -- but not removed. The format is
    -- @aws:Partition:batch:Region:Account:scheduling-policy\/Name @. For
    -- example,
    -- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
    schedulingPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The name or the Amazon Resource Name (ARN) of the job queue.
    jobQueue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeEnvironmentOrder', 'updateJobQueue_computeEnvironmentOrder' - Details the set of compute environments mapped to a job queue and their
-- order relative to each other. This is one of the parameters used by the
-- job scheduler to determine which compute environment runs a given job.
-- Compute environments must be in the @VALID@ state before you can
-- associate them with a job queue. All of the compute environments must be
-- either EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@).
-- EC2 and Fargate compute environments can\'t be mixed.
--
-- All compute environments that are associated with a job queue must share
-- the same architecture. Batch doesn\'t support mixing compute environment
-- architecture types in a single job queue.
--
-- 'state', 'updateJobQueue_state' - Describes the queue\'s ability to accept new jobs. If the job queue
-- state is @ENABLED@, it can accept jobs. If the job queue state is
-- @DISABLED@, new jobs can\'t be added to the queue, but jobs already in
-- the queue can finish.
--
-- 'priority', 'updateJobQueue_priority' - The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order. For example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@). EC2 and
-- Fargate compute environments can\'t be mixed.
--
-- 'schedulingPolicyArn', 'updateJobQueue_schedulingPolicyArn' - Amazon Resource Name (ARN) of the fair share scheduling policy. Once a
-- job queue is created, the fair share scheduling policy can be replaced
-- but not removed. The format is
-- @aws:Partition:batch:Region:Account:scheduling-policy\/Name @. For
-- example,
-- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
--
-- 'jobQueue', 'updateJobQueue_jobQueue' - The name or the Amazon Resource Name (ARN) of the job queue.
newUpdateJobQueue ::
  -- | 'jobQueue'
  Prelude.Text ->
  UpdateJobQueue
newUpdateJobQueue pJobQueue_ =
  UpdateJobQueue'
    { computeEnvironmentOrder =
        Prelude.Nothing,
      state = Prelude.Nothing,
      priority = Prelude.Nothing,
      schedulingPolicyArn = Prelude.Nothing,
      jobQueue = pJobQueue_
    }

-- | Details the set of compute environments mapped to a job queue and their
-- order relative to each other. This is one of the parameters used by the
-- job scheduler to determine which compute environment runs a given job.
-- Compute environments must be in the @VALID@ state before you can
-- associate them with a job queue. All of the compute environments must be
-- either EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@).
-- EC2 and Fargate compute environments can\'t be mixed.
--
-- All compute environments that are associated with a job queue must share
-- the same architecture. Batch doesn\'t support mixing compute environment
-- architecture types in a single job queue.
updateJobQueue_computeEnvironmentOrder :: Lens.Lens' UpdateJobQueue (Prelude.Maybe [ComputeEnvironmentOrder])
updateJobQueue_computeEnvironmentOrder = Lens.lens (\UpdateJobQueue' {computeEnvironmentOrder} -> computeEnvironmentOrder) (\s@UpdateJobQueue' {} a -> s {computeEnvironmentOrder = a} :: UpdateJobQueue) Prelude.. Lens.mapping Lens.coerced

-- | Describes the queue\'s ability to accept new jobs. If the job queue
-- state is @ENABLED@, it can accept jobs. If the job queue state is
-- @DISABLED@, new jobs can\'t be added to the queue, but jobs already in
-- the queue can finish.
updateJobQueue_state :: Lens.Lens' UpdateJobQueue (Prelude.Maybe JQState)
updateJobQueue_state = Lens.lens (\UpdateJobQueue' {state} -> state) (\s@UpdateJobQueue' {} a -> s {state = a} :: UpdateJobQueue)

-- | The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order. For example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@). EC2 and
-- Fargate compute environments can\'t be mixed.
updateJobQueue_priority :: Lens.Lens' UpdateJobQueue (Prelude.Maybe Prelude.Int)
updateJobQueue_priority = Lens.lens (\UpdateJobQueue' {priority} -> priority) (\s@UpdateJobQueue' {} a -> s {priority = a} :: UpdateJobQueue)

-- | Amazon Resource Name (ARN) of the fair share scheduling policy. Once a
-- job queue is created, the fair share scheduling policy can be replaced
-- but not removed. The format is
-- @aws:Partition:batch:Region:Account:scheduling-policy\/Name @. For
-- example,
-- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
updateJobQueue_schedulingPolicyArn :: Lens.Lens' UpdateJobQueue (Prelude.Maybe Prelude.Text)
updateJobQueue_schedulingPolicyArn = Lens.lens (\UpdateJobQueue' {schedulingPolicyArn} -> schedulingPolicyArn) (\s@UpdateJobQueue' {} a -> s {schedulingPolicyArn = a} :: UpdateJobQueue)

-- | The name or the Amazon Resource Name (ARN) of the job queue.
updateJobQueue_jobQueue :: Lens.Lens' UpdateJobQueue Prelude.Text
updateJobQueue_jobQueue = Lens.lens (\UpdateJobQueue' {jobQueue} -> jobQueue) (\s@UpdateJobQueue' {} a -> s {jobQueue = a} :: UpdateJobQueue)

instance Core.AWSRequest UpdateJobQueue where
  type
    AWSResponse UpdateJobQueue =
      UpdateJobQueueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobQueueResponse'
            Prelude.<$> (x Data..?> "jobQueueArn")
            Prelude.<*> (x Data..?> "jobQueueName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateJobQueue where
  hashWithSalt _salt UpdateJobQueue' {..} =
    _salt
      `Prelude.hashWithSalt` computeEnvironmentOrder
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` schedulingPolicyArn
      `Prelude.hashWithSalt` jobQueue

instance Prelude.NFData UpdateJobQueue where
  rnf UpdateJobQueue' {..} =
    Prelude.rnf computeEnvironmentOrder
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf schedulingPolicyArn
      `Prelude.seq` Prelude.rnf jobQueue

instance Data.ToHeaders UpdateJobQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateJobQueue where
  toJSON UpdateJobQueue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("computeEnvironmentOrder" Data..=)
              Prelude.<$> computeEnvironmentOrder,
            ("state" Data..=) Prelude.<$> state,
            ("priority" Data..=) Prelude.<$> priority,
            ("schedulingPolicyArn" Data..=)
              Prelude.<$> schedulingPolicyArn,
            Prelude.Just ("jobQueue" Data..= jobQueue)
          ]
      )

instance Data.ToPath UpdateJobQueue where
  toPath = Prelude.const "/v1/updatejobqueue"

instance Data.ToQuery UpdateJobQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJobQueueResponse' smart constructor.
data UpdateJobQueueResponse = UpdateJobQueueResponse'
  { -- | The Amazon Resource Name (ARN) of the job queue.
    jobQueueArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the job queue.
    jobQueueName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobQueueArn', 'updateJobQueueResponse_jobQueueArn' - The Amazon Resource Name (ARN) of the job queue.
--
-- 'jobQueueName', 'updateJobQueueResponse_jobQueueName' - The name of the job queue.
--
-- 'httpStatus', 'updateJobQueueResponse_httpStatus' - The response's http status code.
newUpdateJobQueueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateJobQueueResponse
newUpdateJobQueueResponse pHttpStatus_ =
  UpdateJobQueueResponse'
    { jobQueueArn =
        Prelude.Nothing,
      jobQueueName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the job queue.
updateJobQueueResponse_jobQueueArn :: Lens.Lens' UpdateJobQueueResponse (Prelude.Maybe Prelude.Text)
updateJobQueueResponse_jobQueueArn = Lens.lens (\UpdateJobQueueResponse' {jobQueueArn} -> jobQueueArn) (\s@UpdateJobQueueResponse' {} a -> s {jobQueueArn = a} :: UpdateJobQueueResponse)

-- | The name of the job queue.
updateJobQueueResponse_jobQueueName :: Lens.Lens' UpdateJobQueueResponse (Prelude.Maybe Prelude.Text)
updateJobQueueResponse_jobQueueName = Lens.lens (\UpdateJobQueueResponse' {jobQueueName} -> jobQueueName) (\s@UpdateJobQueueResponse' {} a -> s {jobQueueName = a} :: UpdateJobQueueResponse)

-- | The response's http status code.
updateJobQueueResponse_httpStatus :: Lens.Lens' UpdateJobQueueResponse Prelude.Int
updateJobQueueResponse_httpStatus = Lens.lens (\UpdateJobQueueResponse' {httpStatus} -> httpStatus) (\s@UpdateJobQueueResponse' {} a -> s {httpStatus = a} :: UpdateJobQueueResponse)

instance Prelude.NFData UpdateJobQueueResponse where
  rnf UpdateJobQueueResponse' {..} =
    Prelude.rnf jobQueueArn
      `Prelude.seq` Prelude.rnf jobQueueName
      `Prelude.seq` Prelude.rnf httpStatus
