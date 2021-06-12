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
-- Module      : Network.AWS.Batch.UpdateJobQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a job queue.
module Network.AWS.Batch.UpdateJobQueue
  ( -- * Creating a Request
    UpdateJobQueue (..),
    newUpdateJobQueue,

    -- * Request Lenses
    updateJobQueue_computeEnvironmentOrder,
    updateJobQueue_priority,
    updateJobQueue_state,
    updateJobQueue_jobQueue,

    -- * Destructuring the Response
    UpdateJobQueueResponse (..),
    newUpdateJobQueueResponse,

    -- * Response Lenses
    updateJobQueueResponse_jobQueueName,
    updateJobQueueResponse_jobQueueArn,
    updateJobQueueResponse_httpStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @UpdateJobQueue@.
--
-- /See:/ 'newUpdateJobQueue' smart constructor.
data UpdateJobQueue = UpdateJobQueue'
  { -- | Details the set of compute environments mapped to a job queue and their
    -- order relative to each other. This is one of the parameters used by the
    -- job scheduler to determine which compute environment should run a given
    -- job. Compute environments must be in the @VALID@ state before you can
    -- associate them with a job queue. All of the compute environments must be
    -- either EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@);
    -- EC2 and Fargate compute environments can\'t be mixed.
    --
    -- All compute environments that are associated with a job queue must share
    -- the same architecture. AWS Batch doesn\'t support mixing compute
    -- environment architecture types in a single job queue.
    computeEnvironmentOrder :: Core.Maybe [ComputeEnvironmentOrder],
    -- | The priority of the job queue. Job queues with a higher priority (or a
    -- higher integer value for the @priority@ parameter) are evaluated first
    -- when associated with the same compute environment. Priority is
    -- determined in descending order, for example, a job queue with a priority
    -- value of @10@ is given scheduling preference over a job queue with a
    -- priority value of @1@. All of the compute environments must be either
    -- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
    -- Fargate compute environments cannot be mixed.
    priority :: Core.Maybe Core.Int,
    -- | Describes the queue\'s ability to accept new jobs. If the job queue
    -- state is @ENABLED@, it is able to accept jobs. If the job queue state is
    -- @DISABLED@, new jobs cannot be added to the queue, but jobs already in
    -- the queue can finish.
    state :: Core.Maybe JQState,
    -- | The name or the Amazon Resource Name (ARN) of the job queue.
    jobQueue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- job scheduler to determine which compute environment should run a given
-- job. Compute environments must be in the @VALID@ state before you can
-- associate them with a job queue. All of the compute environments must be
-- either EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@);
-- EC2 and Fargate compute environments can\'t be mixed.
--
-- All compute environments that are associated with a job queue must share
-- the same architecture. AWS Batch doesn\'t support mixing compute
-- environment architecture types in a single job queue.
--
-- 'priority', 'updateJobQueue_priority' - The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order, for example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
-- Fargate compute environments cannot be mixed.
--
-- 'state', 'updateJobQueue_state' - Describes the queue\'s ability to accept new jobs. If the job queue
-- state is @ENABLED@, it is able to accept jobs. If the job queue state is
-- @DISABLED@, new jobs cannot be added to the queue, but jobs already in
-- the queue can finish.
--
-- 'jobQueue', 'updateJobQueue_jobQueue' - The name or the Amazon Resource Name (ARN) of the job queue.
newUpdateJobQueue ::
  -- | 'jobQueue'
  Core.Text ->
  UpdateJobQueue
newUpdateJobQueue pJobQueue_ =
  UpdateJobQueue'
    { computeEnvironmentOrder =
        Core.Nothing,
      priority = Core.Nothing,
      state = Core.Nothing,
      jobQueue = pJobQueue_
    }

-- | Details the set of compute environments mapped to a job queue and their
-- order relative to each other. This is one of the parameters used by the
-- job scheduler to determine which compute environment should run a given
-- job. Compute environments must be in the @VALID@ state before you can
-- associate them with a job queue. All of the compute environments must be
-- either EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@);
-- EC2 and Fargate compute environments can\'t be mixed.
--
-- All compute environments that are associated with a job queue must share
-- the same architecture. AWS Batch doesn\'t support mixing compute
-- environment architecture types in a single job queue.
updateJobQueue_computeEnvironmentOrder :: Lens.Lens' UpdateJobQueue (Core.Maybe [ComputeEnvironmentOrder])
updateJobQueue_computeEnvironmentOrder = Lens.lens (\UpdateJobQueue' {computeEnvironmentOrder} -> computeEnvironmentOrder) (\s@UpdateJobQueue' {} a -> s {computeEnvironmentOrder = a} :: UpdateJobQueue) Core.. Lens.mapping Lens._Coerce

-- | The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order, for example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
-- Fargate compute environments cannot be mixed.
updateJobQueue_priority :: Lens.Lens' UpdateJobQueue (Core.Maybe Core.Int)
updateJobQueue_priority = Lens.lens (\UpdateJobQueue' {priority} -> priority) (\s@UpdateJobQueue' {} a -> s {priority = a} :: UpdateJobQueue)

-- | Describes the queue\'s ability to accept new jobs. If the job queue
-- state is @ENABLED@, it is able to accept jobs. If the job queue state is
-- @DISABLED@, new jobs cannot be added to the queue, but jobs already in
-- the queue can finish.
updateJobQueue_state :: Lens.Lens' UpdateJobQueue (Core.Maybe JQState)
updateJobQueue_state = Lens.lens (\UpdateJobQueue' {state} -> state) (\s@UpdateJobQueue' {} a -> s {state = a} :: UpdateJobQueue)

-- | The name or the Amazon Resource Name (ARN) of the job queue.
updateJobQueue_jobQueue :: Lens.Lens' UpdateJobQueue Core.Text
updateJobQueue_jobQueue = Lens.lens (\UpdateJobQueue' {jobQueue} -> jobQueue) (\s@UpdateJobQueue' {} a -> s {jobQueue = a} :: UpdateJobQueue)

instance Core.AWSRequest UpdateJobQueue where
  type
    AWSResponse UpdateJobQueue =
      UpdateJobQueueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobQueueResponse'
            Core.<$> (x Core..?> "jobQueueName")
            Core.<*> (x Core..?> "jobQueueArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateJobQueue

instance Core.NFData UpdateJobQueue

instance Core.ToHeaders UpdateJobQueue where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateJobQueue where
  toJSON UpdateJobQueue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("computeEnvironmentOrder" Core..=)
              Core.<$> computeEnvironmentOrder,
            ("priority" Core..=) Core.<$> priority,
            ("state" Core..=) Core.<$> state,
            Core.Just ("jobQueue" Core..= jobQueue)
          ]
      )

instance Core.ToPath UpdateJobQueue where
  toPath = Core.const "/v1/updatejobqueue"

instance Core.ToQuery UpdateJobQueue where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateJobQueueResponse' smart constructor.
data UpdateJobQueueResponse = UpdateJobQueueResponse'
  { -- | The name of the job queue.
    jobQueueName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the job queue.
    jobQueueArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateJobQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobQueueName', 'updateJobQueueResponse_jobQueueName' - The name of the job queue.
--
-- 'jobQueueArn', 'updateJobQueueResponse_jobQueueArn' - The Amazon Resource Name (ARN) of the job queue.
--
-- 'httpStatus', 'updateJobQueueResponse_httpStatus' - The response's http status code.
newUpdateJobQueueResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateJobQueueResponse
newUpdateJobQueueResponse pHttpStatus_ =
  UpdateJobQueueResponse'
    { jobQueueName =
        Core.Nothing,
      jobQueueArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the job queue.
updateJobQueueResponse_jobQueueName :: Lens.Lens' UpdateJobQueueResponse (Core.Maybe Core.Text)
updateJobQueueResponse_jobQueueName = Lens.lens (\UpdateJobQueueResponse' {jobQueueName} -> jobQueueName) (\s@UpdateJobQueueResponse' {} a -> s {jobQueueName = a} :: UpdateJobQueueResponse)

-- | The Amazon Resource Name (ARN) of the job queue.
updateJobQueueResponse_jobQueueArn :: Lens.Lens' UpdateJobQueueResponse (Core.Maybe Core.Text)
updateJobQueueResponse_jobQueueArn = Lens.lens (\UpdateJobQueueResponse' {jobQueueArn} -> jobQueueArn) (\s@UpdateJobQueueResponse' {} a -> s {jobQueueArn = a} :: UpdateJobQueueResponse)

-- | The response's http status code.
updateJobQueueResponse_httpStatus :: Lens.Lens' UpdateJobQueueResponse Core.Int
updateJobQueueResponse_httpStatus = Lens.lens (\UpdateJobQueueResponse' {httpStatus} -> httpStatus) (\s@UpdateJobQueueResponse' {} a -> s {httpStatus = a} :: UpdateJobQueueResponse)

instance Core.NFData UpdateJobQueueResponse
