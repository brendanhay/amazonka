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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    computeEnvironmentOrder :: Prelude.Maybe [ComputeEnvironmentOrder],
    -- | The priority of the job queue. Job queues with a higher priority (or a
    -- higher integer value for the @priority@ parameter) are evaluated first
    -- when associated with the same compute environment. Priority is
    -- determined in descending order, for example, a job queue with a priority
    -- value of @10@ is given scheduling preference over a job queue with a
    -- priority value of @1@. All of the compute environments must be either
    -- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
    -- Fargate compute environments cannot be mixed.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Describes the queue\'s ability to accept new jobs. If the job queue
    -- state is @ENABLED@, it is able to accept jobs. If the job queue state is
    -- @DISABLED@, new jobs cannot be added to the queue, but jobs already in
    -- the queue can finish.
    state :: Prelude.Maybe JQState,
    -- | The name or the Amazon Resource Name (ARN) of the job queue.
    jobQueue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateJobQueue
newUpdateJobQueue pJobQueue_ =
  UpdateJobQueue'
    { computeEnvironmentOrder =
        Prelude.Nothing,
      priority = Prelude.Nothing,
      state = Prelude.Nothing,
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
updateJobQueue_computeEnvironmentOrder :: Lens.Lens' UpdateJobQueue (Prelude.Maybe [ComputeEnvironmentOrder])
updateJobQueue_computeEnvironmentOrder = Lens.lens (\UpdateJobQueue' {computeEnvironmentOrder} -> computeEnvironmentOrder) (\s@UpdateJobQueue' {} a -> s {computeEnvironmentOrder = a} :: UpdateJobQueue) Prelude.. Lens.mapping Prelude._Coerce

-- | The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order, for example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
-- Fargate compute environments cannot be mixed.
updateJobQueue_priority :: Lens.Lens' UpdateJobQueue (Prelude.Maybe Prelude.Int)
updateJobQueue_priority = Lens.lens (\UpdateJobQueue' {priority} -> priority) (\s@UpdateJobQueue' {} a -> s {priority = a} :: UpdateJobQueue)

-- | Describes the queue\'s ability to accept new jobs. If the job queue
-- state is @ENABLED@, it is able to accept jobs. If the job queue state is
-- @DISABLED@, new jobs cannot be added to the queue, but jobs already in
-- the queue can finish.
updateJobQueue_state :: Lens.Lens' UpdateJobQueue (Prelude.Maybe JQState)
updateJobQueue_state = Lens.lens (\UpdateJobQueue' {state} -> state) (\s@UpdateJobQueue' {} a -> s {state = a} :: UpdateJobQueue)

-- | The name or the Amazon Resource Name (ARN) of the job queue.
updateJobQueue_jobQueue :: Lens.Lens' UpdateJobQueue Prelude.Text
updateJobQueue_jobQueue = Lens.lens (\UpdateJobQueue' {jobQueue} -> jobQueue) (\s@UpdateJobQueue' {} a -> s {jobQueue = a} :: UpdateJobQueue)

instance Prelude.AWSRequest UpdateJobQueue where
  type Rs UpdateJobQueue = UpdateJobQueueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobQueueResponse'
            Prelude.<$> (x Prelude..?> "jobQueueName")
            Prelude.<*> (x Prelude..?> "jobQueueArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateJobQueue

instance Prelude.NFData UpdateJobQueue

instance Prelude.ToHeaders UpdateJobQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateJobQueue where
  toJSON UpdateJobQueue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("computeEnvironmentOrder" Prelude..=)
              Prelude.<$> computeEnvironmentOrder,
            ("priority" Prelude..=) Prelude.<$> priority,
            ("state" Prelude..=) Prelude.<$> state,
            Prelude.Just ("jobQueue" Prelude..= jobQueue)
          ]
      )

instance Prelude.ToPath UpdateJobQueue where
  toPath = Prelude.const "/v1/updatejobqueue"

instance Prelude.ToQuery UpdateJobQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJobQueueResponse' smart constructor.
data UpdateJobQueueResponse = UpdateJobQueueResponse'
  { -- | The name of the job queue.
    jobQueueName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the job queue.
    jobQueueArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateJobQueueResponse
newUpdateJobQueueResponse pHttpStatus_ =
  UpdateJobQueueResponse'
    { jobQueueName =
        Prelude.Nothing,
      jobQueueArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the job queue.
updateJobQueueResponse_jobQueueName :: Lens.Lens' UpdateJobQueueResponse (Prelude.Maybe Prelude.Text)
updateJobQueueResponse_jobQueueName = Lens.lens (\UpdateJobQueueResponse' {jobQueueName} -> jobQueueName) (\s@UpdateJobQueueResponse' {} a -> s {jobQueueName = a} :: UpdateJobQueueResponse)

-- | The Amazon Resource Name (ARN) of the job queue.
updateJobQueueResponse_jobQueueArn :: Lens.Lens' UpdateJobQueueResponse (Prelude.Maybe Prelude.Text)
updateJobQueueResponse_jobQueueArn = Lens.lens (\UpdateJobQueueResponse' {jobQueueArn} -> jobQueueArn) (\s@UpdateJobQueueResponse' {} a -> s {jobQueueArn = a} :: UpdateJobQueueResponse)

-- | The response's http status code.
updateJobQueueResponse_httpStatus :: Lens.Lens' UpdateJobQueueResponse Prelude.Int
updateJobQueueResponse_httpStatus = Lens.lens (\UpdateJobQueueResponse' {httpStatus} -> httpStatus) (\s@UpdateJobQueueResponse' {} a -> s {httpStatus = a} :: UpdateJobQueueResponse)

instance Prelude.NFData UpdateJobQueueResponse
