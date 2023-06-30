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
-- Module      : Amazonka.Batch.CreateJobQueue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Batch job queue. When you create a job queue, you associate
-- one or more compute environments to the queue and assign an order of
-- preference for the compute environments.
--
-- You also set a priority to the job queue that determines the order that
-- the Batch scheduler places jobs onto its associated compute
-- environments. For example, if a compute environment is associated with
-- more than one job queue, the job queue with a higher priority is given
-- preference for scheduling jobs to that compute environment.
module Amazonka.Batch.CreateJobQueue
  ( -- * Creating a Request
    CreateJobQueue (..),
    newCreateJobQueue,

    -- * Request Lenses
    createJobQueue_schedulingPolicyArn,
    createJobQueue_state,
    createJobQueue_tags,
    createJobQueue_jobQueueName,
    createJobQueue_priority,
    createJobQueue_computeEnvironmentOrder,

    -- * Destructuring the Response
    CreateJobQueueResponse (..),
    newCreateJobQueueResponse,

    -- * Response Lenses
    createJobQueueResponse_httpStatus,
    createJobQueueResponse_jobQueueName,
    createJobQueueResponse_jobQueueArn,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @CreateJobQueue@.
--
-- /See:/ 'newCreateJobQueue' smart constructor.
data CreateJobQueue = CreateJobQueue'
  { -- | The Amazon Resource Name (ARN) of the fair share scheduling policy. If
    -- this parameter is specified, the job queue uses a fair share scheduling
    -- policy. If this parameter isn\'t specified, the job queue uses a first
    -- in, first out (FIFO) scheduling policy. After a job queue is created,
    -- you can replace but can\'t remove the fair share scheduling policy. The
    -- format is
    -- @aws:@/@Partition@/@:batch:@/@Region@/@:@/@Account@/@:scheduling-policy\/@/@Name@/@ @.
    -- An example is
    -- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
    schedulingPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the job queue. If the job queue state is @ENABLED@, it is
    -- able to accept jobs. If the job queue state is @DISABLED@, new jobs
    -- can\'t be added to the queue, but jobs already in the queue can finish.
    state :: Prelude.Maybe JQState,
    -- | The tags that you apply to the job queue to help you categorize and
    -- organize your resources. Each tag consists of a key and an optional
    -- value. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging your Batch resources>
    -- in /Batch User Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the job queue. It can be up to 128 letters long. It can
    -- contain uppercase and lowercase letters, numbers, hyphens (-), and
    -- underscores (_).
    jobQueueName :: Prelude.Text,
    -- | The priority of the job queue. Job queues with a higher priority (or a
    -- higher integer value for the @priority@ parameter) are evaluated first
    -- when associated with the same compute environment. Priority is
    -- determined in descending order. For example, a job queue with a priority
    -- value of @10@ is given scheduling preference over a job queue with a
    -- priority value of @1@. All of the compute environments must be either
    -- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
    -- Fargate compute environments can\'t be mixed.
    priority :: Prelude.Int,
    -- | The set of compute environments mapped to a job queue and their order
    -- relative to each other. The job scheduler uses this parameter to
    -- determine which compute environment runs a specific job. Compute
    -- environments must be in the @VALID@ state before you can associate them
    -- with a job queue. You can associate up to three compute environments
    -- with a job queue. All of the compute environments must be either EC2
    -- (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
    -- Fargate compute environments can\'t be mixed.
    --
    -- All compute environments that are associated with a job queue must share
    -- the same architecture. Batch doesn\'t support mixing compute environment
    -- architecture types in a single job queue.
    computeEnvironmentOrder :: [ComputeEnvironmentOrder]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schedulingPolicyArn', 'createJobQueue_schedulingPolicyArn' - The Amazon Resource Name (ARN) of the fair share scheduling policy. If
-- this parameter is specified, the job queue uses a fair share scheduling
-- policy. If this parameter isn\'t specified, the job queue uses a first
-- in, first out (FIFO) scheduling policy. After a job queue is created,
-- you can replace but can\'t remove the fair share scheduling policy. The
-- format is
-- @aws:@/@Partition@/@:batch:@/@Region@/@:@/@Account@/@:scheduling-policy\/@/@Name@/@ @.
-- An example is
-- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
--
-- 'state', 'createJobQueue_state' - The state of the job queue. If the job queue state is @ENABLED@, it is
-- able to accept jobs. If the job queue state is @DISABLED@, new jobs
-- can\'t be added to the queue, but jobs already in the queue can finish.
--
-- 'tags', 'createJobQueue_tags' - The tags that you apply to the job queue to help you categorize and
-- organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging your Batch resources>
-- in /Batch User Guide/.
--
-- 'jobQueueName', 'createJobQueue_jobQueueName' - The name of the job queue. It can be up to 128 letters long. It can
-- contain uppercase and lowercase letters, numbers, hyphens (-), and
-- underscores (_).
--
-- 'priority', 'createJobQueue_priority' - The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order. For example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
-- Fargate compute environments can\'t be mixed.
--
-- 'computeEnvironmentOrder', 'createJobQueue_computeEnvironmentOrder' - The set of compute environments mapped to a job queue and their order
-- relative to each other. The job scheduler uses this parameter to
-- determine which compute environment runs a specific job. Compute
-- environments must be in the @VALID@ state before you can associate them
-- with a job queue. You can associate up to three compute environments
-- with a job queue. All of the compute environments must be either EC2
-- (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
-- Fargate compute environments can\'t be mixed.
--
-- All compute environments that are associated with a job queue must share
-- the same architecture. Batch doesn\'t support mixing compute environment
-- architecture types in a single job queue.
newCreateJobQueue ::
  -- | 'jobQueueName'
  Prelude.Text ->
  -- | 'priority'
  Prelude.Int ->
  CreateJobQueue
newCreateJobQueue pJobQueueName_ pPriority_ =
  CreateJobQueue'
    { schedulingPolicyArn =
        Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      jobQueueName = pJobQueueName_,
      priority = pPriority_,
      computeEnvironmentOrder = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the fair share scheduling policy. If
-- this parameter is specified, the job queue uses a fair share scheduling
-- policy. If this parameter isn\'t specified, the job queue uses a first
-- in, first out (FIFO) scheduling policy. After a job queue is created,
-- you can replace but can\'t remove the fair share scheduling policy. The
-- format is
-- @aws:@/@Partition@/@:batch:@/@Region@/@:@/@Account@/@:scheduling-policy\/@/@Name@/@ @.
-- An example is
-- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
createJobQueue_schedulingPolicyArn :: Lens.Lens' CreateJobQueue (Prelude.Maybe Prelude.Text)
createJobQueue_schedulingPolicyArn = Lens.lens (\CreateJobQueue' {schedulingPolicyArn} -> schedulingPolicyArn) (\s@CreateJobQueue' {} a -> s {schedulingPolicyArn = a} :: CreateJobQueue)

-- | The state of the job queue. If the job queue state is @ENABLED@, it is
-- able to accept jobs. If the job queue state is @DISABLED@, new jobs
-- can\'t be added to the queue, but jobs already in the queue can finish.
createJobQueue_state :: Lens.Lens' CreateJobQueue (Prelude.Maybe JQState)
createJobQueue_state = Lens.lens (\CreateJobQueue' {state} -> state) (\s@CreateJobQueue' {} a -> s {state = a} :: CreateJobQueue)

-- | The tags that you apply to the job queue to help you categorize and
-- organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging your Batch resources>
-- in /Batch User Guide/.
createJobQueue_tags :: Lens.Lens' CreateJobQueue (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJobQueue_tags = Lens.lens (\CreateJobQueue' {tags} -> tags) (\s@CreateJobQueue' {} a -> s {tags = a} :: CreateJobQueue) Prelude.. Lens.mapping Lens.coerced

-- | The name of the job queue. It can be up to 128 letters long. It can
-- contain uppercase and lowercase letters, numbers, hyphens (-), and
-- underscores (_).
createJobQueue_jobQueueName :: Lens.Lens' CreateJobQueue Prelude.Text
createJobQueue_jobQueueName = Lens.lens (\CreateJobQueue' {jobQueueName} -> jobQueueName) (\s@CreateJobQueue' {} a -> s {jobQueueName = a} :: CreateJobQueue)

-- | The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order. For example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
-- Fargate compute environments can\'t be mixed.
createJobQueue_priority :: Lens.Lens' CreateJobQueue Prelude.Int
createJobQueue_priority = Lens.lens (\CreateJobQueue' {priority} -> priority) (\s@CreateJobQueue' {} a -> s {priority = a} :: CreateJobQueue)

-- | The set of compute environments mapped to a job queue and their order
-- relative to each other. The job scheduler uses this parameter to
-- determine which compute environment runs a specific job. Compute
-- environments must be in the @VALID@ state before you can associate them
-- with a job queue. You can associate up to three compute environments
-- with a job queue. All of the compute environments must be either EC2
-- (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
-- Fargate compute environments can\'t be mixed.
--
-- All compute environments that are associated with a job queue must share
-- the same architecture. Batch doesn\'t support mixing compute environment
-- architecture types in a single job queue.
createJobQueue_computeEnvironmentOrder :: Lens.Lens' CreateJobQueue [ComputeEnvironmentOrder]
createJobQueue_computeEnvironmentOrder = Lens.lens (\CreateJobQueue' {computeEnvironmentOrder} -> computeEnvironmentOrder) (\s@CreateJobQueue' {} a -> s {computeEnvironmentOrder = a} :: CreateJobQueue) Prelude.. Lens.coerced

instance Core.AWSRequest CreateJobQueue where
  type
    AWSResponse CreateJobQueue =
      CreateJobQueueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobQueueResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobQueueName")
            Prelude.<*> (x Data..:> "jobQueueArn")
      )

instance Prelude.Hashable CreateJobQueue where
  hashWithSalt _salt CreateJobQueue' {..} =
    _salt
      `Prelude.hashWithSalt` schedulingPolicyArn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` jobQueueName
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` computeEnvironmentOrder

instance Prelude.NFData CreateJobQueue where
  rnf CreateJobQueue' {..} =
    Prelude.rnf schedulingPolicyArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf jobQueueName
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf computeEnvironmentOrder

instance Data.ToHeaders CreateJobQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateJobQueue where
  toJSON CreateJobQueue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("schedulingPolicyArn" Data..=)
              Prelude.<$> schedulingPolicyArn,
            ("state" Data..=) Prelude.<$> state,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("jobQueueName" Data..= jobQueueName),
            Prelude.Just ("priority" Data..= priority),
            Prelude.Just
              ( "computeEnvironmentOrder"
                  Data..= computeEnvironmentOrder
              )
          ]
      )

instance Data.ToPath CreateJobQueue where
  toPath = Prelude.const "/v1/createjobqueue"

instance Data.ToQuery CreateJobQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobQueueResponse' smart constructor.
data CreateJobQueueResponse = CreateJobQueueResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the job queue.
    jobQueueName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the job queue.
    jobQueueArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createJobQueueResponse_httpStatus' - The response's http status code.
--
-- 'jobQueueName', 'createJobQueueResponse_jobQueueName' - The name of the job queue.
--
-- 'jobQueueArn', 'createJobQueueResponse_jobQueueArn' - The Amazon Resource Name (ARN) of the job queue.
newCreateJobQueueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobQueueName'
  Prelude.Text ->
  -- | 'jobQueueArn'
  Prelude.Text ->
  CreateJobQueueResponse
newCreateJobQueueResponse
  pHttpStatus_
  pJobQueueName_
  pJobQueueArn_ =
    CreateJobQueueResponse'
      { httpStatus = pHttpStatus_,
        jobQueueName = pJobQueueName_,
        jobQueueArn = pJobQueueArn_
      }

-- | The response's http status code.
createJobQueueResponse_httpStatus :: Lens.Lens' CreateJobQueueResponse Prelude.Int
createJobQueueResponse_httpStatus = Lens.lens (\CreateJobQueueResponse' {httpStatus} -> httpStatus) (\s@CreateJobQueueResponse' {} a -> s {httpStatus = a} :: CreateJobQueueResponse)

-- | The name of the job queue.
createJobQueueResponse_jobQueueName :: Lens.Lens' CreateJobQueueResponse Prelude.Text
createJobQueueResponse_jobQueueName = Lens.lens (\CreateJobQueueResponse' {jobQueueName} -> jobQueueName) (\s@CreateJobQueueResponse' {} a -> s {jobQueueName = a} :: CreateJobQueueResponse)

-- | The Amazon Resource Name (ARN) of the job queue.
createJobQueueResponse_jobQueueArn :: Lens.Lens' CreateJobQueueResponse Prelude.Text
createJobQueueResponse_jobQueueArn = Lens.lens (\CreateJobQueueResponse' {jobQueueArn} -> jobQueueArn) (\s@CreateJobQueueResponse' {} a -> s {jobQueueArn = a} :: CreateJobQueueResponse)

instance Prelude.NFData CreateJobQueueResponse where
  rnf CreateJobQueueResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobQueueName
      `Prelude.seq` Prelude.rnf jobQueueArn
