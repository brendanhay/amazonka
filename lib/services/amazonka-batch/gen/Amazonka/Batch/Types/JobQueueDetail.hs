{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Batch.Types.JobQueueDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.JobQueueDetail where

import Amazonka.Batch.Types.ComputeEnvironmentOrder
import Amazonka.Batch.Types.JQState
import Amazonka.Batch.Types.JQStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the details for an Batch job queue.
--
-- /See:/ 'newJobQueueDetail' smart constructor.
data JobQueueDetail = JobQueueDetail'
  { -- | The Amazon Resource Name (ARN) of the scheduling policy. The format is
    -- @aws:@/@Partition@/@:batch:@/@Region@/@:@/@Account@/@:scheduling-policy\/@/@Name@/@ @.
    -- For example,
    -- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
    schedulingPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the job queue (for example, @CREATING@ or @VALID@).
    status :: Prelude.Maybe JQStatus,
    -- | A short, human-readable string to provide additional details for the
    -- current status of the job queue.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The tags that are applied to the job queue. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging your Batch resources>
    -- in /Batch User Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The job queue name.
    jobQueueName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the job queue.
    jobQueueArn :: Prelude.Text,
    -- | Describes the ability of the queue to accept new jobs. If the job queue
    -- state is @ENABLED@, it can accept jobs. If the job queue state is
    -- @DISABLED@, new jobs can\'t be added to the queue, but jobs already in
    -- the queue can finish.
    state :: JQState,
    -- | The priority of the job queue. Job queues with a higher priority (or a
    -- higher integer value for the @priority@ parameter) are evaluated first
    -- when associated with the same compute environment. Priority is
    -- determined in descending order. For example, a job queue with a priority
    -- value of @10@ is given scheduling preference over a job queue with a
    -- priority value of @1@. All of the compute environments must be either
    -- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@). EC2 and
    -- Fargate compute environments can\'t be mixed.
    priority :: Prelude.Int,
    -- | The compute environments that are attached to the job queue and the
    -- order that job placement is preferred. Compute environments are selected
    -- for job placement in ascending order.
    computeEnvironmentOrder :: [ComputeEnvironmentOrder]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobQueueDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schedulingPolicyArn', 'jobQueueDetail_schedulingPolicyArn' - The Amazon Resource Name (ARN) of the scheduling policy. The format is
-- @aws:@/@Partition@/@:batch:@/@Region@/@:@/@Account@/@:scheduling-policy\/@/@Name@/@ @.
-- For example,
-- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
--
-- 'status', 'jobQueueDetail_status' - The status of the job queue (for example, @CREATING@ or @VALID@).
--
-- 'statusReason', 'jobQueueDetail_statusReason' - A short, human-readable string to provide additional details for the
-- current status of the job queue.
--
-- 'tags', 'jobQueueDetail_tags' - The tags that are applied to the job queue. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging your Batch resources>
-- in /Batch User Guide/.
--
-- 'jobQueueName', 'jobQueueDetail_jobQueueName' - The job queue name.
--
-- 'jobQueueArn', 'jobQueueDetail_jobQueueArn' - The Amazon Resource Name (ARN) of the job queue.
--
-- 'state', 'jobQueueDetail_state' - Describes the ability of the queue to accept new jobs. If the job queue
-- state is @ENABLED@, it can accept jobs. If the job queue state is
-- @DISABLED@, new jobs can\'t be added to the queue, but jobs already in
-- the queue can finish.
--
-- 'priority', 'jobQueueDetail_priority' - The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order. For example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@). EC2 and
-- Fargate compute environments can\'t be mixed.
--
-- 'computeEnvironmentOrder', 'jobQueueDetail_computeEnvironmentOrder' - The compute environments that are attached to the job queue and the
-- order that job placement is preferred. Compute environments are selected
-- for job placement in ascending order.
newJobQueueDetail ::
  -- | 'jobQueueName'
  Prelude.Text ->
  -- | 'jobQueueArn'
  Prelude.Text ->
  -- | 'state'
  JQState ->
  -- | 'priority'
  Prelude.Int ->
  JobQueueDetail
newJobQueueDetail
  pJobQueueName_
  pJobQueueArn_
  pState_
  pPriority_ =
    JobQueueDetail'
      { schedulingPolicyArn =
          Prelude.Nothing,
        status = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        tags = Prelude.Nothing,
        jobQueueName = pJobQueueName_,
        jobQueueArn = pJobQueueArn_,
        state = pState_,
        priority = pPriority_,
        computeEnvironmentOrder = Prelude.mempty
      }

-- | The Amazon Resource Name (ARN) of the scheduling policy. The format is
-- @aws:@/@Partition@/@:batch:@/@Region@/@:@/@Account@/@:scheduling-policy\/@/@Name@/@ @.
-- For example,
-- @aws:aws:batch:us-west-2:123456789012:scheduling-policy\/MySchedulingPolicy@.
jobQueueDetail_schedulingPolicyArn :: Lens.Lens' JobQueueDetail (Prelude.Maybe Prelude.Text)
jobQueueDetail_schedulingPolicyArn = Lens.lens (\JobQueueDetail' {schedulingPolicyArn} -> schedulingPolicyArn) (\s@JobQueueDetail' {} a -> s {schedulingPolicyArn = a} :: JobQueueDetail)

-- | The status of the job queue (for example, @CREATING@ or @VALID@).
jobQueueDetail_status :: Lens.Lens' JobQueueDetail (Prelude.Maybe JQStatus)
jobQueueDetail_status = Lens.lens (\JobQueueDetail' {status} -> status) (\s@JobQueueDetail' {} a -> s {status = a} :: JobQueueDetail)

-- | A short, human-readable string to provide additional details for the
-- current status of the job queue.
jobQueueDetail_statusReason :: Lens.Lens' JobQueueDetail (Prelude.Maybe Prelude.Text)
jobQueueDetail_statusReason = Lens.lens (\JobQueueDetail' {statusReason} -> statusReason) (\s@JobQueueDetail' {} a -> s {statusReason = a} :: JobQueueDetail)

-- | The tags that are applied to the job queue. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging your Batch resources>
-- in /Batch User Guide/.
jobQueueDetail_tags :: Lens.Lens' JobQueueDetail (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobQueueDetail_tags = Lens.lens (\JobQueueDetail' {tags} -> tags) (\s@JobQueueDetail' {} a -> s {tags = a} :: JobQueueDetail) Prelude.. Lens.mapping Lens.coerced

-- | The job queue name.
jobQueueDetail_jobQueueName :: Lens.Lens' JobQueueDetail Prelude.Text
jobQueueDetail_jobQueueName = Lens.lens (\JobQueueDetail' {jobQueueName} -> jobQueueName) (\s@JobQueueDetail' {} a -> s {jobQueueName = a} :: JobQueueDetail)

-- | The Amazon Resource Name (ARN) of the job queue.
jobQueueDetail_jobQueueArn :: Lens.Lens' JobQueueDetail Prelude.Text
jobQueueDetail_jobQueueArn = Lens.lens (\JobQueueDetail' {jobQueueArn} -> jobQueueArn) (\s@JobQueueDetail' {} a -> s {jobQueueArn = a} :: JobQueueDetail)

-- | Describes the ability of the queue to accept new jobs. If the job queue
-- state is @ENABLED@, it can accept jobs. If the job queue state is
-- @DISABLED@, new jobs can\'t be added to the queue, but jobs already in
-- the queue can finish.
jobQueueDetail_state :: Lens.Lens' JobQueueDetail JQState
jobQueueDetail_state = Lens.lens (\JobQueueDetail' {state} -> state) (\s@JobQueueDetail' {} a -> s {state = a} :: JobQueueDetail)

-- | The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order. For example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@). EC2 and
-- Fargate compute environments can\'t be mixed.
jobQueueDetail_priority :: Lens.Lens' JobQueueDetail Prelude.Int
jobQueueDetail_priority = Lens.lens (\JobQueueDetail' {priority} -> priority) (\s@JobQueueDetail' {} a -> s {priority = a} :: JobQueueDetail)

-- | The compute environments that are attached to the job queue and the
-- order that job placement is preferred. Compute environments are selected
-- for job placement in ascending order.
jobQueueDetail_computeEnvironmentOrder :: Lens.Lens' JobQueueDetail [ComputeEnvironmentOrder]
jobQueueDetail_computeEnvironmentOrder = Lens.lens (\JobQueueDetail' {computeEnvironmentOrder} -> computeEnvironmentOrder) (\s@JobQueueDetail' {} a -> s {computeEnvironmentOrder = a} :: JobQueueDetail) Prelude.. Lens.coerced

instance Data.FromJSON JobQueueDetail where
  parseJSON =
    Data.withObject
      "JobQueueDetail"
      ( \x ->
          JobQueueDetail'
            Prelude.<$> (x Data..:? "schedulingPolicyArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusReason")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "jobQueueName")
            Prelude.<*> (x Data..: "jobQueueArn")
            Prelude.<*> (x Data..: "state")
            Prelude.<*> (x Data..: "priority")
            Prelude.<*> ( x
                            Data..:? "computeEnvironmentOrder"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable JobQueueDetail where
  hashWithSalt _salt JobQueueDetail' {..} =
    _salt
      `Prelude.hashWithSalt` schedulingPolicyArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` jobQueueName
      `Prelude.hashWithSalt` jobQueueArn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` computeEnvironmentOrder

instance Prelude.NFData JobQueueDetail where
  rnf JobQueueDetail' {..} =
    Prelude.rnf schedulingPolicyArn `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf statusReason `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf jobQueueName `Prelude.seq`
              Prelude.rnf jobQueueArn `Prelude.seq`
                Prelude.rnf state `Prelude.seq`
                  Prelude.rnf priority `Prelude.seq`
                    Prelude.rnf computeEnvironmentOrder
