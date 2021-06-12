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
-- Module      : Network.AWS.Batch.Types.JobQueueDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobQueueDetail where

import Network.AWS.Batch.Types.ComputeEnvironmentOrder
import Network.AWS.Batch.Types.JQState
import Network.AWS.Batch.Types.JQStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing the details of an AWS Batch job queue.
--
-- /See:/ 'newJobQueueDetail' smart constructor.
data JobQueueDetail = JobQueueDetail'
  { -- | The status of the job queue (for example, @CREATING@ or @VALID@).
    status :: Core.Maybe JQStatus,
    -- | The tags applied to the job queue. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging your AWS Batch resources>
    -- in /AWS Batch User Guide/.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A short, human-readable string to provide additional details about the
    -- current status of the job queue.
    statusReason :: Core.Maybe Core.Text,
    -- | The name of the job queue.
    jobQueueName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the job queue.
    jobQueueArn :: Core.Text,
    -- | Describes the ability of the queue to accept new jobs. If the job queue
    -- state is @ENABLED@, it\'s able to accept jobs. If the job queue state is
    -- @DISABLED@, new jobs can\'t be added to the queue, but jobs already in
    -- the queue can finish.
    state :: JQState,
    -- | The priority of the job queue. Job queues with a higher priority (or a
    -- higher integer value for the @priority@ parameter) are evaluated first
    -- when associated with the same compute environment. Priority is
    -- determined in descending order, for example, a job queue with a priority
    -- value of @10@ is given scheduling preference over a job queue with a
    -- priority value of @1@. All of the compute environments must be either
    -- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
    -- Fargate compute environments cannot be mixed.
    priority :: Core.Int,
    -- | The compute environments that are attached to the job queue and the
    -- order that job placement is preferred. Compute environments are selected
    -- for job placement in ascending order.
    computeEnvironmentOrder :: [ComputeEnvironmentOrder]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobQueueDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'jobQueueDetail_status' - The status of the job queue (for example, @CREATING@ or @VALID@).
--
-- 'tags', 'jobQueueDetail_tags' - The tags applied to the job queue. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging your AWS Batch resources>
-- in /AWS Batch User Guide/.
--
-- 'statusReason', 'jobQueueDetail_statusReason' - A short, human-readable string to provide additional details about the
-- current status of the job queue.
--
-- 'jobQueueName', 'jobQueueDetail_jobQueueName' - The name of the job queue.
--
-- 'jobQueueArn', 'jobQueueDetail_jobQueueArn' - The Amazon Resource Name (ARN) of the job queue.
--
-- 'state', 'jobQueueDetail_state' - Describes the ability of the queue to accept new jobs. If the job queue
-- state is @ENABLED@, it\'s able to accept jobs. If the job queue state is
-- @DISABLED@, new jobs can\'t be added to the queue, but jobs already in
-- the queue can finish.
--
-- 'priority', 'jobQueueDetail_priority' - The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order, for example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
-- Fargate compute environments cannot be mixed.
--
-- 'computeEnvironmentOrder', 'jobQueueDetail_computeEnvironmentOrder' - The compute environments that are attached to the job queue and the
-- order that job placement is preferred. Compute environments are selected
-- for job placement in ascending order.
newJobQueueDetail ::
  -- | 'jobQueueName'
  Core.Text ->
  -- | 'jobQueueArn'
  Core.Text ->
  -- | 'state'
  JQState ->
  -- | 'priority'
  Core.Int ->
  JobQueueDetail
newJobQueueDetail
  pJobQueueName_
  pJobQueueArn_
  pState_
  pPriority_ =
    JobQueueDetail'
      { status = Core.Nothing,
        tags = Core.Nothing,
        statusReason = Core.Nothing,
        jobQueueName = pJobQueueName_,
        jobQueueArn = pJobQueueArn_,
        state = pState_,
        priority = pPriority_,
        computeEnvironmentOrder = Core.mempty
      }

-- | The status of the job queue (for example, @CREATING@ or @VALID@).
jobQueueDetail_status :: Lens.Lens' JobQueueDetail (Core.Maybe JQStatus)
jobQueueDetail_status = Lens.lens (\JobQueueDetail' {status} -> status) (\s@JobQueueDetail' {} a -> s {status = a} :: JobQueueDetail)

-- | The tags applied to the job queue. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging your AWS Batch resources>
-- in /AWS Batch User Guide/.
jobQueueDetail_tags :: Lens.Lens' JobQueueDetail (Core.Maybe (Core.HashMap Core.Text Core.Text))
jobQueueDetail_tags = Lens.lens (\JobQueueDetail' {tags} -> tags) (\s@JobQueueDetail' {} a -> s {tags = a} :: JobQueueDetail) Core.. Lens.mapping Lens._Coerce

-- | A short, human-readable string to provide additional details about the
-- current status of the job queue.
jobQueueDetail_statusReason :: Lens.Lens' JobQueueDetail (Core.Maybe Core.Text)
jobQueueDetail_statusReason = Lens.lens (\JobQueueDetail' {statusReason} -> statusReason) (\s@JobQueueDetail' {} a -> s {statusReason = a} :: JobQueueDetail)

-- | The name of the job queue.
jobQueueDetail_jobQueueName :: Lens.Lens' JobQueueDetail Core.Text
jobQueueDetail_jobQueueName = Lens.lens (\JobQueueDetail' {jobQueueName} -> jobQueueName) (\s@JobQueueDetail' {} a -> s {jobQueueName = a} :: JobQueueDetail)

-- | The Amazon Resource Name (ARN) of the job queue.
jobQueueDetail_jobQueueArn :: Lens.Lens' JobQueueDetail Core.Text
jobQueueDetail_jobQueueArn = Lens.lens (\JobQueueDetail' {jobQueueArn} -> jobQueueArn) (\s@JobQueueDetail' {} a -> s {jobQueueArn = a} :: JobQueueDetail)

-- | Describes the ability of the queue to accept new jobs. If the job queue
-- state is @ENABLED@, it\'s able to accept jobs. If the job queue state is
-- @DISABLED@, new jobs can\'t be added to the queue, but jobs already in
-- the queue can finish.
jobQueueDetail_state :: Lens.Lens' JobQueueDetail JQState
jobQueueDetail_state = Lens.lens (\JobQueueDetail' {state} -> state) (\s@JobQueueDetail' {} a -> s {state = a} :: JobQueueDetail)

-- | The priority of the job queue. Job queues with a higher priority (or a
-- higher integer value for the @priority@ parameter) are evaluated first
-- when associated with the same compute environment. Priority is
-- determined in descending order, for example, a job queue with a priority
-- value of @10@ is given scheduling preference over a job queue with a
-- priority value of @1@. All of the compute environments must be either
-- EC2 (@EC2@ or @SPOT@) or Fargate (@FARGATE@ or @FARGATE_SPOT@); EC2 and
-- Fargate compute environments cannot be mixed.
jobQueueDetail_priority :: Lens.Lens' JobQueueDetail Core.Int
jobQueueDetail_priority = Lens.lens (\JobQueueDetail' {priority} -> priority) (\s@JobQueueDetail' {} a -> s {priority = a} :: JobQueueDetail)

-- | The compute environments that are attached to the job queue and the
-- order that job placement is preferred. Compute environments are selected
-- for job placement in ascending order.
jobQueueDetail_computeEnvironmentOrder :: Lens.Lens' JobQueueDetail [ComputeEnvironmentOrder]
jobQueueDetail_computeEnvironmentOrder = Lens.lens (\JobQueueDetail' {computeEnvironmentOrder} -> computeEnvironmentOrder) (\s@JobQueueDetail' {} a -> s {computeEnvironmentOrder = a} :: JobQueueDetail) Core.. Lens._Coerce

instance Core.FromJSON JobQueueDetail where
  parseJSON =
    Core.withObject
      "JobQueueDetail"
      ( \x ->
          JobQueueDetail'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "statusReason")
            Core.<*> (x Core..: "jobQueueName")
            Core.<*> (x Core..: "jobQueueArn")
            Core.<*> (x Core..: "state")
            Core.<*> (x Core..: "priority")
            Core.<*> ( x Core..:? "computeEnvironmentOrder"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable JobQueueDetail

instance Core.NFData JobQueueDetail
