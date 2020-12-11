-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobQueueDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobQueueDetail
  ( JobQueueDetail (..),

    -- * Smart constructor
    mkJobQueueDetail,

    -- * Lenses
    jqdStatus,
    jqdStatusReason,
    jqdTags,
    jqdJobQueueName,
    jqdJobQueueARN,
    jqdState,
    jqdPriority,
    jqdComputeEnvironmentOrder,
  )
where

import Network.AWS.Batch.Types.ComputeEnvironmentOrder
import Network.AWS.Batch.Types.JQState
import Network.AWS.Batch.Types.JQStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the details of an AWS Batch job queue.
--
-- /See:/ 'mkJobQueueDetail' smart constructor.
data JobQueueDetail = JobQueueDetail'
  { status ::
      Lude.Maybe JQStatus,
    statusReason :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    jobQueueName :: Lude.Text,
    jobQueueARN :: Lude.Text,
    state :: JQState,
    priority :: Lude.Int,
    computeEnvironmentOrder :: [ComputeEnvironmentOrder]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobQueueDetail' with the minimum fields required to make a request.
--
-- * 'computeEnvironmentOrder' - The compute environments that are attached to the job queue and the order in which job placement is preferred. Compute environments are selected for job placement in ascending order.
-- * 'jobQueueARN' - The Amazon Resource Name (ARN) of the job queue.
-- * 'jobQueueName' - The name of the job queue.
-- * 'priority' - The priority of the job queue.
-- * 'state' - Describes the ability of the queue to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
-- * 'status' - The status of the job queue (for example, @CREATING@ or @VALID@ ).
-- * 'statusReason' - A short, human-readable string to provide additional details about the current status of the job queue.
-- * 'tags' - The tags applied to the job queue.
mkJobQueueDetail ::
  -- | 'jobQueueName'
  Lude.Text ->
  -- | 'jobQueueARN'
  Lude.Text ->
  -- | 'state'
  JQState ->
  -- | 'priority'
  Lude.Int ->
  JobQueueDetail
mkJobQueueDetail pJobQueueName_ pJobQueueARN_ pState_ pPriority_ =
  JobQueueDetail'
    { status = Lude.Nothing,
      statusReason = Lude.Nothing,
      tags = Lude.Nothing,
      jobQueueName = pJobQueueName_,
      jobQueueARN = pJobQueueARN_,
      state = pState_,
      priority = pPriority_,
      computeEnvironmentOrder = Lude.mempty
    }

-- | The status of the job queue (for example, @CREATING@ or @VALID@ ).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdStatus :: Lens.Lens' JobQueueDetail (Lude.Maybe JQStatus)
jqdStatus = Lens.lens (status :: JobQueueDetail -> Lude.Maybe JQStatus) (\s a -> s {status = a} :: JobQueueDetail)
{-# DEPRECATED jqdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A short, human-readable string to provide additional details about the current status of the job queue.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdStatusReason :: Lens.Lens' JobQueueDetail (Lude.Maybe Lude.Text)
jqdStatusReason = Lens.lens (statusReason :: JobQueueDetail -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: JobQueueDetail)
{-# DEPRECATED jqdStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The tags applied to the job queue.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdTags :: Lens.Lens' JobQueueDetail (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jqdTags = Lens.lens (tags :: JobQueueDetail -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: JobQueueDetail)
{-# DEPRECATED jqdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the job queue.
--
-- /Note:/ Consider using 'jobQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdJobQueueName :: Lens.Lens' JobQueueDetail Lude.Text
jqdJobQueueName = Lens.lens (jobQueueName :: JobQueueDetail -> Lude.Text) (\s a -> s {jobQueueName = a} :: JobQueueDetail)
{-# DEPRECATED jqdJobQueueName "Use generic-lens or generic-optics with 'jobQueueName' instead." #-}

-- | The Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueueARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdJobQueueARN :: Lens.Lens' JobQueueDetail Lude.Text
jqdJobQueueARN = Lens.lens (jobQueueARN :: JobQueueDetail -> Lude.Text) (\s a -> s {jobQueueARN = a} :: JobQueueDetail)
{-# DEPRECATED jqdJobQueueARN "Use generic-lens or generic-optics with 'jobQueueARN' instead." #-}

-- | Describes the ability of the queue to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdState :: Lens.Lens' JobQueueDetail JQState
jqdState = Lens.lens (state :: JobQueueDetail -> JQState) (\s a -> s {state = a} :: JobQueueDetail)
{-# DEPRECATED jqdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The priority of the job queue.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdPriority :: Lens.Lens' JobQueueDetail Lude.Int
jqdPriority = Lens.lens (priority :: JobQueueDetail -> Lude.Int) (\s a -> s {priority = a} :: JobQueueDetail)
{-# DEPRECATED jqdPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The compute environments that are attached to the job queue and the order in which job placement is preferred. Compute environments are selected for job placement in ascending order.
--
-- /Note:/ Consider using 'computeEnvironmentOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdComputeEnvironmentOrder :: Lens.Lens' JobQueueDetail [ComputeEnvironmentOrder]
jqdComputeEnvironmentOrder = Lens.lens (computeEnvironmentOrder :: JobQueueDetail -> [ComputeEnvironmentOrder]) (\s a -> s {computeEnvironmentOrder = a} :: JobQueueDetail)
{-# DEPRECATED jqdComputeEnvironmentOrder "Use generic-lens or generic-optics with 'computeEnvironmentOrder' instead." #-}

instance Lude.FromJSON JobQueueDetail where
  parseJSON =
    Lude.withObject
      "JobQueueDetail"
      ( \x ->
          JobQueueDetail'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "statusReason")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "jobQueueName")
            Lude.<*> (x Lude..: "jobQueueArn")
            Lude.<*> (x Lude..: "state")
            Lude.<*> (x Lude..: "priority")
            Lude.<*> (x Lude..:? "computeEnvironmentOrder" Lude..!= Lude.mempty)
      )
