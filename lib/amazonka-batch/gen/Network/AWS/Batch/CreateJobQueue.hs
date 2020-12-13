{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.CreateJobQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Batch job queue. When you create a job queue, you associate one or more compute environments to the queue and assign an order of preference for the compute environments.
--
-- You also set a priority to the job queue that determines the order in which the AWS Batch scheduler places jobs onto its associated compute environments. For example, if a compute environment is associated with more than one job queue, the job queue with a higher priority is given preference for scheduling jobs to that compute environment.
module Network.AWS.Batch.CreateJobQueue
  ( -- * Creating a request
    CreateJobQueue (..),
    mkCreateJobQueue,

    -- ** Request lenses
    cjqState,
    cjqPriority,
    cjqComputeEnvironmentOrder,
    cjqJobQueueName,
    cjqTags,

    -- * Destructuring the response
    CreateJobQueueResponse (..),
    mkCreateJobQueueResponse,

    -- ** Response lenses
    cjqrsJobQueueARN,
    cjqrsJobQueueName,
    cjqrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateJobQueue' smart constructor.
data CreateJobQueue = CreateJobQueue'
  { -- | The state of the job queue. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
    state :: Lude.Maybe JQState,
    -- | The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
    priority :: Lude.Int,
    -- | The set of compute environments mapped to a job queue and their order relative to each other. The job scheduler uses this parameter to determine which compute environment should execute a given job. Compute environments must be in the @VALID@ state before you can associate them with a job queue. You can associate up to three compute environments with a job queue.
    computeEnvironmentOrder :: [ComputeEnvironmentOrder],
    -- | The name of the job queue.
    jobQueueName :: Lude.Text,
    -- | The tags that you apply to the job queue to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJobQueue' with the minimum fields required to make a request.
--
-- * 'state' - The state of the job queue. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
-- * 'priority' - The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
-- * 'computeEnvironmentOrder' - The set of compute environments mapped to a job queue and their order relative to each other. The job scheduler uses this parameter to determine which compute environment should execute a given job. Compute environments must be in the @VALID@ state before you can associate them with a job queue. You can associate up to three compute environments with a job queue.
-- * 'jobQueueName' - The name of the job queue.
-- * 'tags' - The tags that you apply to the job queue to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
mkCreateJobQueue ::
  -- | 'priority'
  Lude.Int ->
  -- | 'jobQueueName'
  Lude.Text ->
  CreateJobQueue
mkCreateJobQueue pPriority_ pJobQueueName_ =
  CreateJobQueue'
    { state = Lude.Nothing,
      priority = pPriority_,
      computeEnvironmentOrder = Lude.mempty,
      jobQueueName = pJobQueueName_,
      tags = Lude.Nothing
    }

-- | The state of the job queue. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqState :: Lens.Lens' CreateJobQueue (Lude.Maybe JQState)
cjqState = Lens.lens (state :: CreateJobQueue -> Lude.Maybe JQState) (\s a -> s {state = a} :: CreateJobQueue)
{-# DEPRECATED cjqState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqPriority :: Lens.Lens' CreateJobQueue Lude.Int
cjqPriority = Lens.lens (priority :: CreateJobQueue -> Lude.Int) (\s a -> s {priority = a} :: CreateJobQueue)
{-# DEPRECATED cjqPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The set of compute environments mapped to a job queue and their order relative to each other. The job scheduler uses this parameter to determine which compute environment should execute a given job. Compute environments must be in the @VALID@ state before you can associate them with a job queue. You can associate up to three compute environments with a job queue.
--
-- /Note:/ Consider using 'computeEnvironmentOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqComputeEnvironmentOrder :: Lens.Lens' CreateJobQueue [ComputeEnvironmentOrder]
cjqComputeEnvironmentOrder = Lens.lens (computeEnvironmentOrder :: CreateJobQueue -> [ComputeEnvironmentOrder]) (\s a -> s {computeEnvironmentOrder = a} :: CreateJobQueue)
{-# DEPRECATED cjqComputeEnvironmentOrder "Use generic-lens or generic-optics with 'computeEnvironmentOrder' instead." #-}

-- | The name of the job queue.
--
-- /Note:/ Consider using 'jobQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqJobQueueName :: Lens.Lens' CreateJobQueue Lude.Text
cjqJobQueueName = Lens.lens (jobQueueName :: CreateJobQueue -> Lude.Text) (\s a -> s {jobQueueName = a} :: CreateJobQueue)
{-# DEPRECATED cjqJobQueueName "Use generic-lens or generic-optics with 'jobQueueName' instead." #-}

-- | The tags that you apply to the job queue to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqTags :: Lens.Lens' CreateJobQueue (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cjqTags = Lens.lens (tags :: CreateJobQueue -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateJobQueue)
{-# DEPRECATED cjqTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateJobQueue where
  type Rs CreateJobQueue = CreateJobQueueResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateJobQueueResponse'
            Lude.<$> (x Lude..:> "jobQueueArn")
            Lude.<*> (x Lude..:> "jobQueueName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateJobQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateJobQueue where
  toJSON CreateJobQueue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("state" Lude..=) Lude.<$> state,
            Lude.Just ("priority" Lude..= priority),
            Lude.Just
              ("computeEnvironmentOrder" Lude..= computeEnvironmentOrder),
            Lude.Just ("jobQueueName" Lude..= jobQueueName),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateJobQueue where
  toPath = Lude.const "/v1/createjobqueue"

instance Lude.ToQuery CreateJobQueue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateJobQueueResponse' smart constructor.
data CreateJobQueueResponse = CreateJobQueueResponse'
  { -- | The Amazon Resource Name (ARN) of the job queue.
    jobQueueARN :: Lude.Text,
    -- | The name of the job queue.
    jobQueueName :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJobQueueResponse' with the minimum fields required to make a request.
--
-- * 'jobQueueARN' - The Amazon Resource Name (ARN) of the job queue.
-- * 'jobQueueName' - The name of the job queue.
-- * 'responseStatus' - The response status code.
mkCreateJobQueueResponse ::
  -- | 'jobQueueARN'
  Lude.Text ->
  -- | 'jobQueueName'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateJobQueueResponse
mkCreateJobQueueResponse
  pJobQueueARN_
  pJobQueueName_
  pResponseStatus_ =
    CreateJobQueueResponse'
      { jobQueueARN = pJobQueueARN_,
        jobQueueName = pJobQueueName_,
        responseStatus = pResponseStatus_
      }

-- | The Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueueARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqrsJobQueueARN :: Lens.Lens' CreateJobQueueResponse Lude.Text
cjqrsJobQueueARN = Lens.lens (jobQueueARN :: CreateJobQueueResponse -> Lude.Text) (\s a -> s {jobQueueARN = a} :: CreateJobQueueResponse)
{-# DEPRECATED cjqrsJobQueueARN "Use generic-lens or generic-optics with 'jobQueueARN' instead." #-}

-- | The name of the job queue.
--
-- /Note:/ Consider using 'jobQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqrsJobQueueName :: Lens.Lens' CreateJobQueueResponse Lude.Text
cjqrsJobQueueName = Lens.lens (jobQueueName :: CreateJobQueueResponse -> Lude.Text) (\s a -> s {jobQueueName = a} :: CreateJobQueueResponse)
{-# DEPRECATED cjqrsJobQueueName "Use generic-lens or generic-optics with 'jobQueueName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqrsResponseStatus :: Lens.Lens' CreateJobQueueResponse Lude.Int
cjqrsResponseStatus = Lens.lens (responseStatus :: CreateJobQueueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateJobQueueResponse)
{-# DEPRECATED cjqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
