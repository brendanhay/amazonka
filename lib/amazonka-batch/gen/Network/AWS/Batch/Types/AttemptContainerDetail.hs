{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.AttemptContainerDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.AttemptContainerDetail
  ( AttemptContainerDetail (..),

    -- * Smart constructor
    mkAttemptContainerDetail,

    -- * Lenses
    acdNetworkInterfaces,
    acdTaskARN,
    acdContainerInstanceARN,
    acdReason,
    acdLogStreamName,
    acdExitCode,
  )
where

import Network.AWS.Batch.Types.NetworkInterface
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the details of a container that is part of a job attempt.
--
-- /See:/ 'mkAttemptContainerDetail' smart constructor.
data AttemptContainerDetail = AttemptContainerDetail'
  { -- | The network interfaces associated with the job attempt.
    networkInterfaces :: Lude.Maybe [NetworkInterface],
    -- | The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the job attempt. Each container attempt receives a task ARN when they reach the @STARTING@ status.
    taskARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon ECS container instance that hosts the job attempt.
    containerInstanceARN :: Lude.Maybe Lude.Text,
    -- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
    reason :: Lude.Maybe Lude.Text,
    -- | The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
    logStreamName :: Lude.Maybe Lude.Text,
    -- | The exit code for the job attempt. A non-zero exit code is considered a failure.
    exitCode :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttemptContainerDetail' with the minimum fields required to make a request.
--
-- * 'networkInterfaces' - The network interfaces associated with the job attempt.
-- * 'taskARN' - The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the job attempt. Each container attempt receives a task ARN when they reach the @STARTING@ status.
-- * 'containerInstanceARN' - The Amazon Resource Name (ARN) of the Amazon ECS container instance that hosts the job attempt.
-- * 'reason' - A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
-- * 'logStreamName' - The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
-- * 'exitCode' - The exit code for the job attempt. A non-zero exit code is considered a failure.
mkAttemptContainerDetail ::
  AttemptContainerDetail
mkAttemptContainerDetail =
  AttemptContainerDetail'
    { networkInterfaces = Lude.Nothing,
      taskARN = Lude.Nothing,
      containerInstanceARN = Lude.Nothing,
      reason = Lude.Nothing,
      logStreamName = Lude.Nothing,
      exitCode = Lude.Nothing
    }

-- | The network interfaces associated with the job attempt.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdNetworkInterfaces :: Lens.Lens' AttemptContainerDetail (Lude.Maybe [NetworkInterface])
acdNetworkInterfaces = Lens.lens (networkInterfaces :: AttemptContainerDetail -> Lude.Maybe [NetworkInterface]) (\s a -> s {networkInterfaces = a} :: AttemptContainerDetail)
{-# DEPRECATED acdNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the job attempt. Each container attempt receives a task ARN when they reach the @STARTING@ status.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdTaskARN :: Lens.Lens' AttemptContainerDetail (Lude.Maybe Lude.Text)
acdTaskARN = Lens.lens (taskARN :: AttemptContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {taskARN = a} :: AttemptContainerDetail)
{-# DEPRECATED acdTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon ECS container instance that hosts the job attempt.
--
-- /Note:/ Consider using 'containerInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdContainerInstanceARN :: Lens.Lens' AttemptContainerDetail (Lude.Maybe Lude.Text)
acdContainerInstanceARN = Lens.lens (containerInstanceARN :: AttemptContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {containerInstanceARN = a} :: AttemptContainerDetail)
{-# DEPRECATED acdContainerInstanceARN "Use generic-lens or generic-optics with 'containerInstanceARN' instead." #-}

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdReason :: Lens.Lens' AttemptContainerDetail (Lude.Maybe Lude.Text)
acdReason = Lens.lens (reason :: AttemptContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: AttemptContainerDetail)
{-# DEPRECATED acdReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdLogStreamName :: Lens.Lens' AttemptContainerDetail (Lude.Maybe Lude.Text)
acdLogStreamName = Lens.lens (logStreamName :: AttemptContainerDetail -> Lude.Maybe Lude.Text) (\s a -> s {logStreamName = a} :: AttemptContainerDetail)
{-# DEPRECATED acdLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | The exit code for the job attempt. A non-zero exit code is considered a failure.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdExitCode :: Lens.Lens' AttemptContainerDetail (Lude.Maybe Lude.Int)
acdExitCode = Lens.lens (exitCode :: AttemptContainerDetail -> Lude.Maybe Lude.Int) (\s a -> s {exitCode = a} :: AttemptContainerDetail)
{-# DEPRECATED acdExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

instance Lude.FromJSON AttemptContainerDetail where
  parseJSON =
    Lude.withObject
      "AttemptContainerDetail"
      ( \x ->
          AttemptContainerDetail'
            Lude.<$> (x Lude..:? "networkInterfaces" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "taskArn")
            Lude.<*> (x Lude..:? "containerInstanceArn")
            Lude.<*> (x Lude..:? "reason")
            Lude.<*> (x Lude..:? "logStreamName")
            Lude.<*> (x Lude..:? "exitCode")
      )
