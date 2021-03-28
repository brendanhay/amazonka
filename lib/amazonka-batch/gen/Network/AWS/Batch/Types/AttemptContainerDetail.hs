{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.AttemptContainerDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.AttemptContainerDetail
  ( AttemptContainerDetail (..)
  -- * Smart constructor
  , mkAttemptContainerDetail
  -- * Lenses
  , acdContainerInstanceArn
  , acdExitCode
  , acdLogStreamName
  , acdNetworkInterfaces
  , acdReason
  , acdTaskArn
  ) where

import qualified Network.AWS.Batch.Types.NetworkInterface as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the details of a container that is part of a job attempt.
--
-- /See:/ 'mkAttemptContainerDetail' smart constructor.
data AttemptContainerDetail = AttemptContainerDetail'
  { containerInstanceArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Amazon ECS container instance that hosts the job attempt.
  , exitCode :: Core.Maybe Core.Int
    -- ^ The exit code for the job attempt. A non-zero exit code is considered a failure.
  , logStreamName :: Core.Maybe Core.Text
    -- ^ The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
  , networkInterfaces :: Core.Maybe [Types.NetworkInterface]
    -- ^ The network interfaces associated with the job attempt.
  , reason :: Core.Maybe Core.Text
    -- ^ A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
  , taskArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the job attempt. Each container attempt receives a task ARN when they reach the @STARTING@ status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttemptContainerDetail' value with any optional fields omitted.
mkAttemptContainerDetail
    :: AttemptContainerDetail
mkAttemptContainerDetail
  = AttemptContainerDetail'{containerInstanceArn = Core.Nothing,
                            exitCode = Core.Nothing, logStreamName = Core.Nothing,
                            networkInterfaces = Core.Nothing, reason = Core.Nothing,
                            taskArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the Amazon ECS container instance that hosts the job attempt.
--
-- /Note:/ Consider using 'containerInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdContainerInstanceArn :: Lens.Lens' AttemptContainerDetail (Core.Maybe Core.Text)
acdContainerInstanceArn = Lens.field @"containerInstanceArn"
{-# INLINEABLE acdContainerInstanceArn #-}
{-# DEPRECATED containerInstanceArn "Use generic-lens or generic-optics with 'containerInstanceArn' instead"  #-}

-- | The exit code for the job attempt. A non-zero exit code is considered a failure.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdExitCode :: Lens.Lens' AttemptContainerDetail (Core.Maybe Core.Int)
acdExitCode = Lens.field @"exitCode"
{-# INLINEABLE acdExitCode #-}
{-# DEPRECATED exitCode "Use generic-lens or generic-optics with 'exitCode' instead"  #-}

-- | The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdLogStreamName :: Lens.Lens' AttemptContainerDetail (Core.Maybe Core.Text)
acdLogStreamName = Lens.field @"logStreamName"
{-# INLINEABLE acdLogStreamName #-}
{-# DEPRECATED logStreamName "Use generic-lens or generic-optics with 'logStreamName' instead"  #-}

-- | The network interfaces associated with the job attempt.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdNetworkInterfaces :: Lens.Lens' AttemptContainerDetail (Core.Maybe [Types.NetworkInterface])
acdNetworkInterfaces = Lens.field @"networkInterfaces"
{-# INLINEABLE acdNetworkInterfaces #-}
{-# DEPRECATED networkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead"  #-}

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdReason :: Lens.Lens' AttemptContainerDetail (Core.Maybe Core.Text)
acdReason = Lens.field @"reason"
{-# INLINEABLE acdReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the job attempt. Each container attempt receives a task ARN when they reach the @STARTING@ status.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acdTaskArn :: Lens.Lens' AttemptContainerDetail (Core.Maybe Core.Text)
acdTaskArn = Lens.field @"taskArn"
{-# INLINEABLE acdTaskArn #-}
{-# DEPRECATED taskArn "Use generic-lens or generic-optics with 'taskArn' instead"  #-}

instance Core.FromJSON AttemptContainerDetail where
        parseJSON
          = Core.withObject "AttemptContainerDetail" Core.$
              \ x ->
                AttemptContainerDetail' Core.<$>
                  (x Core..:? "containerInstanceArn") Core.<*> x Core..:? "exitCode"
                    Core.<*> x Core..:? "logStreamName"
                    Core.<*> x Core..:? "networkInterfaces"
                    Core.<*> x Core..:? "reason"
                    Core.<*> x Core..:? "taskArn"
