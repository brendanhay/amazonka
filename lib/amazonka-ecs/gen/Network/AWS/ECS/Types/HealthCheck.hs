-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.HealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HealthCheck
  ( HealthCheck (..),

    -- * Smart constructor
    mkHealthCheck,

    -- * Lenses
    hcStartPeriod,
    hcRetries,
    hcInterval,
    hcTimeout,
    hcCommand,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a container health check. Health check parameters that are specified in a container definition override any Docker health checks that exist in the container image (such as those specified in a parent image or from the image's Dockerfile).
--
-- You can view the health status of both individual containers and a task with the DescribeTasks API operation or when viewing the task details in the console.
-- The following describes the possible @healthStatus@ values for a container:
--
--     * @HEALTHY@ -The container health check has passed successfully.
--
--
--     * @UNHEALTHY@ -The container health check has failed.
--
--
--     * @UNKNOWN@ -The container health check is being evaluated or there is no container health check defined.
--
--
-- The following describes the possible @healthStatus@ values for a task. The container health check status of nonessential containers do not have an effect on the health status of a task.
--
--     * @HEALTHY@ -All essential containers within the task have passed their health checks.
--
--
--     * @UNHEALTHY@ -One or more essential containers have failed their health check.
--
--
--     * @UNKNOWN@ -The essential containers within the task are still having their health checks evaluated or there are no container health checks defined.
--
--
-- If a task is run manually, and not as part of a service, the task will continue its lifecycle regardless of its health status. For tasks that are part of a service, if the task reports as unhealthy then the task will be stopped and the service scheduler will replace it.
-- The following are notes about container health check support:
--
--     * Container health checks require version 1.17.0 or greater of the Amazon ECS container agent. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> .
--
--
--     * Container health checks are supported for Fargate tasks if you are using platform version 1.1.0 or greater. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> .
--
--
--     * Container health checks are not supported for tasks that are part of a service that is configured to use a Classic Load Balancer.
--
--
--
-- /See:/ 'mkHealthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { startPeriod :: Lude.Maybe Lude.Int,
    retries :: Lude.Maybe Lude.Int,
    interval :: Lude.Maybe Lude.Int,
    timeout :: Lude.Maybe Lude.Int,
    command :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HealthCheck' with the minimum fields required to make a request.
--
-- * 'command' - A string array representing the command that the container runs to determine if it is healthy. The string array must start with @CMD@ to execute the command arguments directly, or @CMD-SHELL@ to run the command with the container's default shell. For example:
--
-- @[ "CMD-SHELL", "curl -f http://localhost/ || exit 1" ]@
-- An exit code of 0 indicates success, and non-zero exit code indicates failure. For more information, see @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
-- * 'interval' - The time period in seconds between each health check execution. You may specify between 5 and 300 seconds. The default value is 30 seconds.
-- * 'retries' - The number of times to retry a failed health check before the container is considered unhealthy. You may specify between 1 and 10 retries. The default value is 3.
-- * 'startPeriod' - The optional grace period within which to provide containers time to bootstrap before failed health checks count towards the maximum number of retries. You may specify between 0 and 300 seconds. The @startPeriod@ is disabled by default.
-- * 'timeout' - The time period in seconds to wait for a health check to succeed before it is considered a failure. You may specify between 2 and 60 seconds. The default value is 5.
mkHealthCheck ::
  HealthCheck
mkHealthCheck =
  HealthCheck'
    { startPeriod = Lude.Nothing,
      retries = Lude.Nothing,
      interval = Lude.Nothing,
      timeout = Lude.Nothing,
      command = Lude.mempty
    }

-- | The optional grace period within which to provide containers time to bootstrap before failed health checks count towards the maximum number of retries. You may specify between 0 and 300 seconds. The @startPeriod@ is disabled by default.
--
-- /Note:/ Consider using 'startPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcStartPeriod :: Lens.Lens' HealthCheck (Lude.Maybe Lude.Int)
hcStartPeriod = Lens.lens (startPeriod :: HealthCheck -> Lude.Maybe Lude.Int) (\s a -> s {startPeriod = a} :: HealthCheck)
{-# DEPRECATED hcStartPeriod "Use generic-lens or generic-optics with 'startPeriod' instead." #-}

-- | The number of times to retry a failed health check before the container is considered unhealthy. You may specify between 1 and 10 retries. The default value is 3.
--
-- /Note:/ Consider using 'retries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcRetries :: Lens.Lens' HealthCheck (Lude.Maybe Lude.Int)
hcRetries = Lens.lens (retries :: HealthCheck -> Lude.Maybe Lude.Int) (\s a -> s {retries = a} :: HealthCheck)
{-# DEPRECATED hcRetries "Use generic-lens or generic-optics with 'retries' instead." #-}

-- | The time period in seconds between each health check execution. You may specify between 5 and 300 seconds. The default value is 30 seconds.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcInterval :: Lens.Lens' HealthCheck (Lude.Maybe Lude.Int)
hcInterval = Lens.lens (interval :: HealthCheck -> Lude.Maybe Lude.Int) (\s a -> s {interval = a} :: HealthCheck)
{-# DEPRECATED hcInterval "Use generic-lens or generic-optics with 'interval' instead." #-}

-- | The time period in seconds to wait for a health check to succeed before it is considered a failure. You may specify between 2 and 60 seconds. The default value is 5.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcTimeout :: Lens.Lens' HealthCheck (Lude.Maybe Lude.Int)
hcTimeout = Lens.lens (timeout :: HealthCheck -> Lude.Maybe Lude.Int) (\s a -> s {timeout = a} :: HealthCheck)
{-# DEPRECATED hcTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | A string array representing the command that the container runs to determine if it is healthy. The string array must start with @CMD@ to execute the command arguments directly, or @CMD-SHELL@ to run the command with the container's default shell. For example:
--
-- @[ "CMD-SHELL", "curl -f http://localhost/ || exit 1" ]@
-- An exit code of 0 indicates success, and non-zero exit code indicates failure. For more information, see @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcCommand :: Lens.Lens' HealthCheck [Lude.Text]
hcCommand = Lens.lens (command :: HealthCheck -> [Lude.Text]) (\s a -> s {command = a} :: HealthCheck)
{-# DEPRECATED hcCommand "Use generic-lens or generic-optics with 'command' instead." #-}

instance Lude.FromJSON HealthCheck where
  parseJSON =
    Lude.withObject
      "HealthCheck"
      ( \x ->
          HealthCheck'
            Lude.<$> (x Lude..:? "startPeriod")
            Lude.<*> (x Lude..:? "retries")
            Lude.<*> (x Lude..:? "interval")
            Lude.<*> (x Lude..:? "timeout")
            Lude.<*> (x Lude..:? "command" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON HealthCheck where
  toJSON HealthCheck' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("startPeriod" Lude..=) Lude.<$> startPeriod,
            ("retries" Lude..=) Lude.<$> retries,
            ("interval" Lude..=) Lude.<$> interval,
            ("timeout" Lude..=) Lude.<$> timeout,
            Lude.Just ("command" Lude..= command)
          ]
      )
