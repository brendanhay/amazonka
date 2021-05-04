{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECS.Types.HealthCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HealthCheck where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a container health check. Health check parameters
-- that are specified in a container definition override any Docker health
-- checks that exist in the container image (such as those specified in a
-- parent image or from the image\'s Dockerfile).
--
-- You can view the health status of both individual containers and a task
-- with the DescribeTasks API operation or when viewing the task details in
-- the console.
--
-- The following describes the possible @healthStatus@ values for a
-- container:
--
-- -   @HEALTHY@-The container health check has passed successfully.
--
-- -   @UNHEALTHY@-The container health check has failed.
--
-- -   @UNKNOWN@-The container health check is being evaluated or there is
--     no container health check defined.
--
-- The following describes the possible @healthStatus@ values for a task.
-- The container health check status of nonessential containers do not have
-- an effect on the health status of a task.
--
-- -   @HEALTHY@-All essential containers within the task have passed their
--     health checks.
--
-- -   @UNHEALTHY@-One or more essential containers have failed their
--     health check.
--
-- -   @UNKNOWN@-The essential containers within the task are still having
--     their health checks evaluated or there are no container health
--     checks defined.
--
-- If a task is run manually, and not as part of a service, the task will
-- continue its lifecycle regardless of its health status. For tasks that
-- are part of a service, if the task reports as unhealthy then the task
-- will be stopped and the service scheduler will replace it.
--
-- The following are notes about container health check support:
--
-- -   Container health checks require version 1.17.0 or greater of the
--     Amazon ECS container agent. For more information, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>.
--
-- -   Container health checks are supported for Fargate tasks if you are
--     using platform version 1.1.0 or greater. For more information, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>.
--
-- -   Container health checks are not supported for tasks that are part of
--     a service that is configured to use a Classic Load Balancer.
--
-- /See:/ 'newHealthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { -- | The number of times to retry a failed health check before the container
    -- is considered unhealthy. You may specify between 1 and 10 retries. The
    -- default value is 3.
    retries :: Prelude.Maybe Prelude.Int,
    -- | The time period in seconds to wait for a health check to succeed before
    -- it is considered a failure. You may specify between 2 and 60 seconds.
    -- The default value is 5.
    timeout :: Prelude.Maybe Prelude.Int,
    -- | The optional grace period within which to provide containers time to
    -- bootstrap before failed health checks count towards the maximum number
    -- of retries. You may specify between 0 and 300 seconds. The @startPeriod@
    -- is disabled by default.
    --
    -- If a health check succeeds within the @startPeriod@, then the container
    -- is considered healthy and any subsequent failures count toward the
    -- maximum number of retries.
    startPeriod :: Prelude.Maybe Prelude.Int,
    -- | The time period in seconds between each health check execution. You may
    -- specify between 5 and 300 seconds. The default value is 30 seconds.
    interval :: Prelude.Maybe Prelude.Int,
    -- | A string array representing the command that the container runs to
    -- determine if it is healthy. The string array must start with @CMD@ to
    -- execute the command arguments directly, or @CMD-SHELL@ to run the
    -- command with the container\'s default shell. For example:
    --
    -- @[ \"CMD-SHELL\", \"curl -f http:\/\/localhost\/ || exit 1\" ]@
    --
    -- An exit code of 0 indicates success, and non-zero exit code indicates
    -- failure. For more information, see @HealthCheck@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API>.
    command :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retries', 'healthCheck_retries' - The number of times to retry a failed health check before the container
-- is considered unhealthy. You may specify between 1 and 10 retries. The
-- default value is 3.
--
-- 'timeout', 'healthCheck_timeout' - The time period in seconds to wait for a health check to succeed before
-- it is considered a failure. You may specify between 2 and 60 seconds.
-- The default value is 5.
--
-- 'startPeriod', 'healthCheck_startPeriod' - The optional grace period within which to provide containers time to
-- bootstrap before failed health checks count towards the maximum number
-- of retries. You may specify between 0 and 300 seconds. The @startPeriod@
-- is disabled by default.
--
-- If a health check succeeds within the @startPeriod@, then the container
-- is considered healthy and any subsequent failures count toward the
-- maximum number of retries.
--
-- 'interval', 'healthCheck_interval' - The time period in seconds between each health check execution. You may
-- specify between 5 and 300 seconds. The default value is 30 seconds.
--
-- 'command', 'healthCheck_command' - A string array representing the command that the container runs to
-- determine if it is healthy. The string array must start with @CMD@ to
-- execute the command arguments directly, or @CMD-SHELL@ to run the
-- command with the container\'s default shell. For example:
--
-- @[ \"CMD-SHELL\", \"curl -f http:\/\/localhost\/ || exit 1\" ]@
--
-- An exit code of 0 indicates success, and non-zero exit code indicates
-- failure. For more information, see @HealthCheck@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API>.
newHealthCheck ::
  HealthCheck
newHealthCheck =
  HealthCheck'
    { retries = Prelude.Nothing,
      timeout = Prelude.Nothing,
      startPeriod = Prelude.Nothing,
      interval = Prelude.Nothing,
      command = Prelude.mempty
    }

-- | The number of times to retry a failed health check before the container
-- is considered unhealthy. You may specify between 1 and 10 retries. The
-- default value is 3.
healthCheck_retries :: Lens.Lens' HealthCheck (Prelude.Maybe Prelude.Int)
healthCheck_retries = Lens.lens (\HealthCheck' {retries} -> retries) (\s@HealthCheck' {} a -> s {retries = a} :: HealthCheck)

-- | The time period in seconds to wait for a health check to succeed before
-- it is considered a failure. You may specify between 2 and 60 seconds.
-- The default value is 5.
healthCheck_timeout :: Lens.Lens' HealthCheck (Prelude.Maybe Prelude.Int)
healthCheck_timeout = Lens.lens (\HealthCheck' {timeout} -> timeout) (\s@HealthCheck' {} a -> s {timeout = a} :: HealthCheck)

-- | The optional grace period within which to provide containers time to
-- bootstrap before failed health checks count towards the maximum number
-- of retries. You may specify between 0 and 300 seconds. The @startPeriod@
-- is disabled by default.
--
-- If a health check succeeds within the @startPeriod@, then the container
-- is considered healthy and any subsequent failures count toward the
-- maximum number of retries.
healthCheck_startPeriod :: Lens.Lens' HealthCheck (Prelude.Maybe Prelude.Int)
healthCheck_startPeriod = Lens.lens (\HealthCheck' {startPeriod} -> startPeriod) (\s@HealthCheck' {} a -> s {startPeriod = a} :: HealthCheck)

-- | The time period in seconds between each health check execution. You may
-- specify between 5 and 300 seconds. The default value is 30 seconds.
healthCheck_interval :: Lens.Lens' HealthCheck (Prelude.Maybe Prelude.Int)
healthCheck_interval = Lens.lens (\HealthCheck' {interval} -> interval) (\s@HealthCheck' {} a -> s {interval = a} :: HealthCheck)

-- | A string array representing the command that the container runs to
-- determine if it is healthy. The string array must start with @CMD@ to
-- execute the command arguments directly, or @CMD-SHELL@ to run the
-- command with the container\'s default shell. For example:
--
-- @[ \"CMD-SHELL\", \"curl -f http:\/\/localhost\/ || exit 1\" ]@
--
-- An exit code of 0 indicates success, and non-zero exit code indicates
-- failure. For more information, see @HealthCheck@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API>.
healthCheck_command :: Lens.Lens' HealthCheck [Prelude.Text]
healthCheck_command = Lens.lens (\HealthCheck' {command} -> command) (\s@HealthCheck' {} a -> s {command = a} :: HealthCheck) Prelude.. Prelude._Coerce

instance Prelude.FromJSON HealthCheck where
  parseJSON =
    Prelude.withObject
      "HealthCheck"
      ( \x ->
          HealthCheck'
            Prelude.<$> (x Prelude..:? "retries")
            Prelude.<*> (x Prelude..:? "timeout")
            Prelude.<*> (x Prelude..:? "startPeriod")
            Prelude.<*> (x Prelude..:? "interval")
            Prelude.<*> (x Prelude..:? "command" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable HealthCheck

instance Prelude.NFData HealthCheck

instance Prelude.ToJSON HealthCheck where
  toJSON HealthCheck' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("retries" Prelude..=) Prelude.<$> retries,
            ("timeout" Prelude..=) Prelude.<$> timeout,
            ("startPeriod" Prelude..=) Prelude.<$> startPeriod,
            ("interval" Prelude..=) Prelude.<$> interval,
            Prelude.Just ("command" Prelude..= command)
          ]
      )
