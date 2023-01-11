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
-- Module      : Amazonka.ECS.Types.HealthCheck
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.HealthCheck where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing a container health check. Health check parameters
-- that are specified in a container definition override any Docker health
-- checks that exist in the container image (such as those specified in a
-- parent image or from the image\'s Dockerfile).
--
-- The Amazon ECS container agent only monitors and reports on the health
-- checks specified in the task definition. Amazon ECS does not monitor
-- Docker health checks that are embedded in a container image and not
-- specified in the container definition. Health check parameters that are
-- specified in a container definition override any Docker health checks
-- that exist in the container image.
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
-- -   @UNKNOWN@-The container health check is being evaluated or there\'s
--     no container health check defined.
--
-- The following describes the possible @healthStatus@ values for a task.
-- The container health check status of nonessential containers only
-- affects the health status of a task if no essential containers have
-- health checks defined.
--
-- -   @HEALTHY@-All essential containers within the task have passed their
--     health checks.
--
-- -   @UNHEALTHY@-One or more essential containers have failed their
--     health check.
--
-- -   @UNKNOWN@-The essential containers within the task are still having
--     their health checks evaluated or there are only nonessential
--     containers with health checks defined.
--
-- If a task is run manually, and not as part of a service, the task will
-- continue its lifecycle regardless of its health status. For tasks that
-- are part of a service, if the task reports as unhealthy then the task
-- will be stopped and the service scheduler will replace it.
--
-- For tasks that are a part of a service and the service uses the @ECS@
-- rolling deployment type, the deployment is paused while the new tasks
-- have the @UNKNOWN@ task health check status. For example, tasks that
-- define health checks for nonessential containers when no essential
-- containers have health checks will have the @UNKNOWN@ health check
-- status indefinitely which prevents the deployment from completing.
--
-- The following are notes about container health check support:
--
-- -   Container health checks require version 1.17.0 or greater of the
--     Amazon ECS container agent. For more information, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS container agent>.
--
-- -   Container health checks are supported for Fargate tasks if you\'re
--     using platform version @1.1.0@ or greater. For more information, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>.
--
-- -   Container health checks aren\'t supported for tasks that are part of
--     a service that\'s configured to use a Classic Load Balancer.
--
-- /See:/ 'newHealthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { -- | The time period in seconds between each health check execution. You may
    -- specify between 5 and 300 seconds. The default value is 30 seconds.
    interval :: Prelude.Maybe Prelude.Int,
    -- | The number of times to retry a failed health check before the container
    -- is considered unhealthy. You may specify between 1 and 10 retries. The
    -- default value is 3.
    retries :: Prelude.Maybe Prelude.Int,
    -- | The optional grace period to provide containers time to bootstrap before
    -- failed health checks count towards the maximum number of retries. You
    -- can specify between 0 and 300 seconds. By default, the @startPeriod@ is
    -- disabled.
    --
    -- If a health check succeeds within the @startPeriod@, then the container
    -- is considered healthy and any subsequent failures count toward the
    -- maximum number of retries.
    startPeriod :: Prelude.Maybe Prelude.Int,
    -- | The time period in seconds to wait for a health check to succeed before
    -- it is considered a failure. You may specify between 2 and 60 seconds.
    -- The default value is 5.
    timeout :: Prelude.Maybe Prelude.Int,
    -- | A string array representing the command that the container runs to
    -- determine if it is healthy. The string array must start with @CMD@ to
    -- run the command arguments directly, or @CMD-SHELL@ to run the command
    -- with the container\'s default shell.
    --
    -- When you use the Amazon Web Services Management Console JSON panel, the
    -- Command Line Interface, or the APIs, enclose the list of commands in
    -- brackets.
    --
    -- @[ \"CMD-SHELL\", \"curl -f http:\/\/localhost\/ || exit 1\" ]@
    --
    -- You don\'t need to include the brackets when you use the Amazon Web
    -- Services Management Console.
    --
    -- @ \"CMD-SHELL\", \"curl -f http:\/\/localhost\/ || exit 1\" @
    --
    -- An exit code of 0 indicates success, and non-zero exit code indicates
    -- failure. For more information, see @HealthCheck@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API>.
    command :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interval', 'healthCheck_interval' - The time period in seconds between each health check execution. You may
-- specify between 5 and 300 seconds. The default value is 30 seconds.
--
-- 'retries', 'healthCheck_retries' - The number of times to retry a failed health check before the container
-- is considered unhealthy. You may specify between 1 and 10 retries. The
-- default value is 3.
--
-- 'startPeriod', 'healthCheck_startPeriod' - The optional grace period to provide containers time to bootstrap before
-- failed health checks count towards the maximum number of retries. You
-- can specify between 0 and 300 seconds. By default, the @startPeriod@ is
-- disabled.
--
-- If a health check succeeds within the @startPeriod@, then the container
-- is considered healthy and any subsequent failures count toward the
-- maximum number of retries.
--
-- 'timeout', 'healthCheck_timeout' - The time period in seconds to wait for a health check to succeed before
-- it is considered a failure. You may specify between 2 and 60 seconds.
-- The default value is 5.
--
-- 'command', 'healthCheck_command' - A string array representing the command that the container runs to
-- determine if it is healthy. The string array must start with @CMD@ to
-- run the command arguments directly, or @CMD-SHELL@ to run the command
-- with the container\'s default shell.
--
-- When you use the Amazon Web Services Management Console JSON panel, the
-- Command Line Interface, or the APIs, enclose the list of commands in
-- brackets.
--
-- @[ \"CMD-SHELL\", \"curl -f http:\/\/localhost\/ || exit 1\" ]@
--
-- You don\'t need to include the brackets when you use the Amazon Web
-- Services Management Console.
--
-- @ \"CMD-SHELL\", \"curl -f http:\/\/localhost\/ || exit 1\" @
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
    { interval = Prelude.Nothing,
      retries = Prelude.Nothing,
      startPeriod = Prelude.Nothing,
      timeout = Prelude.Nothing,
      command = Prelude.mempty
    }

-- | The time period in seconds between each health check execution. You may
-- specify between 5 and 300 seconds. The default value is 30 seconds.
healthCheck_interval :: Lens.Lens' HealthCheck (Prelude.Maybe Prelude.Int)
healthCheck_interval = Lens.lens (\HealthCheck' {interval} -> interval) (\s@HealthCheck' {} a -> s {interval = a} :: HealthCheck)

-- | The number of times to retry a failed health check before the container
-- is considered unhealthy. You may specify between 1 and 10 retries. The
-- default value is 3.
healthCheck_retries :: Lens.Lens' HealthCheck (Prelude.Maybe Prelude.Int)
healthCheck_retries = Lens.lens (\HealthCheck' {retries} -> retries) (\s@HealthCheck' {} a -> s {retries = a} :: HealthCheck)

-- | The optional grace period to provide containers time to bootstrap before
-- failed health checks count towards the maximum number of retries. You
-- can specify between 0 and 300 seconds. By default, the @startPeriod@ is
-- disabled.
--
-- If a health check succeeds within the @startPeriod@, then the container
-- is considered healthy and any subsequent failures count toward the
-- maximum number of retries.
healthCheck_startPeriod :: Lens.Lens' HealthCheck (Prelude.Maybe Prelude.Int)
healthCheck_startPeriod = Lens.lens (\HealthCheck' {startPeriod} -> startPeriod) (\s@HealthCheck' {} a -> s {startPeriod = a} :: HealthCheck)

-- | The time period in seconds to wait for a health check to succeed before
-- it is considered a failure. You may specify between 2 and 60 seconds.
-- The default value is 5.
healthCheck_timeout :: Lens.Lens' HealthCheck (Prelude.Maybe Prelude.Int)
healthCheck_timeout = Lens.lens (\HealthCheck' {timeout} -> timeout) (\s@HealthCheck' {} a -> s {timeout = a} :: HealthCheck)

-- | A string array representing the command that the container runs to
-- determine if it is healthy. The string array must start with @CMD@ to
-- run the command arguments directly, or @CMD-SHELL@ to run the command
-- with the container\'s default shell.
--
-- When you use the Amazon Web Services Management Console JSON panel, the
-- Command Line Interface, or the APIs, enclose the list of commands in
-- brackets.
--
-- @[ \"CMD-SHELL\", \"curl -f http:\/\/localhost\/ || exit 1\" ]@
--
-- You don\'t need to include the brackets when you use the Amazon Web
-- Services Management Console.
--
-- @ \"CMD-SHELL\", \"curl -f http:\/\/localhost\/ || exit 1\" @
--
-- An exit code of 0 indicates success, and non-zero exit code indicates
-- failure. For more information, see @HealthCheck@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API>.
healthCheck_command :: Lens.Lens' HealthCheck [Prelude.Text]
healthCheck_command = Lens.lens (\HealthCheck' {command} -> command) (\s@HealthCheck' {} a -> s {command = a} :: HealthCheck) Prelude.. Lens.coerced

instance Data.FromJSON HealthCheck where
  parseJSON =
    Data.withObject
      "HealthCheck"
      ( \x ->
          HealthCheck'
            Prelude.<$> (x Data..:? "interval")
            Prelude.<*> (x Data..:? "retries")
            Prelude.<*> (x Data..:? "startPeriod")
            Prelude.<*> (x Data..:? "timeout")
            Prelude.<*> (x Data..:? "command" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable HealthCheck where
  hashWithSalt _salt HealthCheck' {..} =
    _salt `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` retries
      `Prelude.hashWithSalt` startPeriod
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` command

instance Prelude.NFData HealthCheck where
  rnf HealthCheck' {..} =
    Prelude.rnf interval
      `Prelude.seq` Prelude.rnf retries
      `Prelude.seq` Prelude.rnf startPeriod
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf command

instance Data.ToJSON HealthCheck where
  toJSON HealthCheck' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("interval" Data..=) Prelude.<$> interval,
            ("retries" Data..=) Prelude.<$> retries,
            ("startPeriod" Data..=) Prelude.<$> startPeriod,
            ("timeout" Data..=) Prelude.<$> timeout,
            Prelude.Just ("command" Data..= command)
          ]
      )
