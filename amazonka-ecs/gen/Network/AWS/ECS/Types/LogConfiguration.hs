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
-- Module      : Network.AWS.ECS.Types.LogConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.LogConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.LogDriver
import Network.AWS.ECS.Types.Secret
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The log configuration for the container. This parameter maps to
-- @LogConfig@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--log-driver@ option to
-- <https://docs.docker.com/engine/reference/commandline/run/ docker run> .
--
-- By default, containers use the same logging driver that the Docker
-- daemon uses; however the container may use a different logging driver
-- than the Docker daemon by specifying a log driver configuration in the
-- container definition. For more information on the options for different
-- supported log drivers, see
-- <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers>
-- in the Docker documentation.
--
-- The following should be noted when specifying a log configuration for
-- your containers:
--
-- -   Amazon ECS currently supports a subset of the logging drivers
--     available to the Docker daemon (shown in the valid values below).
--     Additional log drivers may be available in future releases of the
--     Amazon ECS container agent.
--
-- -   This parameter requires version 1.18 of the Docker Remote API or
--     greater on your container instance.
--
-- -   For tasks hosted on Amazon EC2 instances, the Amazon ECS container
--     agent must register the available logging drivers with the
--     @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before
--     containers placed on that instance can use these log configuration
--     options. For more information, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS container agent configuration>
--     in the /Amazon Elastic Container Service Developer Guide/.
--
-- -   For tasks on AWS Fargate, because you do not have access to the
--     underlying infrastructure your tasks are hosted on, any additional
--     software needed will have to be installed outside of the task. For
--     example, the Fluentd output aggregators or a remote host running
--     Logstash to send Gelf logs to.
--
-- /See:/ 'newLogConfiguration' smart constructor.
data LogConfiguration = LogConfiguration'
  { -- | The configuration options to send to the log driver. This parameter
    -- requires version 1.19 of the Docker Remote API or greater on your
    -- container instance. To check the Docker Remote API version on your
    -- container instance, log in to your container instance and run the
    -- following command:
    -- @sudo docker version --format \'{{.Server.APIVersion}}\'@
    options :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The secrets to pass to the log configuration. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    secretOptions :: Prelude.Maybe [Secret],
    -- | The log driver to use for the container.
    --
    -- For tasks on AWS Fargate, the supported log drivers are @awslogs@,
    -- @splunk@, and @awsfirelens@.
    --
    -- For tasks hosted on Amazon EC2 instances, the supported log drivers are
    -- @awslogs@, @fluentd@, @gelf@, @json-file@, @journald@,
    -- @logentries@,@syslog@, @splunk@, and @awsfirelens@.
    --
    -- For more information about using the @awslogs@ log driver, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_awslogs.html Using the awslogs log driver>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- For more information about using the @awsfirelens@ log driver, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom log routing>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If you have a custom driver that is not listed, you can fork the Amazon
    -- ECS container agent project that is
    -- <https://github.com/aws/amazon-ecs-agent available on GitHub> and
    -- customize it to work with that driver. We encourage you to submit pull
    -- requests for changes that you would like to have included. However, we
    -- do not currently provide support for running modified copies of this
    -- software.
    logDriver :: LogDriver
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'logConfiguration_options' - The configuration options to send to the log driver. This parameter
-- requires version 1.19 of the Docker Remote API or greater on your
-- container instance. To check the Docker Remote API version on your
-- container instance, log in to your container instance and run the
-- following command:
-- @sudo docker version --format \'{{.Server.APIVersion}}\'@
--
-- 'secretOptions', 'logConfiguration_secretOptions' - The secrets to pass to the log configuration. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'logDriver', 'logConfiguration_logDriver' - The log driver to use for the container.
--
-- For tasks on AWS Fargate, the supported log drivers are @awslogs@,
-- @splunk@, and @awsfirelens@.
--
-- For tasks hosted on Amazon EC2 instances, the supported log drivers are
-- @awslogs@, @fluentd@, @gelf@, @json-file@, @journald@,
-- @logentries@,@syslog@, @splunk@, and @awsfirelens@.
--
-- For more information about using the @awslogs@ log driver, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_awslogs.html Using the awslogs log driver>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For more information about using the @awsfirelens@ log driver, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom log routing>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If you have a custom driver that is not listed, you can fork the Amazon
-- ECS container agent project that is
-- <https://github.com/aws/amazon-ecs-agent available on GitHub> and
-- customize it to work with that driver. We encourage you to submit pull
-- requests for changes that you would like to have included. However, we
-- do not currently provide support for running modified copies of this
-- software.
newLogConfiguration ::
  -- | 'logDriver'
  LogDriver ->
  LogConfiguration
newLogConfiguration pLogDriver_ =
  LogConfiguration'
    { options = Prelude.Nothing,
      secretOptions = Prelude.Nothing,
      logDriver = pLogDriver_
    }

-- | The configuration options to send to the log driver. This parameter
-- requires version 1.19 of the Docker Remote API or greater on your
-- container instance. To check the Docker Remote API version on your
-- container instance, log in to your container instance and run the
-- following command:
-- @sudo docker version --format \'{{.Server.APIVersion}}\'@
logConfiguration_options :: Lens.Lens' LogConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
logConfiguration_options = Lens.lens (\LogConfiguration' {options} -> options) (\s@LogConfiguration' {} a -> s {options = a} :: LogConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | The secrets to pass to the log configuration. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data>
-- in the /Amazon Elastic Container Service Developer Guide/.
logConfiguration_secretOptions :: Lens.Lens' LogConfiguration (Prelude.Maybe [Secret])
logConfiguration_secretOptions = Lens.lens (\LogConfiguration' {secretOptions} -> secretOptions) (\s@LogConfiguration' {} a -> s {secretOptions = a} :: LogConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | The log driver to use for the container.
--
-- For tasks on AWS Fargate, the supported log drivers are @awslogs@,
-- @splunk@, and @awsfirelens@.
--
-- For tasks hosted on Amazon EC2 instances, the supported log drivers are
-- @awslogs@, @fluentd@, @gelf@, @json-file@, @journald@,
-- @logentries@,@syslog@, @splunk@, and @awsfirelens@.
--
-- For more information about using the @awslogs@ log driver, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_awslogs.html Using the awslogs log driver>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For more information about using the @awsfirelens@ log driver, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom log routing>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If you have a custom driver that is not listed, you can fork the Amazon
-- ECS container agent project that is
-- <https://github.com/aws/amazon-ecs-agent available on GitHub> and
-- customize it to work with that driver. We encourage you to submit pull
-- requests for changes that you would like to have included. However, we
-- do not currently provide support for running modified copies of this
-- software.
logConfiguration_logDriver :: Lens.Lens' LogConfiguration LogDriver
logConfiguration_logDriver = Lens.lens (\LogConfiguration' {logDriver} -> logDriver) (\s@LogConfiguration' {} a -> s {logDriver = a} :: LogConfiguration)

instance Core.FromJSON LogConfiguration where
  parseJSON =
    Core.withObject
      "LogConfiguration"
      ( \x ->
          LogConfiguration'
            Prelude.<$> (x Core..:? "options" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "secretOptions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "logDriver")
      )

instance Prelude.Hashable LogConfiguration

instance Prelude.NFData LogConfiguration

instance Core.ToJSON LogConfiguration where
  toJSON LogConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("options" Core..=) Prelude.<$> options,
            ("secretOptions" Core..=) Prelude.<$> secretOptions,
            Prelude.Just ("logDriver" Core..= logDriver)
          ]
      )
