{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LogConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.LogConfiguration
  ( LogConfiguration (..),

    -- * Smart constructor
    mkLogConfiguration,

    -- * Lenses
    lcOptions,
    lcSecretOptions,
    lcLogDriver,
  )
where

import Network.AWS.ECS.Types.LogDriver
import Network.AWS.ECS.Types.Secret
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The log configuration for the container. This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/commandline/run/ @docker run@ > .
--
-- By default, containers use the same logging driver that the Docker daemon uses; however the container may use a different logging driver than the Docker daemon by specifying a log driver configuration in the container definition. For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
-- The following should be noted when specifying a log configuration for your containers:
--
--     * Amazon ECS currently supports a subset of the logging drivers available to the Docker daemon (shown in the valid values below). Additional log drivers may be available in future releases of the Amazon ECS container agent.
--
--
--     * This parameter requires version 1.18 of the Docker Remote API or greater on your container instance.
--
--
--     * For tasks hosted on Amazon EC2 instances, the Amazon ECS container agent must register the available logging drivers with the @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers placed on that instance can use these log configuration options. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS container agent configuration> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--     * For tasks on AWS Fargate, because you do not have access to the underlying infrastructure your tasks are hosted on, any additional software needed will have to be installed outside of the task. For example, the Fluentd output aggregators or a remote host running Logstash to send Gelf logs to.
--
--
--
-- /See:/ 'mkLogConfiguration' smart constructor.
data LogConfiguration = LogConfiguration'
  { options ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    secretOptions :: Lude.Maybe [Secret],
    logDriver :: LogDriver
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogConfiguration' with the minimum fields required to make a request.
--
-- * 'logDriver' - The log driver to use for the container.
--
-- For tasks on AWS Fargate, the supported log drivers are @awslogs@ , @splunk@ , and @awsfirelens@ .
-- For tasks hosted on Amazon EC2 instances, the supported log drivers are @awslogs@ , @fluentd@ , @gelf@ , @json-file@ , @journald@ , @logentries@ ,@syslog@ , @splunk@ , and @awsfirelens@ .
-- For more information about using the @awslogs@ log driver, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_awslogs.html Using the awslogs log driver> in the /Amazon Elastic Container Service Developer Guide/ .
-- For more information about using the @awsfirelens@ log driver, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom log routing> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'options' - The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
-- * 'secretOptions' - The secrets to pass to the log configuration. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
mkLogConfiguration ::
  -- | 'logDriver'
  LogDriver ->
  LogConfiguration
mkLogConfiguration pLogDriver_ =
  LogConfiguration'
    { options = Lude.Nothing,
      secretOptions = Lude.Nothing,
      logDriver = pLogDriver_
    }

-- | The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcOptions :: Lens.Lens' LogConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
lcOptions = Lens.lens (options :: LogConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {options = a} :: LogConfiguration)
{-# DEPRECATED lcOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The secrets to pass to the log configuration. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'secretOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcSecretOptions :: Lens.Lens' LogConfiguration (Lude.Maybe [Secret])
lcSecretOptions = Lens.lens (secretOptions :: LogConfiguration -> Lude.Maybe [Secret]) (\s a -> s {secretOptions = a} :: LogConfiguration)
{-# DEPRECATED lcSecretOptions "Use generic-lens or generic-optics with 'secretOptions' instead." #-}

-- | The log driver to use for the container.
--
-- For tasks on AWS Fargate, the supported log drivers are @awslogs@ , @splunk@ , and @awsfirelens@ .
-- For tasks hosted on Amazon EC2 instances, the supported log drivers are @awslogs@ , @fluentd@ , @gelf@ , @json-file@ , @journald@ , @logentries@ ,@syslog@ , @splunk@ , and @awsfirelens@ .
-- For more information about using the @awslogs@ log driver, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_awslogs.html Using the awslogs log driver> in the /Amazon Elastic Container Service Developer Guide/ .
-- For more information about using the @awsfirelens@ log driver, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom log routing> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'logDriver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLogDriver :: Lens.Lens' LogConfiguration LogDriver
lcLogDriver = Lens.lens (logDriver :: LogConfiguration -> LogDriver) (\s a -> s {logDriver = a} :: LogConfiguration)
{-# DEPRECATED lcLogDriver "Use generic-lens or generic-optics with 'logDriver' instead." #-}

instance Lude.FromJSON LogConfiguration where
  parseJSON =
    Lude.withObject
      "LogConfiguration"
      ( \x ->
          LogConfiguration'
            Lude.<$> (x Lude..:? "options" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "secretOptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "logDriver")
      )

instance Lude.ToJSON LogConfiguration where
  toJSON LogConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("options" Lude..=) Lude.<$> options,
            ("secretOptions" Lude..=) Lude.<$> secretOptions,
            Lude.Just ("logDriver" Lude..= logDriver)
          ]
      )
