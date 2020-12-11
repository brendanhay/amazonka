-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LogConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.LogConfiguration
  ( LogConfiguration (..),

    -- * Smart constructor
    mkLogConfiguration,

    -- * Lenses
    lcOptions,
    lcSecretOptions,
    lcLogDriver,
  )
where

import Network.AWS.Batch.Types.LogDriver
import Network.AWS.Batch.Types.Secret
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Log configuration options to send to a custom log driver for the container.
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
-- * 'logDriver' - The log driver to use for the container. The valid values listed for this parameter are log drivers that the Amazon ECS container agent can communicate with by default.
--
-- The supported log drivers are @awslogs@ , @fluentd@ , @gelf@ , @json-file@ , @journald@ , @logentries@ , @syslog@ , and @splunk@ .
--
--     * awslogs
--
--     * Specifies the Amazon CloudWatch Logs logging driver. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/using_awslogs.html Using the awslogs Log Driver> in the /AWS Batch User Guide/ and <https://docs.docker.com/config/containers/logging/awslogs/ Amazon CloudWatch Logs logging driver> in the Docker documentation.
--
--
--     * fluentd
--
--     * Specifies the Fluentd logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/fluentd/ Fluentd logging driver> in the Docker documentation.
--
--
--     * gelf
--
--     * Specifies the Graylog Extended Format (GELF) logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/gelf/ Graylog Extended Format logging driver> in the Docker documentation.
--
--
--     * journald
--
--     * Specifies the journald logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/journald/ Journald logging driver> in the Docker documentation.
--
--
--     * json-file
--
--     * Specifies the JSON file logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/json-file/ JSON File logging driver> in the Docker documentation.
--
--
--     * splunk
--
--     * Specifies the Splunk logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/splunk/ Splunk logging driver> in the Docker documentation.
--
--
--     * syslog
--
--     * Specifies the syslog logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/syslog/ Syslog logging driver> in the Docker documentation.
--
--
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
-- * 'options' - The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
-- * 'secretOptions' - The secrets to pass to the log configuration. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /AWS Batch User Guide/ .
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

-- | The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcOptions :: Lens.Lens' LogConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
lcOptions = Lens.lens (options :: LogConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {options = a} :: LogConfiguration)
{-# DEPRECATED lcOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The secrets to pass to the log configuration. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'secretOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcSecretOptions :: Lens.Lens' LogConfiguration (Lude.Maybe [Secret])
lcSecretOptions = Lens.lens (secretOptions :: LogConfiguration -> Lude.Maybe [Secret]) (\s a -> s {secretOptions = a} :: LogConfiguration)
{-# DEPRECATED lcSecretOptions "Use generic-lens or generic-optics with 'secretOptions' instead." #-}

-- | The log driver to use for the container. The valid values listed for this parameter are log drivers that the Amazon ECS container agent can communicate with by default.
--
-- The supported log drivers are @awslogs@ , @fluentd@ , @gelf@ , @json-file@ , @journald@ , @logentries@ , @syslog@ , and @splunk@ .
--
--     * awslogs
--
--     * Specifies the Amazon CloudWatch Logs logging driver. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/using_awslogs.html Using the awslogs Log Driver> in the /AWS Batch User Guide/ and <https://docs.docker.com/config/containers/logging/awslogs/ Amazon CloudWatch Logs logging driver> in the Docker documentation.
--
--
--     * fluentd
--
--     * Specifies the Fluentd logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/fluentd/ Fluentd logging driver> in the Docker documentation.
--
--
--     * gelf
--
--     * Specifies the Graylog Extended Format (GELF) logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/gelf/ Graylog Extended Format logging driver> in the Docker documentation.
--
--
--     * journald
--
--     * Specifies the journald logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/journald/ Journald logging driver> in the Docker documentation.
--
--
--     * json-file
--
--     * Specifies the JSON file logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/json-file/ JSON File logging driver> in the Docker documentation.
--
--
--     * splunk
--
--     * Specifies the Splunk logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/splunk/ Splunk logging driver> in the Docker documentation.
--
--
--     * syslog
--
--     * Specifies the syslog logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/syslog/ Syslog logging driver> in the Docker documentation.
--
--
-- This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
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
