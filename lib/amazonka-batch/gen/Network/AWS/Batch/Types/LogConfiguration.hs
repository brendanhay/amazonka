{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LogConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.LogConfiguration
  ( LogConfiguration (..)
  -- * Smart constructor
  , mkLogConfiguration
  -- * Lenses
  , lcLogDriver
  , lcOptions
  , lcSecretOptions
  ) where

import qualified Network.AWS.Batch.Types.LogDriver as Types
import qualified Network.AWS.Batch.Types.Secret as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Log configuration options to send to a custom log driver for the container.
--
-- /See:/ 'mkLogConfiguration' smart constructor.
data LogConfiguration = LogConfiguration'
  { logDriver :: Types.LogDriver
    -- ^ The log driver to use for the container. The valid values listed for this parameter are log drivers that the Amazon ECS container agent can communicate with by default.
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
  , options :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@ 
  , secretOptions :: Core.Maybe [Types.Secret]
    -- ^ The secrets to pass to the log configuration. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /AWS Batch User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogConfiguration' value with any optional fields omitted.
mkLogConfiguration
    :: Types.LogDriver -- ^ 'logDriver'
    -> LogConfiguration
mkLogConfiguration logDriver
  = LogConfiguration'{logDriver, options = Core.Nothing,
                      secretOptions = Core.Nothing}

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
lcLogDriver :: Lens.Lens' LogConfiguration Types.LogDriver
lcLogDriver = Lens.field @"logDriver"
{-# INLINEABLE lcLogDriver #-}
{-# DEPRECATED logDriver "Use generic-lens or generic-optics with 'logDriver' instead"  #-}

-- | The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@ 
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcOptions :: Lens.Lens' LogConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
lcOptions = Lens.field @"options"
{-# INLINEABLE lcOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The secrets to pass to the log configuration. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'secretOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcSecretOptions :: Lens.Lens' LogConfiguration (Core.Maybe [Types.Secret])
lcSecretOptions = Lens.field @"secretOptions"
{-# INLINEABLE lcSecretOptions #-}
{-# DEPRECATED secretOptions "Use generic-lens or generic-optics with 'secretOptions' instead"  #-}

instance Core.FromJSON LogConfiguration where
        toJSON LogConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logDriver" Core..= logDriver),
                  ("options" Core..=) Core.<$> options,
                  ("secretOptions" Core..=) Core.<$> secretOptions])

instance Core.FromJSON LogConfiguration where
        parseJSON
          = Core.withObject "LogConfiguration" Core.$
              \ x ->
                LogConfiguration' Core.<$>
                  (x Core..: "logDriver") Core.<*> x Core..:? "options" Core.<*>
                    x Core..:? "secretOptions"
