{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LogConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.LogConfiguration where

import Network.AWS.Batch.Types.LogDriver
import Network.AWS.Batch.Types.Secret
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Log configuration options to send to a custom log driver for the container.
--
--
--
-- /See:/ 'logConfiguration' smart constructor.
data LogConfiguration = LogConfiguration'
  { _lcOptions ::
      !(Maybe (Map Text (Text))),
    _lcSecretOptions :: !(Maybe [Secret]),
    _lcLogDriver :: !LogDriver
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcOptions' - The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
--
-- * 'lcSecretOptions' - The secrets to pass to the log configuration. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /AWS Batch User Guide/ .
--
-- * 'lcLogDriver' - The log driver to use for the container. The valid values listed for this parameter are log drivers that the Amazon ECS container agent can communicate with by default. The supported log drivers are @awslogs@ , @fluentd@ , @gelf@ , @json-file@ , @journald@ , @logentries@ , @syslog@ , and @splunk@ .     * awslogs    * Specifies the Amazon CloudWatch Logs logging driver. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/using_awslogs.html Using the awslogs Log Driver> in the /AWS Batch User Guide/ and <https://docs.docker.com/config/containers/logging/awslogs/ Amazon CloudWatch Logs logging driver> in the Docker documentation.     * fluentd    * Specifies the Fluentd logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/fluentd/ Fluentd logging driver> in the Docker documentation.     * gelf    * Specifies the Graylog Extended Format (GELF) logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/gelf/ Graylog Extended Format logging driver> in the Docker documentation.     * journald    * Specifies the journald logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/journald/ Journald logging driver> in the Docker documentation.     * json-file    * Specifies the JSON file logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/json-file/ JSON File logging driver> in the Docker documentation.     * splunk    * Specifies the Splunk logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/splunk/ Splunk logging driver> in the Docker documentation.     * syslog    * Specifies the syslog logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/syslog/ Syslog logging driver> in the Docker documentation. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
logConfiguration ::
  -- | 'lcLogDriver'
  LogDriver ->
  LogConfiguration
logConfiguration pLogDriver_ =
  LogConfiguration'
    { _lcOptions = Nothing,
      _lcSecretOptions = Nothing,
      _lcLogDriver = pLogDriver_
    }

-- | The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
lcOptions :: Lens' LogConfiguration (HashMap Text (Text))
lcOptions = lens _lcOptions (\s a -> s {_lcOptions = a}) . _Default . _Map

-- | The secrets to pass to the log configuration. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /AWS Batch User Guide/ .
lcSecretOptions :: Lens' LogConfiguration [Secret]
lcSecretOptions = lens _lcSecretOptions (\s a -> s {_lcSecretOptions = a}) . _Default . _Coerce

-- | The log driver to use for the container. The valid values listed for this parameter are log drivers that the Amazon ECS container agent can communicate with by default. The supported log drivers are @awslogs@ , @fluentd@ , @gelf@ , @json-file@ , @journald@ , @logentries@ , @syslog@ , and @splunk@ .     * awslogs    * Specifies the Amazon CloudWatch Logs logging driver. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/using_awslogs.html Using the awslogs Log Driver> in the /AWS Batch User Guide/ and <https://docs.docker.com/config/containers/logging/awslogs/ Amazon CloudWatch Logs logging driver> in the Docker documentation.     * fluentd    * Specifies the Fluentd logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/fluentd/ Fluentd logging driver> in the Docker documentation.     * gelf    * Specifies the Graylog Extended Format (GELF) logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/gelf/ Graylog Extended Format logging driver> in the Docker documentation.     * journald    * Specifies the journald logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/journald/ Journald logging driver> in the Docker documentation.     * json-file    * Specifies the JSON file logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/json-file/ JSON File logging driver> in the Docker documentation.     * splunk    * Specifies the Splunk logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/splunk/ Splunk logging driver> in the Docker documentation.     * syslog    * Specifies the syslog logging driver. For more information, including usage and options, see <https://docs.docker.com/config/containers/logging/syslog/ Syslog logging driver> in the Docker documentation. This parameter requires version 1.18 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log into your container instance and run the following command: @sudo docker version | grep "Server API version"@
lcLogDriver :: Lens' LogConfiguration LogDriver
lcLogDriver = lens _lcLogDriver (\s a -> s {_lcLogDriver = a})

instance FromJSON LogConfiguration where
  parseJSON =
    withObject
      "LogConfiguration"
      ( \x ->
          LogConfiguration'
            <$> (x .:? "options" .!= mempty)
            <*> (x .:? "secretOptions" .!= mempty)
            <*> (x .: "logDriver")
      )

instance Hashable LogConfiguration

instance NFData LogConfiguration

instance ToJSON LogConfiguration where
  toJSON LogConfiguration' {..} =
    object
      ( catMaybes
          [ ("options" .=) <$> _lcOptions,
            ("secretOptions" .=) <$> _lcSecretOptions,
            Just ("logDriver" .= _lcLogDriver)
          ]
      )
