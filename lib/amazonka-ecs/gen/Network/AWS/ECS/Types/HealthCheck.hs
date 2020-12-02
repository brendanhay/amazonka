{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.HealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HealthCheck where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a container health check. Health check parameters that are specified in a container definition override any Docker health checks that exist in the container image (such as those specified in a parent image or from the image's Dockerfile).
--
--
-- You can view the health status of both individual containers and a task with the DescribeTasks API operation or when viewing the task details in the console.
--
-- The following describes the possible @healthStatus@ values for a container:
--
--     * @HEALTHY@ -The container health check has passed successfully.
--
--     * @UNHEALTHY@ -The container health check has failed.
--
--     * @UNKNOWN@ -The container health check is being evaluated or there is no container health check defined.
--
--
--
-- The following describes the possible @healthStatus@ values for a task. The container health check status of nonessential containers do not have an effect on the health status of a task.
--
--     * @HEALTHY@ -All essential containers within the task have passed their health checks.
--
--     * @UNHEALTHY@ -One or more essential containers have failed their health check.
--
--     * @UNKNOWN@ -The essential containers within the task are still having their health checks evaluated or there are no container health checks defined.
--
--
--
-- If a task is run manually, and not as part of a service, the task will continue its lifecycle regardless of its health status. For tasks that are part of a service, if the task reports as unhealthy then the task will be stopped and the service scheduler will replace it.
--
-- The following are notes about container health check support:
--
--     * Container health checks require version 1.17.0 or greater of the Amazon ECS container agent. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent> .
--
--     * Container health checks are supported for Fargate tasks if you are using platform version 1.1.0 or greater. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> .
--
--     * Container health checks are not supported for tasks that are part of a service that is configured to use a Classic Load Balancer.
--
--
--
--
-- /See:/ 'healthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { _hcStartPeriod :: !(Maybe Int),
    _hcRetries :: !(Maybe Int),
    _hcInterval :: !(Maybe Int),
    _hcTimeout :: !(Maybe Int),
    _hcCommand :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcStartPeriod' - The optional grace period within which to provide containers time to bootstrap before failed health checks count towards the maximum number of retries. You may specify between 0 and 300 seconds. The @startPeriod@ is disabled by default.
--
-- * 'hcRetries' - The number of times to retry a failed health check before the container is considered unhealthy. You may specify between 1 and 10 retries. The default value is 3.
--
-- * 'hcInterval' - The time period in seconds between each health check execution. You may specify between 5 and 300 seconds. The default value is 30 seconds.
--
-- * 'hcTimeout' - The time period in seconds to wait for a health check to succeed before it is considered a failure. You may specify between 2 and 60 seconds. The default value is 5.
--
-- * 'hcCommand' - A string array representing the command that the container runs to determine if it is healthy. The string array must start with @CMD@ to execute the command arguments directly, or @CMD-SHELL@ to run the command with the container's default shell. For example: @[ "CMD-SHELL", "curl -f http://localhost/ || exit 1" ]@  An exit code of 0 indicates success, and non-zero exit code indicates failure. For more information, see @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
healthCheck ::
  HealthCheck
healthCheck =
  HealthCheck'
    { _hcStartPeriod = Nothing,
      _hcRetries = Nothing,
      _hcInterval = Nothing,
      _hcTimeout = Nothing,
      _hcCommand = mempty
    }

-- | The optional grace period within which to provide containers time to bootstrap before failed health checks count towards the maximum number of retries. You may specify between 0 and 300 seconds. The @startPeriod@ is disabled by default.
hcStartPeriod :: Lens' HealthCheck (Maybe Int)
hcStartPeriod = lens _hcStartPeriod (\s a -> s {_hcStartPeriod = a})

-- | The number of times to retry a failed health check before the container is considered unhealthy. You may specify between 1 and 10 retries. The default value is 3.
hcRetries :: Lens' HealthCheck (Maybe Int)
hcRetries = lens _hcRetries (\s a -> s {_hcRetries = a})

-- | The time period in seconds between each health check execution. You may specify between 5 and 300 seconds. The default value is 30 seconds.
hcInterval :: Lens' HealthCheck (Maybe Int)
hcInterval = lens _hcInterval (\s a -> s {_hcInterval = a})

-- | The time period in seconds to wait for a health check to succeed before it is considered a failure. You may specify between 2 and 60 seconds. The default value is 5.
hcTimeout :: Lens' HealthCheck (Maybe Int)
hcTimeout = lens _hcTimeout (\s a -> s {_hcTimeout = a})

-- | A string array representing the command that the container runs to determine if it is healthy. The string array must start with @CMD@ to execute the command arguments directly, or @CMD-SHELL@ to run the command with the container's default shell. For example: @[ "CMD-SHELL", "curl -f http://localhost/ || exit 1" ]@  An exit code of 0 indicates success, and non-zero exit code indicates failure. For more information, see @HealthCheck@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> .
hcCommand :: Lens' HealthCheck [Text]
hcCommand = lens _hcCommand (\s a -> s {_hcCommand = a}) . _Coerce

instance FromJSON HealthCheck where
  parseJSON =
    withObject
      "HealthCheck"
      ( \x ->
          HealthCheck'
            <$> (x .:? "startPeriod")
            <*> (x .:? "retries")
            <*> (x .:? "interval")
            <*> (x .:? "timeout")
            <*> (x .:? "command" .!= mempty)
      )

instance Hashable HealthCheck

instance NFData HealthCheck

instance ToJSON HealthCheck where
  toJSON HealthCheck' {..} =
    object
      ( catMaybes
          [ ("startPeriod" .=) <$> _hcStartPeriod,
            ("retries" .=) <$> _hcRetries,
            ("interval" .=) <$> _hcInterval,
            ("timeout" .=) <$> _hcTimeout,
            Just ("command" .= _hcCommand)
          ]
      )
