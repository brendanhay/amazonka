{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeploymentConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentConfiguration where

import Network.AWS.ECS.Types.DeploymentCircuitBreaker
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Optional deployment parameters that control how many tasks run during a deployment and the ordering of stopping and starting tasks.
--
--
--
-- /See:/ 'deploymentConfiguration' smart constructor.
data DeploymentConfiguration = DeploymentConfiguration'
  { _dcMinimumHealthyPercent ::
      !(Maybe Int),
    _dcMaximumPercent :: !(Maybe Int),
    _dcDeploymentCircuitBreaker ::
      !(Maybe DeploymentCircuitBreaker)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeploymentConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcMinimumHealthyPercent' - If a service is using the rolling update (@ECS@ ) deployment type, the __minimum healthy percent__ represents a lower limit on the number of tasks in a service that must remain in the @RUNNING@ state during a deployment, as a percentage of the desired number of tasks (rounded up to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to deploy without using additional cluster capacity. For example, if your service has a desired number of four tasks and a minimum healthy percent of 50%, the scheduler may stop two existing tasks to free up cluster capacity before starting two new tasks. Tasks for services that /do not/ use a load balancer are considered healthy if they are in the @RUNNING@ state; tasks for services that /do/ use a load balancer are considered healthy if they are in the @RUNNING@ state and they are reported as healthy by the load balancer. The default value for minimum healthy percent is 100%. If a service is using the blue/green (@CODE_DEPLOY@ ) or @EXTERNAL@ deployment types and tasks that use the EC2 launch type, the __minimum healthy percent__ value is set to the default value and is used to define the lower limit on the number of the tasks in the service that remain in the @RUNNING@ state while the container instances are in the @DRAINING@ state. If the tasks in the service use the Fargate launch type, the minimum healthy percent value is not used, although it is returned when describing your service.
--
-- * 'dcMaximumPercent' - If a service is using the rolling update (@ECS@ ) deployment type, the __maximum percent__ parameter represents an upper limit on the number of tasks in a service that are allowed in the @RUNNING@ or @PENDING@ state during a deployment, as a percentage of the desired number of tasks (rounded down to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to define the deployment batch size. For example, if your service has a desired number of four tasks and a maximum percent value of 200%, the scheduler may start four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available). The default value for maximum percent is 200%. If a service is using the blue/green (@CODE_DEPLOY@ ) or @EXTERNAL@ deployment types and tasks that use the EC2 launch type, the __maximum percent__ value is set to the default value and is used to define the upper limit on the number of the tasks in the service that remain in the @RUNNING@ state while the container instances are in the @DRAINING@ state. If the tasks in the service use the Fargate launch type, the maximum percent value is not used, although it is returned when describing your service.
--
-- * 'dcDeploymentCircuitBreaker' - The __deployment circuit breaker__ determines whether a service deployment will fail if the service can't reach a steady state. If deployment circuit breaker is enabled, a service deployment will transition to a failed state and stop launching new tasks. If rollback is enabled, when a service deployment fails, the service is rolled back to the last deployment that completed successfully.
deploymentConfiguration ::
  DeploymentConfiguration
deploymentConfiguration =
  DeploymentConfiguration'
    { _dcMinimumHealthyPercent = Nothing,
      _dcMaximumPercent = Nothing,
      _dcDeploymentCircuitBreaker = Nothing
    }

-- | If a service is using the rolling update (@ECS@ ) deployment type, the __minimum healthy percent__ represents a lower limit on the number of tasks in a service that must remain in the @RUNNING@ state during a deployment, as a percentage of the desired number of tasks (rounded up to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to deploy without using additional cluster capacity. For example, if your service has a desired number of four tasks and a minimum healthy percent of 50%, the scheduler may stop two existing tasks to free up cluster capacity before starting two new tasks. Tasks for services that /do not/ use a load balancer are considered healthy if they are in the @RUNNING@ state; tasks for services that /do/ use a load balancer are considered healthy if they are in the @RUNNING@ state and they are reported as healthy by the load balancer. The default value for minimum healthy percent is 100%. If a service is using the blue/green (@CODE_DEPLOY@ ) or @EXTERNAL@ deployment types and tasks that use the EC2 launch type, the __minimum healthy percent__ value is set to the default value and is used to define the lower limit on the number of the tasks in the service that remain in the @RUNNING@ state while the container instances are in the @DRAINING@ state. If the tasks in the service use the Fargate launch type, the minimum healthy percent value is not used, although it is returned when describing your service.
dcMinimumHealthyPercent :: Lens' DeploymentConfiguration (Maybe Int)
dcMinimumHealthyPercent = lens _dcMinimumHealthyPercent (\s a -> s {_dcMinimumHealthyPercent = a})

-- | If a service is using the rolling update (@ECS@ ) deployment type, the __maximum percent__ parameter represents an upper limit on the number of tasks in a service that are allowed in the @RUNNING@ or @PENDING@ state during a deployment, as a percentage of the desired number of tasks (rounded down to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to define the deployment batch size. For example, if your service has a desired number of four tasks and a maximum percent value of 200%, the scheduler may start four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available). The default value for maximum percent is 200%. If a service is using the blue/green (@CODE_DEPLOY@ ) or @EXTERNAL@ deployment types and tasks that use the EC2 launch type, the __maximum percent__ value is set to the default value and is used to define the upper limit on the number of the tasks in the service that remain in the @RUNNING@ state while the container instances are in the @DRAINING@ state. If the tasks in the service use the Fargate launch type, the maximum percent value is not used, although it is returned when describing your service.
dcMaximumPercent :: Lens' DeploymentConfiguration (Maybe Int)
dcMaximumPercent = lens _dcMaximumPercent (\s a -> s {_dcMaximumPercent = a})

-- | The __deployment circuit breaker__ determines whether a service deployment will fail if the service can't reach a steady state. If deployment circuit breaker is enabled, a service deployment will transition to a failed state and stop launching new tasks. If rollback is enabled, when a service deployment fails, the service is rolled back to the last deployment that completed successfully.
dcDeploymentCircuitBreaker :: Lens' DeploymentConfiguration (Maybe DeploymentCircuitBreaker)
dcDeploymentCircuitBreaker = lens _dcDeploymentCircuitBreaker (\s a -> s {_dcDeploymentCircuitBreaker = a})

instance FromJSON DeploymentConfiguration where
  parseJSON =
    withObject
      "DeploymentConfiguration"
      ( \x ->
          DeploymentConfiguration'
            <$> (x .:? "minimumHealthyPercent")
            <*> (x .:? "maximumPercent")
            <*> (x .:? "deploymentCircuitBreaker")
      )

instance Hashable DeploymentConfiguration

instance NFData DeploymentConfiguration

instance ToJSON DeploymentConfiguration where
  toJSON DeploymentConfiguration' {..} =
    object
      ( catMaybes
          [ ("minimumHealthyPercent" .=) <$> _dcMinimumHealthyPercent,
            ("maximumPercent" .=) <$> _dcMaximumPercent,
            ("deploymentCircuitBreaker" .=) <$> _dcDeploymentCircuitBreaker
          ]
      )
