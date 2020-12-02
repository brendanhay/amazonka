{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentReadyOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentReadyOption where

import Network.AWS.CodeDeploy.Types.DeploymentReadyAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about how traffic is rerouted to instances in a replacement environment in a blue/green deployment.
--
--
--
-- /See:/ 'deploymentReadyOption' smart constructor.
data DeploymentReadyOption = DeploymentReadyOption'
  { _droActionOnTimeout ::
      !(Maybe DeploymentReadyAction),
    _droWaitTimeInMinutes :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeploymentReadyOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'droActionOnTimeout' - Information about when to reroute traffic from an original environment to a replacement environment in a blue/green deployment.     * CONTINUE_DEPLOYMENT: Register new instances with the load balancer immediately after the new application revision is installed on the instances in the replacement environment.     * STOP_DEPLOYMENT: Do not register new instances with a load balancer unless traffic rerouting is started using 'ContinueDeployment' . If traffic rerouting is not started before the end of the specified wait period, the deployment status is changed to Stopped.
--
-- * 'droWaitTimeInMinutes' - The number of minutes to wait before the status of a blue/green deployment is changed to Stopped if rerouting is not started manually. Applies only to the @STOP_DEPLOYMENT@ option for @actionOnTimeout@ .
deploymentReadyOption ::
  DeploymentReadyOption
deploymentReadyOption =
  DeploymentReadyOption'
    { _droActionOnTimeout = Nothing,
      _droWaitTimeInMinutes = Nothing
    }

-- | Information about when to reroute traffic from an original environment to a replacement environment in a blue/green deployment.     * CONTINUE_DEPLOYMENT: Register new instances with the load balancer immediately after the new application revision is installed on the instances in the replacement environment.     * STOP_DEPLOYMENT: Do not register new instances with a load balancer unless traffic rerouting is started using 'ContinueDeployment' . If traffic rerouting is not started before the end of the specified wait period, the deployment status is changed to Stopped.
droActionOnTimeout :: Lens' DeploymentReadyOption (Maybe DeploymentReadyAction)
droActionOnTimeout = lens _droActionOnTimeout (\s a -> s {_droActionOnTimeout = a})

-- | The number of minutes to wait before the status of a blue/green deployment is changed to Stopped if rerouting is not started manually. Applies only to the @STOP_DEPLOYMENT@ option for @actionOnTimeout@ .
droWaitTimeInMinutes :: Lens' DeploymentReadyOption (Maybe Int)
droWaitTimeInMinutes = lens _droWaitTimeInMinutes (\s a -> s {_droWaitTimeInMinutes = a})

instance FromJSON DeploymentReadyOption where
  parseJSON =
    withObject
      "DeploymentReadyOption"
      ( \x ->
          DeploymentReadyOption'
            <$> (x .:? "actionOnTimeout") <*> (x .:? "waitTimeInMinutes")
      )

instance Hashable DeploymentReadyOption

instance NFData DeploymentReadyOption

instance ToJSON DeploymentReadyOption where
  toJSON DeploymentReadyOption' {..} =
    object
      ( catMaybes
          [ ("actionOnTimeout" .=) <$> _droActionOnTimeout,
            ("waitTimeInMinutes" .=) <$> _droWaitTimeInMinutes
          ]
      )
