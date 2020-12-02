{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeploymentController
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentController where

import Network.AWS.ECS.Types.DeploymentControllerType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The deployment controller to use for the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'deploymentController' smart constructor.
newtype DeploymentController = DeploymentController'
  { _dcType ::
      DeploymentControllerType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeploymentController' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcType' - The deployment controller type to use. There are three deployment controller types available:     * ECS    * The rolling update (@ECS@ ) deployment type involves replacing the current running version of the container with the latest version. The number of containers Amazon ECS adds or removes from the service during a rolling update is controlled by adjusting the minimum and maximum number of healthy tasks allowed during a service deployment, as specified in the 'DeploymentConfiguration' .     * CODE_DEPLOY    * The blue/green (@CODE_DEPLOY@ ) deployment type uses the blue/green deployment model powered by AWS CodeDeploy, which allows you to verify a new deployment of a service before sending production traffic to it.     * EXTERNAL    * The external (@EXTERNAL@ ) deployment type enables you to use any third-party deployment controller for full control over the deployment process for an Amazon ECS service.
deploymentController ::
  -- | 'dcType'
  DeploymentControllerType ->
  DeploymentController
deploymentController pType_ =
  DeploymentController' {_dcType = pType_}

-- | The deployment controller type to use. There are three deployment controller types available:     * ECS    * The rolling update (@ECS@ ) deployment type involves replacing the current running version of the container with the latest version. The number of containers Amazon ECS adds or removes from the service during a rolling update is controlled by adjusting the minimum and maximum number of healthy tasks allowed during a service deployment, as specified in the 'DeploymentConfiguration' .     * CODE_DEPLOY    * The blue/green (@CODE_DEPLOY@ ) deployment type uses the blue/green deployment model powered by AWS CodeDeploy, which allows you to verify a new deployment of a service before sending production traffic to it.     * EXTERNAL    * The external (@EXTERNAL@ ) deployment type enables you to use any third-party deployment controller for full control over the deployment process for an Amazon ECS service.
dcType :: Lens' DeploymentController DeploymentControllerType
dcType = lens _dcType (\s a -> s {_dcType = a})

instance FromJSON DeploymentController where
  parseJSON =
    withObject
      "DeploymentController"
      (\x -> DeploymentController' <$> (x .: "type"))

instance Hashable DeploymentController

instance NFData DeploymentController

instance ToJSON DeploymentController where
  toJSON DeploymentController' {..} =
    object (catMaybes [Just ("type" .= _dcType)])
