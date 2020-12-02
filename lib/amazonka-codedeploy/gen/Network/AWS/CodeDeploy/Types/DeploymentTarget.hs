{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentTarget where

import Network.AWS.CodeDeploy.Types.CloudFormationTarget
import Network.AWS.CodeDeploy.Types.DeploymentTargetType
import Network.AWS.CodeDeploy.Types.ECSTarget
import Network.AWS.CodeDeploy.Types.InstanceTarget
import Network.AWS.CodeDeploy.Types.LambdaTarget
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the deployment target.
--
--
--
-- /See:/ 'deploymentTarget' smart constructor.
data DeploymentTarget = DeploymentTarget'
  { _dtInstanceTarget ::
      !(Maybe InstanceTarget),
    _dtCloudFormationTarget :: !(Maybe CloudFormationTarget),
    _dtEcsTarget :: !(Maybe ECSTarget),
    _dtDeploymentTargetType :: !(Maybe DeploymentTargetType),
    _dtLambdaTarget :: !(Maybe LambdaTarget)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeploymentTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtInstanceTarget' - Information about the target for a deployment that uses the EC2/On-premises compute platform.
--
-- * 'dtCloudFormationTarget' - Undocumented member.
--
-- * 'dtEcsTarget' - Information about the target for a deployment that uses the Amazon ECS compute platform.
--
-- * 'dtDeploymentTargetType' - The deployment type that is specific to the deployment's compute platform or deployments initiated by a CloudFormation stack update.
--
-- * 'dtLambdaTarget' - Information about the target for a deployment that uses the AWS Lambda compute platform.
deploymentTarget ::
  DeploymentTarget
deploymentTarget =
  DeploymentTarget'
    { _dtInstanceTarget = Nothing,
      _dtCloudFormationTarget = Nothing,
      _dtEcsTarget = Nothing,
      _dtDeploymentTargetType = Nothing,
      _dtLambdaTarget = Nothing
    }

-- | Information about the target for a deployment that uses the EC2/On-premises compute platform.
dtInstanceTarget :: Lens' DeploymentTarget (Maybe InstanceTarget)
dtInstanceTarget = lens _dtInstanceTarget (\s a -> s {_dtInstanceTarget = a})

-- | Undocumented member.
dtCloudFormationTarget :: Lens' DeploymentTarget (Maybe CloudFormationTarget)
dtCloudFormationTarget = lens _dtCloudFormationTarget (\s a -> s {_dtCloudFormationTarget = a})

-- | Information about the target for a deployment that uses the Amazon ECS compute platform.
dtEcsTarget :: Lens' DeploymentTarget (Maybe ECSTarget)
dtEcsTarget = lens _dtEcsTarget (\s a -> s {_dtEcsTarget = a})

-- | The deployment type that is specific to the deployment's compute platform or deployments initiated by a CloudFormation stack update.
dtDeploymentTargetType :: Lens' DeploymentTarget (Maybe DeploymentTargetType)
dtDeploymentTargetType = lens _dtDeploymentTargetType (\s a -> s {_dtDeploymentTargetType = a})

-- | Information about the target for a deployment that uses the AWS Lambda compute platform.
dtLambdaTarget :: Lens' DeploymentTarget (Maybe LambdaTarget)
dtLambdaTarget = lens _dtLambdaTarget (\s a -> s {_dtLambdaTarget = a})

instance FromJSON DeploymentTarget where
  parseJSON =
    withObject
      "DeploymentTarget"
      ( \x ->
          DeploymentTarget'
            <$> (x .:? "instanceTarget")
            <*> (x .:? "cloudFormationTarget")
            <*> (x .:? "ecsTarget")
            <*> (x .:? "deploymentTargetType")
            <*> (x .:? "lambdaTarget")
      )

instance Hashable DeploymentTarget

instance NFData DeploymentTarget
