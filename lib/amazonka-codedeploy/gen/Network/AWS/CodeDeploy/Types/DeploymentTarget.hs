-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentTarget
  ( DeploymentTarget (..),

    -- * Smart constructor
    mkDeploymentTarget,

    -- * Lenses
    dtInstanceTarget,
    dtCloudFormationTarget,
    dtEcsTarget,
    dtDeploymentTargetType,
    dtLambdaTarget,
  )
where

import Network.AWS.CodeDeploy.Types.CloudFormationTarget
import Network.AWS.CodeDeploy.Types.DeploymentTargetType
import Network.AWS.CodeDeploy.Types.ECSTarget
import Network.AWS.CodeDeploy.Types.InstanceTarget
import Network.AWS.CodeDeploy.Types.LambdaTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the deployment target.
--
-- /See:/ 'mkDeploymentTarget' smart constructor.
data DeploymentTarget = DeploymentTarget'
  { instanceTarget ::
      Lude.Maybe InstanceTarget,
    cloudFormationTarget :: Lude.Maybe CloudFormationTarget,
    ecsTarget :: Lude.Maybe ECSTarget,
    deploymentTargetType :: Lude.Maybe DeploymentTargetType,
    lambdaTarget :: Lude.Maybe LambdaTarget
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentTarget' with the minimum fields required to make a request.
--
-- * 'cloudFormationTarget' - Undocumented field.
-- * 'deploymentTargetType' - The deployment type that is specific to the deployment's compute platform or deployments initiated by a CloudFormation stack update.
-- * 'ecsTarget' - Information about the target for a deployment that uses the Amazon ECS compute platform.
-- * 'instanceTarget' - Information about the target for a deployment that uses the EC2/On-premises compute platform.
-- * 'lambdaTarget' - Information about the target for a deployment that uses the AWS Lambda compute platform.
mkDeploymentTarget ::
  DeploymentTarget
mkDeploymentTarget =
  DeploymentTarget'
    { instanceTarget = Lude.Nothing,
      cloudFormationTarget = Lude.Nothing,
      ecsTarget = Lude.Nothing,
      deploymentTargetType = Lude.Nothing,
      lambdaTarget = Lude.Nothing
    }

-- | Information about the target for a deployment that uses the EC2/On-premises compute platform.
--
-- /Note:/ Consider using 'instanceTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtInstanceTarget :: Lens.Lens' DeploymentTarget (Lude.Maybe InstanceTarget)
dtInstanceTarget = Lens.lens (instanceTarget :: DeploymentTarget -> Lude.Maybe InstanceTarget) (\s a -> s {instanceTarget = a} :: DeploymentTarget)
{-# DEPRECATED dtInstanceTarget "Use generic-lens or generic-optics with 'instanceTarget' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudFormationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtCloudFormationTarget :: Lens.Lens' DeploymentTarget (Lude.Maybe CloudFormationTarget)
dtCloudFormationTarget = Lens.lens (cloudFormationTarget :: DeploymentTarget -> Lude.Maybe CloudFormationTarget) (\s a -> s {cloudFormationTarget = a} :: DeploymentTarget)
{-# DEPRECATED dtCloudFormationTarget "Use generic-lens or generic-optics with 'cloudFormationTarget' instead." #-}

-- | Information about the target for a deployment that uses the Amazon ECS compute platform.
--
-- /Note:/ Consider using 'ecsTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtEcsTarget :: Lens.Lens' DeploymentTarget (Lude.Maybe ECSTarget)
dtEcsTarget = Lens.lens (ecsTarget :: DeploymentTarget -> Lude.Maybe ECSTarget) (\s a -> s {ecsTarget = a} :: DeploymentTarget)
{-# DEPRECATED dtEcsTarget "Use generic-lens or generic-optics with 'ecsTarget' instead." #-}

-- | The deployment type that is specific to the deployment's compute platform or deployments initiated by a CloudFormation stack update.
--
-- /Note:/ Consider using 'deploymentTargetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeploymentTargetType :: Lens.Lens' DeploymentTarget (Lude.Maybe DeploymentTargetType)
dtDeploymentTargetType = Lens.lens (deploymentTargetType :: DeploymentTarget -> Lude.Maybe DeploymentTargetType) (\s a -> s {deploymentTargetType = a} :: DeploymentTarget)
{-# DEPRECATED dtDeploymentTargetType "Use generic-lens or generic-optics with 'deploymentTargetType' instead." #-}

-- | Information about the target for a deployment that uses the AWS Lambda compute platform.
--
-- /Note:/ Consider using 'lambdaTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtLambdaTarget :: Lens.Lens' DeploymentTarget (Lude.Maybe LambdaTarget)
dtLambdaTarget = Lens.lens (lambdaTarget :: DeploymentTarget -> Lude.Maybe LambdaTarget) (\s a -> s {lambdaTarget = a} :: DeploymentTarget)
{-# DEPRECATED dtLambdaTarget "Use generic-lens or generic-optics with 'lambdaTarget' instead." #-}

instance Lude.FromJSON DeploymentTarget where
  parseJSON =
    Lude.withObject
      "DeploymentTarget"
      ( \x ->
          DeploymentTarget'
            Lude.<$> (x Lude..:? "instanceTarget")
            Lude.<*> (x Lude..:? "cloudFormationTarget")
            Lude.<*> (x Lude..:? "ecsTarget")
            Lude.<*> (x Lude..:? "deploymentTargetType")
            Lude.<*> (x Lude..:? "lambdaTarget")
      )
