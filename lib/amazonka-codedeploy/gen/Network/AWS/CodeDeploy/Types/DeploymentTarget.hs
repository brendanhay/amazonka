{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dtCloudFormationTarget,
    dtDeploymentTargetType,
    dtEcsTarget,
    dtInstanceTarget,
    dtLambdaTarget,
  )
where

import qualified Network.AWS.CodeDeploy.Types.CloudFormationTarget as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentTargetType as Types
import qualified Network.AWS.CodeDeploy.Types.ECSTarget as Types
import qualified Network.AWS.CodeDeploy.Types.InstanceTarget as Types
import qualified Network.AWS.CodeDeploy.Types.LambdaTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the deployment target.
--
-- /See:/ 'mkDeploymentTarget' smart constructor.
data DeploymentTarget = DeploymentTarget'
  { cloudFormationTarget :: Core.Maybe Types.CloudFormationTarget,
    -- | The deployment type that is specific to the deployment's compute platform or deployments initiated by a CloudFormation stack update.
    deploymentTargetType :: Core.Maybe Types.DeploymentTargetType,
    -- | Information about the target for a deployment that uses the Amazon ECS compute platform.
    ecsTarget :: Core.Maybe Types.ECSTarget,
    -- | Information about the target for a deployment that uses the EC2/On-premises compute platform.
    instanceTarget :: Core.Maybe Types.InstanceTarget,
    -- | Information about the target for a deployment that uses the AWS Lambda compute platform.
    lambdaTarget :: Core.Maybe Types.LambdaTarget
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeploymentTarget' value with any optional fields omitted.
mkDeploymentTarget ::
  DeploymentTarget
mkDeploymentTarget =
  DeploymentTarget'
    { cloudFormationTarget = Core.Nothing,
      deploymentTargetType = Core.Nothing,
      ecsTarget = Core.Nothing,
      instanceTarget = Core.Nothing,
      lambdaTarget = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudFormationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtCloudFormationTarget :: Lens.Lens' DeploymentTarget (Core.Maybe Types.CloudFormationTarget)
dtCloudFormationTarget = Lens.field @"cloudFormationTarget"
{-# DEPRECATED dtCloudFormationTarget "Use generic-lens or generic-optics with 'cloudFormationTarget' instead." #-}

-- | The deployment type that is specific to the deployment's compute platform or deployments initiated by a CloudFormation stack update.
--
-- /Note:/ Consider using 'deploymentTargetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeploymentTargetType :: Lens.Lens' DeploymentTarget (Core.Maybe Types.DeploymentTargetType)
dtDeploymentTargetType = Lens.field @"deploymentTargetType"
{-# DEPRECATED dtDeploymentTargetType "Use generic-lens or generic-optics with 'deploymentTargetType' instead." #-}

-- | Information about the target for a deployment that uses the Amazon ECS compute platform.
--
-- /Note:/ Consider using 'ecsTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtEcsTarget :: Lens.Lens' DeploymentTarget (Core.Maybe Types.ECSTarget)
dtEcsTarget = Lens.field @"ecsTarget"
{-# DEPRECATED dtEcsTarget "Use generic-lens or generic-optics with 'ecsTarget' instead." #-}

-- | Information about the target for a deployment that uses the EC2/On-premises compute platform.
--
-- /Note:/ Consider using 'instanceTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtInstanceTarget :: Lens.Lens' DeploymentTarget (Core.Maybe Types.InstanceTarget)
dtInstanceTarget = Lens.field @"instanceTarget"
{-# DEPRECATED dtInstanceTarget "Use generic-lens or generic-optics with 'instanceTarget' instead." #-}

-- | Information about the target for a deployment that uses the AWS Lambda compute platform.
--
-- /Note:/ Consider using 'lambdaTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtLambdaTarget :: Lens.Lens' DeploymentTarget (Core.Maybe Types.LambdaTarget)
dtLambdaTarget = Lens.field @"lambdaTarget"
{-# DEPRECATED dtLambdaTarget "Use generic-lens or generic-optics with 'lambdaTarget' instead." #-}

instance Core.FromJSON DeploymentTarget where
  parseJSON =
    Core.withObject "DeploymentTarget" Core.$
      \x ->
        DeploymentTarget'
          Core.<$> (x Core..:? "cloudFormationTarget")
          Core.<*> (x Core..:? "deploymentTargetType")
          Core.<*> (x Core..:? "ecsTarget")
          Core.<*> (x Core..:? "instanceTarget")
          Core.<*> (x Core..:? "lambdaTarget")
