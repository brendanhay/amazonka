{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LambdaTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LambdaTarget
  ( LambdaTarget (..),

    -- * Smart constructor
    mkLambdaTarget,

    -- * Lenses
    ltDeploymentId,
    ltLambdaFunctionInfo,
    ltLastUpdatedAt,
    ltLifecycleEvents,
    ltStatus,
    ltTargetArn,
    ltTargetId,
  )
where

import qualified Network.AWS.CodeDeploy.Types.DeploymentId as Types
import qualified Network.AWS.CodeDeploy.Types.LambdaFunctionInfo as Types
import qualified Network.AWS.CodeDeploy.Types.LifecycleEvent as Types
import qualified Network.AWS.CodeDeploy.Types.TargetArn as Types
import qualified Network.AWS.CodeDeploy.Types.TargetId as Types
import qualified Network.AWS.CodeDeploy.Types.TargetStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the target AWS Lambda function during an AWS Lambda deployment.
--
-- /See:/ 'mkLambdaTarget' smart constructor.
data LambdaTarget = LambdaTarget'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Types.DeploymentId,
    -- | A @LambdaFunctionInfo@ object that describes a target Lambda function.
    lambdaFunctionInfo :: Core.Maybe Types.LambdaFunctionInfo,
    -- | The date and time when the target Lambda function was updated by a deployment.
    lastUpdatedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The lifecycle events of the deployment to this target Lambda function.
    lifecycleEvents :: Core.Maybe [Types.LifecycleEvent],
    -- | The status an AWS Lambda deployment's target Lambda function.
    status :: Core.Maybe Types.TargetStatus,
    -- | The Amazon Resource Name (ARN) of the target.
    targetArn :: Core.Maybe Types.TargetArn,
    -- | The unique ID of a deployment target that has a type of @lambdaTarget@ .
    targetId :: Core.Maybe Types.TargetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LambdaTarget' value with any optional fields omitted.
mkLambdaTarget ::
  LambdaTarget
mkLambdaTarget =
  LambdaTarget'
    { deploymentId = Core.Nothing,
      lambdaFunctionInfo = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      lifecycleEvents = Core.Nothing,
      status = Core.Nothing,
      targetArn = Core.Nothing,
      targetId = Core.Nothing
    }

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDeploymentId :: Lens.Lens' LambdaTarget (Core.Maybe Types.DeploymentId)
ltDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED ltDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | A @LambdaFunctionInfo@ object that describes a target Lambda function.
--
-- /Note:/ Consider using 'lambdaFunctionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLambdaFunctionInfo :: Lens.Lens' LambdaTarget (Core.Maybe Types.LambdaFunctionInfo)
ltLambdaFunctionInfo = Lens.field @"lambdaFunctionInfo"
{-# DEPRECATED ltLambdaFunctionInfo "Use generic-lens or generic-optics with 'lambdaFunctionInfo' instead." #-}

-- | The date and time when the target Lambda function was updated by a deployment.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLastUpdatedAt :: Lens.Lens' LambdaTarget (Core.Maybe Core.NominalDiffTime)
ltLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# DEPRECATED ltLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The lifecycle events of the deployment to this target Lambda function.
--
-- /Note:/ Consider using 'lifecycleEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLifecycleEvents :: Lens.Lens' LambdaTarget (Core.Maybe [Types.LifecycleEvent])
ltLifecycleEvents = Lens.field @"lifecycleEvents"
{-# DEPRECATED ltLifecycleEvents "Use generic-lens or generic-optics with 'lifecycleEvents' instead." #-}

-- | The status an AWS Lambda deployment's target Lambda function.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltStatus :: Lens.Lens' LambdaTarget (Core.Maybe Types.TargetStatus)
ltStatus = Lens.field @"status"
{-# DEPRECATED ltStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) of the target.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTargetArn :: Lens.Lens' LambdaTarget (Core.Maybe Types.TargetArn)
ltTargetArn = Lens.field @"targetArn"
{-# DEPRECATED ltTargetArn "Use generic-lens or generic-optics with 'targetArn' instead." #-}

-- | The unique ID of a deployment target that has a type of @lambdaTarget@ .
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTargetId :: Lens.Lens' LambdaTarget (Core.Maybe Types.TargetId)
ltTargetId = Lens.field @"targetId"
{-# DEPRECATED ltTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

instance Core.FromJSON LambdaTarget where
  parseJSON =
    Core.withObject "LambdaTarget" Core.$
      \x ->
        LambdaTarget'
          Core.<$> (x Core..:? "deploymentId")
          Core.<*> (x Core..:? "lambdaFunctionInfo")
          Core.<*> (x Core..:? "lastUpdatedAt")
          Core.<*> (x Core..:? "lifecycleEvents")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "targetArn")
          Core.<*> (x Core..:? "targetId")
