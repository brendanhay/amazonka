{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.InstanceTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.InstanceTarget
  ( InstanceTarget (..),

    -- * Smart constructor
    mkInstanceTarget,

    -- * Lenses
    itDeploymentId,
    itInstanceLabel,
    itLastUpdatedAt,
    itLifecycleEvents,
    itStatus,
    itTargetArn,
    itTargetId,
  )
where

import qualified Network.AWS.CodeDeploy.Types.DeploymentId as Types
import qualified Network.AWS.CodeDeploy.Types.LifecycleEvent as Types
import qualified Network.AWS.CodeDeploy.Types.TargetArn as Types
import qualified Network.AWS.CodeDeploy.Types.TargetId as Types
import qualified Network.AWS.CodeDeploy.Types.TargetLabel as Types
import qualified Network.AWS.CodeDeploy.Types.TargetStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A target Amazon EC2 or on-premises instance during a deployment that uses the EC2/On-premises compute platform.
--
-- /See:/ 'mkInstanceTarget' smart constructor.
data InstanceTarget = InstanceTarget'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Types.DeploymentId,
    -- | A label that identifies whether the instance is an original target (@BLUE@ ) or a replacement target (@GREEN@ ).
    instanceLabel :: Core.Maybe Types.TargetLabel,
    -- | The date and time when the target instance was updated by a deployment.
    lastUpdatedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The lifecycle events of the deployment to this target instance.
    lifecycleEvents :: Core.Maybe [Types.LifecycleEvent],
    -- | The status an EC2/On-premises deployment's target instance.
    status :: Core.Maybe Types.TargetStatus,
    -- | The Amazon Resource Name (ARN) of the target.
    targetArn :: Core.Maybe Types.TargetArn,
    -- | The unique ID of a deployment target that has a type of @instanceTarget@ .
    targetId :: Core.Maybe Types.TargetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InstanceTarget' value with any optional fields omitted.
mkInstanceTarget ::
  InstanceTarget
mkInstanceTarget =
  InstanceTarget'
    { deploymentId = Core.Nothing,
      instanceLabel = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      lifecycleEvents = Core.Nothing,
      status = Core.Nothing,
      targetArn = Core.Nothing,
      targetId = Core.Nothing
    }

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itDeploymentId :: Lens.Lens' InstanceTarget (Core.Maybe Types.DeploymentId)
itDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED itDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | A label that identifies whether the instance is an original target (@BLUE@ ) or a replacement target (@GREEN@ ).
--
-- /Note:/ Consider using 'instanceLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itInstanceLabel :: Lens.Lens' InstanceTarget (Core.Maybe Types.TargetLabel)
itInstanceLabel = Lens.field @"instanceLabel"
{-# DEPRECATED itInstanceLabel "Use generic-lens or generic-optics with 'instanceLabel' instead." #-}

-- | The date and time when the target instance was updated by a deployment.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itLastUpdatedAt :: Lens.Lens' InstanceTarget (Core.Maybe Core.NominalDiffTime)
itLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# DEPRECATED itLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The lifecycle events of the deployment to this target instance.
--
-- /Note:/ Consider using 'lifecycleEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itLifecycleEvents :: Lens.Lens' InstanceTarget (Core.Maybe [Types.LifecycleEvent])
itLifecycleEvents = Lens.field @"lifecycleEvents"
{-# DEPRECATED itLifecycleEvents "Use generic-lens or generic-optics with 'lifecycleEvents' instead." #-}

-- | The status an EC2/On-premises deployment's target instance.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itStatus :: Lens.Lens' InstanceTarget (Core.Maybe Types.TargetStatus)
itStatus = Lens.field @"status"
{-# DEPRECATED itStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) of the target.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itTargetArn :: Lens.Lens' InstanceTarget (Core.Maybe Types.TargetArn)
itTargetArn = Lens.field @"targetArn"
{-# DEPRECATED itTargetArn "Use generic-lens or generic-optics with 'targetArn' instead." #-}

-- | The unique ID of a deployment target that has a type of @instanceTarget@ .
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itTargetId :: Lens.Lens' InstanceTarget (Core.Maybe Types.TargetId)
itTargetId = Lens.field @"targetId"
{-# DEPRECATED itTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

instance Core.FromJSON InstanceTarget where
  parseJSON =
    Core.withObject "InstanceTarget" Core.$
      \x ->
        InstanceTarget'
          Core.<$> (x Core..:? "deploymentId")
          Core.<*> (x Core..:? "instanceLabel")
          Core.<*> (x Core..:? "lastUpdatedAt")
          Core.<*> (x Core..:? "lifecycleEvents")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "targetArn")
          Core.<*> (x Core..:? "targetId")
