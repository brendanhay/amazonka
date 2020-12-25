{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LastDeploymentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LastDeploymentInfo
  ( LastDeploymentInfo (..),

    -- * Smart constructor
    mkLastDeploymentInfo,

    -- * Lenses
    ldiCreateTime,
    ldiDeploymentId,
    ldiEndTime,
    ldiStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types.DeploymentId as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the most recent attempted or successful deployment to a deployment group.
--
-- /See:/ 'mkLastDeploymentInfo' smart constructor.
data LastDeploymentInfo = LastDeploymentInfo'
  { -- | A timestamp that indicates when the most recent deployment to the deployment group started.
    createTime :: Core.Maybe Core.NominalDiffTime,
    -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Types.DeploymentId,
    -- | A timestamp that indicates when the most recent deployment to the deployment group was complete.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the most recent deployment.
    status :: Core.Maybe Types.DeploymentStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LastDeploymentInfo' value with any optional fields omitted.
mkLastDeploymentInfo ::
  LastDeploymentInfo
mkLastDeploymentInfo =
  LastDeploymentInfo'
    { createTime = Core.Nothing,
      deploymentId = Core.Nothing,
      endTime = Core.Nothing,
      status = Core.Nothing
    }

-- | A timestamp that indicates when the most recent deployment to the deployment group started.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiCreateTime :: Lens.Lens' LastDeploymentInfo (Core.Maybe Core.NominalDiffTime)
ldiCreateTime = Lens.field @"createTime"
{-# DEPRECATED ldiCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiDeploymentId :: Lens.Lens' LastDeploymentInfo (Core.Maybe Types.DeploymentId)
ldiDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED ldiDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | A timestamp that indicates when the most recent deployment to the deployment group was complete.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiEndTime :: Lens.Lens' LastDeploymentInfo (Core.Maybe Core.NominalDiffTime)
ldiEndTime = Lens.field @"endTime"
{-# DEPRECATED ldiEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The status of the most recent deployment.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiStatus :: Lens.Lens' LastDeploymentInfo (Core.Maybe Types.DeploymentStatus)
ldiStatus = Lens.field @"status"
{-# DEPRECATED ldiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON LastDeploymentInfo where
  parseJSON =
    Core.withObject "LastDeploymentInfo" Core.$
      \x ->
        LastDeploymentInfo'
          Core.<$> (x Core..:? "createTime")
          Core.<*> (x Core..:? "deploymentId")
          Core.<*> (x Core..:? "endTime")
          Core.<*> (x Core..:? "status")
