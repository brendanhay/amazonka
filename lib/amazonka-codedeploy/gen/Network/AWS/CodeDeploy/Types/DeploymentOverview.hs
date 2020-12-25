{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentOverview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentOverview
  ( DeploymentOverview (..),

    -- * Smart constructor
    mkDeploymentOverview,

    -- * Lenses
    doFailed,
    doInProgress,
    doPending,
    doReady,
    doSkipped,
    doSucceeded,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the deployment status of the instances in the deployment.
--
-- /See:/ 'mkDeploymentOverview' smart constructor.
data DeploymentOverview = DeploymentOverview'
  { -- | The number of instances in the deployment in a failed state.
    failed :: Core.Maybe Core.Integer,
    -- | The number of instances in which the deployment is in progress.
    inProgress :: Core.Maybe Core.Integer,
    -- | The number of instances in the deployment in a pending state.
    pending :: Core.Maybe Core.Integer,
    -- | The number of instances in a replacement environment ready to receive traffic in a blue/green deployment.
    ready :: Core.Maybe Core.Integer,
    -- | The number of instances in the deployment in a skipped state.
    skipped :: Core.Maybe Core.Integer,
    -- | The number of instances in the deployment to which revisions have been successfully deployed.
    succeeded :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeploymentOverview' value with any optional fields omitted.
mkDeploymentOverview ::
  DeploymentOverview
mkDeploymentOverview =
  DeploymentOverview'
    { failed = Core.Nothing,
      inProgress = Core.Nothing,
      pending = Core.Nothing,
      ready = Core.Nothing,
      skipped = Core.Nothing,
      succeeded = Core.Nothing
    }

-- | The number of instances in the deployment in a failed state.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doFailed :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
doFailed = Lens.field @"failed"
{-# DEPRECATED doFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The number of instances in which the deployment is in progress.
--
-- /Note:/ Consider using 'inProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doInProgress :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
doInProgress = Lens.field @"inProgress"
{-# DEPRECATED doInProgress "Use generic-lens or generic-optics with 'inProgress' instead." #-}

-- | The number of instances in the deployment in a pending state.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doPending :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
doPending = Lens.field @"pending"
{-# DEPRECATED doPending "Use generic-lens or generic-optics with 'pending' instead." #-}

-- | The number of instances in a replacement environment ready to receive traffic in a blue/green deployment.
--
-- /Note:/ Consider using 'ready' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doReady :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
doReady = Lens.field @"ready"
{-# DEPRECATED doReady "Use generic-lens or generic-optics with 'ready' instead." #-}

-- | The number of instances in the deployment in a skipped state.
--
-- /Note:/ Consider using 'skipped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doSkipped :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
doSkipped = Lens.field @"skipped"
{-# DEPRECATED doSkipped "Use generic-lens or generic-optics with 'skipped' instead." #-}

-- | The number of instances in the deployment to which revisions have been successfully deployed.
--
-- /Note:/ Consider using 'succeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doSucceeded :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
doSucceeded = Lens.field @"succeeded"
{-# DEPRECATED doSucceeded "Use generic-lens or generic-optics with 'succeeded' instead." #-}

instance Core.FromJSON DeploymentOverview where
  parseJSON =
    Core.withObject "DeploymentOverview" Core.$
      \x ->
        DeploymentOverview'
          Core.<$> (x Core..:? "Failed")
          Core.<*> (x Core..:? "InProgress")
          Core.<*> (x Core..:? "Pending")
          Core.<*> (x Core..:? "Ready")
          Core.<*> (x Core..:? "Skipped")
          Core.<*> (x Core..:? "Succeeded")
