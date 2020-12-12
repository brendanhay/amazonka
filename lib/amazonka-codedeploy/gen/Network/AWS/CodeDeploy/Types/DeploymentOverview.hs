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
    doPending,
    doSkipped,
    doInProgress,
    doSucceeded,
    doReady,
    doFailed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the deployment status of the instances in the deployment.
--
-- /See:/ 'mkDeploymentOverview' smart constructor.
data DeploymentOverview = DeploymentOverview'
  { pending ::
      Lude.Maybe Lude.Integer,
    skipped :: Lude.Maybe Lude.Integer,
    inProgress :: Lude.Maybe Lude.Integer,
    succeeded :: Lude.Maybe Lude.Integer,
    ready :: Lude.Maybe Lude.Integer,
    failed :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentOverview' with the minimum fields required to make a request.
--
-- * 'failed' - The number of instances in the deployment in a failed state.
-- * 'inProgress' - The number of instances in which the deployment is in progress.
-- * 'pending' - The number of instances in the deployment in a pending state.
-- * 'ready' - The number of instances in a replacement environment ready to receive traffic in a blue/green deployment.
-- * 'skipped' - The number of instances in the deployment in a skipped state.
-- * 'succeeded' - The number of instances in the deployment to which revisions have been successfully deployed.
mkDeploymentOverview ::
  DeploymentOverview
mkDeploymentOverview =
  DeploymentOverview'
    { pending = Lude.Nothing,
      skipped = Lude.Nothing,
      inProgress = Lude.Nothing,
      succeeded = Lude.Nothing,
      ready = Lude.Nothing,
      failed = Lude.Nothing
    }

-- | The number of instances in the deployment in a pending state.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doPending :: Lens.Lens' DeploymentOverview (Lude.Maybe Lude.Integer)
doPending = Lens.lens (pending :: DeploymentOverview -> Lude.Maybe Lude.Integer) (\s a -> s {pending = a} :: DeploymentOverview)
{-# DEPRECATED doPending "Use generic-lens or generic-optics with 'pending' instead." #-}

-- | The number of instances in the deployment in a skipped state.
--
-- /Note:/ Consider using 'skipped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doSkipped :: Lens.Lens' DeploymentOverview (Lude.Maybe Lude.Integer)
doSkipped = Lens.lens (skipped :: DeploymentOverview -> Lude.Maybe Lude.Integer) (\s a -> s {skipped = a} :: DeploymentOverview)
{-# DEPRECATED doSkipped "Use generic-lens or generic-optics with 'skipped' instead." #-}

-- | The number of instances in which the deployment is in progress.
--
-- /Note:/ Consider using 'inProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doInProgress :: Lens.Lens' DeploymentOverview (Lude.Maybe Lude.Integer)
doInProgress = Lens.lens (inProgress :: DeploymentOverview -> Lude.Maybe Lude.Integer) (\s a -> s {inProgress = a} :: DeploymentOverview)
{-# DEPRECATED doInProgress "Use generic-lens or generic-optics with 'inProgress' instead." #-}

-- | The number of instances in the deployment to which revisions have been successfully deployed.
--
-- /Note:/ Consider using 'succeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doSucceeded :: Lens.Lens' DeploymentOverview (Lude.Maybe Lude.Integer)
doSucceeded = Lens.lens (succeeded :: DeploymentOverview -> Lude.Maybe Lude.Integer) (\s a -> s {succeeded = a} :: DeploymentOverview)
{-# DEPRECATED doSucceeded "Use generic-lens or generic-optics with 'succeeded' instead." #-}

-- | The number of instances in a replacement environment ready to receive traffic in a blue/green deployment.
--
-- /Note:/ Consider using 'ready' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doReady :: Lens.Lens' DeploymentOverview (Lude.Maybe Lude.Integer)
doReady = Lens.lens (ready :: DeploymentOverview -> Lude.Maybe Lude.Integer) (\s a -> s {ready = a} :: DeploymentOverview)
{-# DEPRECATED doReady "Use generic-lens or generic-optics with 'ready' instead." #-}

-- | The number of instances in the deployment in a failed state.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doFailed :: Lens.Lens' DeploymentOverview (Lude.Maybe Lude.Integer)
doFailed = Lens.lens (failed :: DeploymentOverview -> Lude.Maybe Lude.Integer) (\s a -> s {failed = a} :: DeploymentOverview)
{-# DEPRECATED doFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

instance Lude.FromJSON DeploymentOverview where
  parseJSON =
    Lude.withObject
      "DeploymentOverview"
      ( \x ->
          DeploymentOverview'
            Lude.<$> (x Lude..:? "Pending")
            Lude.<*> (x Lude..:? "Skipped")
            Lude.<*> (x Lude..:? "InProgress")
            Lude.<*> (x Lude..:? "Succeeded")
            Lude.<*> (x Lude..:? "Ready")
            Lude.<*> (x Lude..:? "Failed")
      )
