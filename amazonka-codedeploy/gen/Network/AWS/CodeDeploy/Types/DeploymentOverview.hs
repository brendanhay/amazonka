{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentOverview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentOverview where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the deployment status of the instances in the
-- deployment.
--
-- /See:/ 'newDeploymentOverview' smart constructor.
data DeploymentOverview = DeploymentOverview'
  { -- | The number of instances in the deployment to which revisions have been
    -- successfully deployed.
    succeeded :: Core.Maybe Core.Integer,
    -- | The number of instances in a replacement environment ready to receive
    -- traffic in a blue\/green deployment.
    ready :: Core.Maybe Core.Integer,
    -- | The number of instances in the deployment in a pending state.
    pending :: Core.Maybe Core.Integer,
    -- | The number of instances in the deployment in a failed state.
    failed :: Core.Maybe Core.Integer,
    -- | The number of instances in the deployment in a skipped state.
    skipped :: Core.Maybe Core.Integer,
    -- | The number of instances in which the deployment is in progress.
    inProgress :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeploymentOverview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'succeeded', 'deploymentOverview_succeeded' - The number of instances in the deployment to which revisions have been
-- successfully deployed.
--
-- 'ready', 'deploymentOverview_ready' - The number of instances in a replacement environment ready to receive
-- traffic in a blue\/green deployment.
--
-- 'pending', 'deploymentOverview_pending' - The number of instances in the deployment in a pending state.
--
-- 'failed', 'deploymentOverview_failed' - The number of instances in the deployment in a failed state.
--
-- 'skipped', 'deploymentOverview_skipped' - The number of instances in the deployment in a skipped state.
--
-- 'inProgress', 'deploymentOverview_inProgress' - The number of instances in which the deployment is in progress.
newDeploymentOverview ::
  DeploymentOverview
newDeploymentOverview =
  DeploymentOverview'
    { succeeded = Core.Nothing,
      ready = Core.Nothing,
      pending = Core.Nothing,
      failed = Core.Nothing,
      skipped = Core.Nothing,
      inProgress = Core.Nothing
    }

-- | The number of instances in the deployment to which revisions have been
-- successfully deployed.
deploymentOverview_succeeded :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
deploymentOverview_succeeded = Lens.lens (\DeploymentOverview' {succeeded} -> succeeded) (\s@DeploymentOverview' {} a -> s {succeeded = a} :: DeploymentOverview)

-- | The number of instances in a replacement environment ready to receive
-- traffic in a blue\/green deployment.
deploymentOverview_ready :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
deploymentOverview_ready = Lens.lens (\DeploymentOverview' {ready} -> ready) (\s@DeploymentOverview' {} a -> s {ready = a} :: DeploymentOverview)

-- | The number of instances in the deployment in a pending state.
deploymentOverview_pending :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
deploymentOverview_pending = Lens.lens (\DeploymentOverview' {pending} -> pending) (\s@DeploymentOverview' {} a -> s {pending = a} :: DeploymentOverview)

-- | The number of instances in the deployment in a failed state.
deploymentOverview_failed :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
deploymentOverview_failed = Lens.lens (\DeploymentOverview' {failed} -> failed) (\s@DeploymentOverview' {} a -> s {failed = a} :: DeploymentOverview)

-- | The number of instances in the deployment in a skipped state.
deploymentOverview_skipped :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
deploymentOverview_skipped = Lens.lens (\DeploymentOverview' {skipped} -> skipped) (\s@DeploymentOverview' {} a -> s {skipped = a} :: DeploymentOverview)

-- | The number of instances in which the deployment is in progress.
deploymentOverview_inProgress :: Lens.Lens' DeploymentOverview (Core.Maybe Core.Integer)
deploymentOverview_inProgress = Lens.lens (\DeploymentOverview' {inProgress} -> inProgress) (\s@DeploymentOverview' {} a -> s {inProgress = a} :: DeploymentOverview)

instance Core.FromJSON DeploymentOverview where
  parseJSON =
    Core.withObject
      "DeploymentOverview"
      ( \x ->
          DeploymentOverview'
            Core.<$> (x Core..:? "Succeeded")
            Core.<*> (x Core..:? "Ready")
            Core.<*> (x Core..:? "Pending")
            Core.<*> (x Core..:? "Failed")
            Core.<*> (x Core..:? "Skipped")
            Core.<*> (x Core..:? "InProgress")
      )

instance Core.Hashable DeploymentOverview

instance Core.NFData DeploymentOverview
