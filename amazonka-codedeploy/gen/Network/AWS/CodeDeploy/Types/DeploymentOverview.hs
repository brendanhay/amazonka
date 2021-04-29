{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the deployment status of the instances in the
-- deployment.
--
-- /See:/ 'newDeploymentOverview' smart constructor.
data DeploymentOverview = DeploymentOverview'
  { -- | The number of instances in the deployment to which revisions have been
    -- successfully deployed.
    succeeded :: Prelude.Maybe Prelude.Integer,
    -- | The number of instances in a replacement environment ready to receive
    -- traffic in a blue\/green deployment.
    ready :: Prelude.Maybe Prelude.Integer,
    -- | The number of instances in the deployment in a pending state.
    pending :: Prelude.Maybe Prelude.Integer,
    -- | The number of instances in the deployment in a failed state.
    failed :: Prelude.Maybe Prelude.Integer,
    -- | The number of instances in the deployment in a skipped state.
    skipped :: Prelude.Maybe Prelude.Integer,
    -- | The number of instances in which the deployment is in progress.
    inProgress :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { succeeded = Prelude.Nothing,
      ready = Prelude.Nothing,
      pending = Prelude.Nothing,
      failed = Prelude.Nothing,
      skipped = Prelude.Nothing,
      inProgress = Prelude.Nothing
    }

-- | The number of instances in the deployment to which revisions have been
-- successfully deployed.
deploymentOverview_succeeded :: Lens.Lens' DeploymentOverview (Prelude.Maybe Prelude.Integer)
deploymentOverview_succeeded = Lens.lens (\DeploymentOverview' {succeeded} -> succeeded) (\s@DeploymentOverview' {} a -> s {succeeded = a} :: DeploymentOverview)

-- | The number of instances in a replacement environment ready to receive
-- traffic in a blue\/green deployment.
deploymentOverview_ready :: Lens.Lens' DeploymentOverview (Prelude.Maybe Prelude.Integer)
deploymentOverview_ready = Lens.lens (\DeploymentOverview' {ready} -> ready) (\s@DeploymentOverview' {} a -> s {ready = a} :: DeploymentOverview)

-- | The number of instances in the deployment in a pending state.
deploymentOverview_pending :: Lens.Lens' DeploymentOverview (Prelude.Maybe Prelude.Integer)
deploymentOverview_pending = Lens.lens (\DeploymentOverview' {pending} -> pending) (\s@DeploymentOverview' {} a -> s {pending = a} :: DeploymentOverview)

-- | The number of instances in the deployment in a failed state.
deploymentOverview_failed :: Lens.Lens' DeploymentOverview (Prelude.Maybe Prelude.Integer)
deploymentOverview_failed = Lens.lens (\DeploymentOverview' {failed} -> failed) (\s@DeploymentOverview' {} a -> s {failed = a} :: DeploymentOverview)

-- | The number of instances in the deployment in a skipped state.
deploymentOverview_skipped :: Lens.Lens' DeploymentOverview (Prelude.Maybe Prelude.Integer)
deploymentOverview_skipped = Lens.lens (\DeploymentOverview' {skipped} -> skipped) (\s@DeploymentOverview' {} a -> s {skipped = a} :: DeploymentOverview)

-- | The number of instances in which the deployment is in progress.
deploymentOverview_inProgress :: Lens.Lens' DeploymentOverview (Prelude.Maybe Prelude.Integer)
deploymentOverview_inProgress = Lens.lens (\DeploymentOverview' {inProgress} -> inProgress) (\s@DeploymentOverview' {} a -> s {inProgress = a} :: DeploymentOverview)

instance Prelude.FromJSON DeploymentOverview where
  parseJSON =
    Prelude.withObject
      "DeploymentOverview"
      ( \x ->
          DeploymentOverview'
            Prelude.<$> (x Prelude..:? "Succeeded")
            Prelude.<*> (x Prelude..:? "Ready")
            Prelude.<*> (x Prelude..:? "Pending")
            Prelude.<*> (x Prelude..:? "Failed")
            Prelude.<*> (x Prelude..:? "Skipped")
            Prelude.<*> (x Prelude..:? "InProgress")
      )

instance Prelude.Hashable DeploymentOverview

instance Prelude.NFData DeploymentOverview
