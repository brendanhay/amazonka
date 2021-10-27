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
-- Module      : Network.AWS.Route53RecoveryControlConfig.Types.ControlPanel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53RecoveryControlConfig.Types.ControlPanel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53RecoveryControlConfig.Types.Status

-- | A control panel represents a group of routing controls that can be
-- changed together in a single transaction.
--
-- /See:/ 'newControlPanel' smart constructor.
data ControlPanel = ControlPanel'
  { -- | The deployment status of control panel. Status can be one of the
    -- following: PENDING, DEPLOYED, PENDING_DELETION.
    status :: Prelude.Maybe Status,
    -- | The Amazon Resource Name (ARN) of the control panel.
    controlPanelArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster that includes the control
    -- panel.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The number of routing controls in the control panel.
    routingControlCount :: Prelude.Maybe Prelude.Int,
    -- | The name of the control panel. You can use any non-white space character
    -- in the name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A flag that Amazon Route 53 Application Recovery Controller sets to true
    -- to designate the default control panel for a cluster. When you create a
    -- cluster, Amazon Route 53 Application Recovery Controller creates a
    -- control panel, and sets this flag for that control panel. If you create
    -- a control panel yourself, this flag is set to false.
    defaultControlPanel :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlPanel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'controlPanel_status' - The deployment status of control panel. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
--
-- 'controlPanelArn', 'controlPanel_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel.
--
-- 'clusterArn', 'controlPanel_clusterArn' - The Amazon Resource Name (ARN) of the cluster that includes the control
-- panel.
--
-- 'routingControlCount', 'controlPanel_routingControlCount' - The number of routing controls in the control panel.
--
-- 'name', 'controlPanel_name' - The name of the control panel. You can use any non-white space character
-- in the name.
--
-- 'defaultControlPanel', 'controlPanel_defaultControlPanel' - A flag that Amazon Route 53 Application Recovery Controller sets to true
-- to designate the default control panel for a cluster. When you create a
-- cluster, Amazon Route 53 Application Recovery Controller creates a
-- control panel, and sets this flag for that control panel. If you create
-- a control panel yourself, this flag is set to false.
newControlPanel ::
  ControlPanel
newControlPanel =
  ControlPanel'
    { status = Prelude.Nothing,
      controlPanelArn = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      routingControlCount = Prelude.Nothing,
      name = Prelude.Nothing,
      defaultControlPanel = Prelude.Nothing
    }

-- | The deployment status of control panel. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
controlPanel_status :: Lens.Lens' ControlPanel (Prelude.Maybe Status)
controlPanel_status = Lens.lens (\ControlPanel' {status} -> status) (\s@ControlPanel' {} a -> s {status = a} :: ControlPanel)

-- | The Amazon Resource Name (ARN) of the control panel.
controlPanel_controlPanelArn :: Lens.Lens' ControlPanel (Prelude.Maybe Prelude.Text)
controlPanel_controlPanelArn = Lens.lens (\ControlPanel' {controlPanelArn} -> controlPanelArn) (\s@ControlPanel' {} a -> s {controlPanelArn = a} :: ControlPanel)

-- | The Amazon Resource Name (ARN) of the cluster that includes the control
-- panel.
controlPanel_clusterArn :: Lens.Lens' ControlPanel (Prelude.Maybe Prelude.Text)
controlPanel_clusterArn = Lens.lens (\ControlPanel' {clusterArn} -> clusterArn) (\s@ControlPanel' {} a -> s {clusterArn = a} :: ControlPanel)

-- | The number of routing controls in the control panel.
controlPanel_routingControlCount :: Lens.Lens' ControlPanel (Prelude.Maybe Prelude.Int)
controlPanel_routingControlCount = Lens.lens (\ControlPanel' {routingControlCount} -> routingControlCount) (\s@ControlPanel' {} a -> s {routingControlCount = a} :: ControlPanel)

-- | The name of the control panel. You can use any non-white space character
-- in the name.
controlPanel_name :: Lens.Lens' ControlPanel (Prelude.Maybe Prelude.Text)
controlPanel_name = Lens.lens (\ControlPanel' {name} -> name) (\s@ControlPanel' {} a -> s {name = a} :: ControlPanel)

-- | A flag that Amazon Route 53 Application Recovery Controller sets to true
-- to designate the default control panel for a cluster. When you create a
-- cluster, Amazon Route 53 Application Recovery Controller creates a
-- control panel, and sets this flag for that control panel. If you create
-- a control panel yourself, this flag is set to false.
controlPanel_defaultControlPanel :: Lens.Lens' ControlPanel (Prelude.Maybe Prelude.Bool)
controlPanel_defaultControlPanel = Lens.lens (\ControlPanel' {defaultControlPanel} -> defaultControlPanel) (\s@ControlPanel' {} a -> s {defaultControlPanel = a} :: ControlPanel)

instance Core.FromJSON ControlPanel where
  parseJSON =
    Core.withObject
      "ControlPanel"
      ( \x ->
          ControlPanel'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ControlPanelArn")
            Prelude.<*> (x Core..:? "ClusterArn")
            Prelude.<*> (x Core..:? "RoutingControlCount")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DefaultControlPanel")
      )

instance Prelude.Hashable ControlPanel

instance Prelude.NFData ControlPanel
