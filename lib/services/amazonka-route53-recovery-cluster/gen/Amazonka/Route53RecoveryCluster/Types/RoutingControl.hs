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
-- Module      : Amazonka.Route53RecoveryCluster.Types.RoutingControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryCluster.Types.RoutingControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryCluster.Types.RoutingControlState

-- | A routing control, which is a simple on\/off switch that you can use to
-- route traffic to cells. When a routing control state is On, traffic
-- flows to a cell. When the state is Off, traffic does not flow.
--
-- /See:/ 'newRoutingControl' smart constructor.
data RoutingControl = RoutingControl'
  { -- | The Amazon Resource Name (ARN) of the control panel where the routing
    -- control is located.
    controlPanelArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the control panel where the routing control is located.
    controlPanelName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the routing control.
    routingControlArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the routing control.
    routingControlName :: Prelude.Maybe Prelude.Text,
    -- | The current state of the routing control. When a routing control state
    -- is On, traffic flows to a cell. When the state is Off, traffic does not
    -- flow.
    routingControlState :: Prelude.Maybe RoutingControlState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanelArn', 'routingControl_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel where the routing
-- control is located.
--
-- 'controlPanelName', 'routingControl_controlPanelName' - The name of the control panel where the routing control is located.
--
-- 'routingControlArn', 'routingControl_routingControlArn' - The Amazon Resource Name (ARN) of the routing control.
--
-- 'routingControlName', 'routingControl_routingControlName' - The name of the routing control.
--
-- 'routingControlState', 'routingControl_routingControlState' - The current state of the routing control. When a routing control state
-- is On, traffic flows to a cell. When the state is Off, traffic does not
-- flow.
newRoutingControl ::
  RoutingControl
newRoutingControl =
  RoutingControl'
    { controlPanelArn = Prelude.Nothing,
      controlPanelName = Prelude.Nothing,
      routingControlArn = Prelude.Nothing,
      routingControlName = Prelude.Nothing,
      routingControlState = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the control panel where the routing
-- control is located.
routingControl_controlPanelArn :: Lens.Lens' RoutingControl (Prelude.Maybe Prelude.Text)
routingControl_controlPanelArn = Lens.lens (\RoutingControl' {controlPanelArn} -> controlPanelArn) (\s@RoutingControl' {} a -> s {controlPanelArn = a} :: RoutingControl)

-- | The name of the control panel where the routing control is located.
routingControl_controlPanelName :: Lens.Lens' RoutingControl (Prelude.Maybe Prelude.Text)
routingControl_controlPanelName = Lens.lens (\RoutingControl' {controlPanelName} -> controlPanelName) (\s@RoutingControl' {} a -> s {controlPanelName = a} :: RoutingControl)

-- | The Amazon Resource Name (ARN) of the routing control.
routingControl_routingControlArn :: Lens.Lens' RoutingControl (Prelude.Maybe Prelude.Text)
routingControl_routingControlArn = Lens.lens (\RoutingControl' {routingControlArn} -> routingControlArn) (\s@RoutingControl' {} a -> s {routingControlArn = a} :: RoutingControl)

-- | The name of the routing control.
routingControl_routingControlName :: Lens.Lens' RoutingControl (Prelude.Maybe Prelude.Text)
routingControl_routingControlName = Lens.lens (\RoutingControl' {routingControlName} -> routingControlName) (\s@RoutingControl' {} a -> s {routingControlName = a} :: RoutingControl)

-- | The current state of the routing control. When a routing control state
-- is On, traffic flows to a cell. When the state is Off, traffic does not
-- flow.
routingControl_routingControlState :: Lens.Lens' RoutingControl (Prelude.Maybe RoutingControlState)
routingControl_routingControlState = Lens.lens (\RoutingControl' {routingControlState} -> routingControlState) (\s@RoutingControl' {} a -> s {routingControlState = a} :: RoutingControl)

instance Data.FromJSON RoutingControl where
  parseJSON =
    Data.withObject
      "RoutingControl"
      ( \x ->
          RoutingControl'
            Prelude.<$> (x Data..:? "ControlPanelArn")
            Prelude.<*> (x Data..:? "ControlPanelName")
            Prelude.<*> (x Data..:? "RoutingControlArn")
            Prelude.<*> (x Data..:? "RoutingControlName")
            Prelude.<*> (x Data..:? "RoutingControlState")
      )

instance Prelude.Hashable RoutingControl where
  hashWithSalt _salt RoutingControl' {..} =
    _salt
      `Prelude.hashWithSalt` controlPanelArn
      `Prelude.hashWithSalt` controlPanelName
      `Prelude.hashWithSalt` routingControlArn
      `Prelude.hashWithSalt` routingControlName
      `Prelude.hashWithSalt` routingControlState

instance Prelude.NFData RoutingControl where
  rnf RoutingControl' {..} =
    Prelude.rnf controlPanelArn
      `Prelude.seq` Prelude.rnf controlPanelName
      `Prelude.seq` Prelude.rnf routingControlArn
      `Prelude.seq` Prelude.rnf routingControlName
      `Prelude.seq` Prelude.rnf routingControlState
