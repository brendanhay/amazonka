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
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.RoutingControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.RoutingControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryControlConfig.Types.Status

-- | A routing control has one of two states: ON and OFF. You can map the
-- routing control state to the state of an Amazon Route 53 health check,
-- which can be used to control traffic routing.
--
-- /See:/ 'newRoutingControl' smart constructor.
data RoutingControl = RoutingControl'
  { -- | The name of the routing control.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the control panel that includes the
    -- routing control.
    controlPanelArn :: Prelude.Maybe Prelude.Text,
    -- | The deployment status of a routing control. Status can be one of the
    -- following: PENDING, DEPLOYED, PENDING_DELETION.
    status :: Prelude.Maybe Status,
    -- | The Amazon Resource Name (ARN) of the routing control.
    routingControlArn :: Prelude.Maybe Prelude.Text
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
-- 'name', 'routingControl_name' - The name of the routing control.
--
-- 'controlPanelArn', 'routingControl_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel that includes the
-- routing control.
--
-- 'status', 'routingControl_status' - The deployment status of a routing control. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
--
-- 'routingControlArn', 'routingControl_routingControlArn' - The Amazon Resource Name (ARN) of the routing control.
newRoutingControl ::
  RoutingControl
newRoutingControl =
  RoutingControl'
    { name = Prelude.Nothing,
      controlPanelArn = Prelude.Nothing,
      status = Prelude.Nothing,
      routingControlArn = Prelude.Nothing
    }

-- | The name of the routing control.
routingControl_name :: Lens.Lens' RoutingControl (Prelude.Maybe Prelude.Text)
routingControl_name = Lens.lens (\RoutingControl' {name} -> name) (\s@RoutingControl' {} a -> s {name = a} :: RoutingControl)

-- | The Amazon Resource Name (ARN) of the control panel that includes the
-- routing control.
routingControl_controlPanelArn :: Lens.Lens' RoutingControl (Prelude.Maybe Prelude.Text)
routingControl_controlPanelArn = Lens.lens (\RoutingControl' {controlPanelArn} -> controlPanelArn) (\s@RoutingControl' {} a -> s {controlPanelArn = a} :: RoutingControl)

-- | The deployment status of a routing control. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
routingControl_status :: Lens.Lens' RoutingControl (Prelude.Maybe Status)
routingControl_status = Lens.lens (\RoutingControl' {status} -> status) (\s@RoutingControl' {} a -> s {status = a} :: RoutingControl)

-- | The Amazon Resource Name (ARN) of the routing control.
routingControl_routingControlArn :: Lens.Lens' RoutingControl (Prelude.Maybe Prelude.Text)
routingControl_routingControlArn = Lens.lens (\RoutingControl' {routingControlArn} -> routingControlArn) (\s@RoutingControl' {} a -> s {routingControlArn = a} :: RoutingControl)

instance Core.FromJSON RoutingControl where
  parseJSON =
    Core.withObject
      "RoutingControl"
      ( \x ->
          RoutingControl'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ControlPanelArn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "RoutingControlArn")
      )

instance Prelude.Hashable RoutingControl where
  hashWithSalt _salt RoutingControl' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` controlPanelArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` routingControlArn

instance Prelude.NFData RoutingControl where
  rnf RoutingControl' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf controlPanelArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf routingControlArn
