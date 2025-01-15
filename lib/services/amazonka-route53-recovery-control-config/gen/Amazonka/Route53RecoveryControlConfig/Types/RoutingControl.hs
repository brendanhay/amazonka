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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.RoutingControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryControlConfig.Types.Status

-- | A routing control has one of two states: ON and OFF. You can map the
-- routing control state to the state of an Amazon Route 53 health check,
-- which can be used to control traffic routing.
--
-- /See:/ 'newRoutingControl' smart constructor.
data RoutingControl = RoutingControl'
  { -- | The Amazon Resource Name (ARN) of the control panel that includes the
    -- routing control.
    controlPanelArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the routing control.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the routing control.
    routingControlArn :: Prelude.Maybe Prelude.Text,
    -- | The deployment status of a routing control. Status can be one of the
    -- following: PENDING, DEPLOYED, PENDING_DELETION.
    status :: Prelude.Maybe Status
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
-- 'controlPanelArn', 'routingControl_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel that includes the
-- routing control.
--
-- 'name', 'routingControl_name' - The name of the routing control.
--
-- 'routingControlArn', 'routingControl_routingControlArn' - The Amazon Resource Name (ARN) of the routing control.
--
-- 'status', 'routingControl_status' - The deployment status of a routing control. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
newRoutingControl ::
  RoutingControl
newRoutingControl =
  RoutingControl'
    { controlPanelArn = Prelude.Nothing,
      name = Prelude.Nothing,
      routingControlArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the control panel that includes the
-- routing control.
routingControl_controlPanelArn :: Lens.Lens' RoutingControl (Prelude.Maybe Prelude.Text)
routingControl_controlPanelArn = Lens.lens (\RoutingControl' {controlPanelArn} -> controlPanelArn) (\s@RoutingControl' {} a -> s {controlPanelArn = a} :: RoutingControl)

-- | The name of the routing control.
routingControl_name :: Lens.Lens' RoutingControl (Prelude.Maybe Prelude.Text)
routingControl_name = Lens.lens (\RoutingControl' {name} -> name) (\s@RoutingControl' {} a -> s {name = a} :: RoutingControl)

-- | The Amazon Resource Name (ARN) of the routing control.
routingControl_routingControlArn :: Lens.Lens' RoutingControl (Prelude.Maybe Prelude.Text)
routingControl_routingControlArn = Lens.lens (\RoutingControl' {routingControlArn} -> routingControlArn) (\s@RoutingControl' {} a -> s {routingControlArn = a} :: RoutingControl)

-- | The deployment status of a routing control. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
routingControl_status :: Lens.Lens' RoutingControl (Prelude.Maybe Status)
routingControl_status = Lens.lens (\RoutingControl' {status} -> status) (\s@RoutingControl' {} a -> s {status = a} :: RoutingControl)

instance Data.FromJSON RoutingControl where
  parseJSON =
    Data.withObject
      "RoutingControl"
      ( \x ->
          RoutingControl'
            Prelude.<$> (x Data..:? "ControlPanelArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RoutingControlArn")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable RoutingControl where
  hashWithSalt _salt RoutingControl' {..} =
    _salt
      `Prelude.hashWithSalt` controlPanelArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` routingControlArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData RoutingControl where
  rnf RoutingControl' {..} =
    Prelude.rnf controlPanelArn `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf routingControlArn `Prelude.seq`
          Prelude.rnf status
