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
-- Module      : Amazonka.AutoScaling.Types.TrafficSourceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.TrafficSourceState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of a traffic source.
--
-- /See:/ 'newTrafficSourceState' smart constructor.
data TrafficSourceState = TrafficSourceState'
  { -- | The following are the possible states for a VPC Lattice target group:
    --
    -- -   @Adding@ - The Auto Scaling instances are being registered with the
    --     target group.
    --
    -- -   @Added@ - All Auto Scaling instances are registered with the target
    --     group.
    --
    -- -   @InService@ - At least one Auto Scaling instance passed the
    --     @VPC_LATTICE@ health check.
    --
    -- -   @Removing@ - The Auto Scaling instances are being deregistered from
    --     the target group. If connection draining is enabled, VPC Lattice
    --     waits for in-flight requests to complete before deregistering the
    --     instances.
    --
    -- -   @Removed@ - All Auto Scaling instances are deregistered from the
    --     target group.
    state :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the traffic source. Currently, this is the
    -- Amazon Resource Name (ARN) for a VPC Lattice target group.
    trafficSource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficSourceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'trafficSourceState_state' - The following are the possible states for a VPC Lattice target group:
--
-- -   @Adding@ - The Auto Scaling instances are being registered with the
--     target group.
--
-- -   @Added@ - All Auto Scaling instances are registered with the target
--     group.
--
-- -   @InService@ - At least one Auto Scaling instance passed the
--     @VPC_LATTICE@ health check.
--
-- -   @Removing@ - The Auto Scaling instances are being deregistered from
--     the target group. If connection draining is enabled, VPC Lattice
--     waits for in-flight requests to complete before deregistering the
--     instances.
--
-- -   @Removed@ - All Auto Scaling instances are deregistered from the
--     target group.
--
-- 'trafficSource', 'trafficSourceState_trafficSource' - The unique identifier of the traffic source. Currently, this is the
-- Amazon Resource Name (ARN) for a VPC Lattice target group.
newTrafficSourceState ::
  TrafficSourceState
newTrafficSourceState =
  TrafficSourceState'
    { state = Prelude.Nothing,
      trafficSource = Prelude.Nothing
    }

-- | The following are the possible states for a VPC Lattice target group:
--
-- -   @Adding@ - The Auto Scaling instances are being registered with the
--     target group.
--
-- -   @Added@ - All Auto Scaling instances are registered with the target
--     group.
--
-- -   @InService@ - At least one Auto Scaling instance passed the
--     @VPC_LATTICE@ health check.
--
-- -   @Removing@ - The Auto Scaling instances are being deregistered from
--     the target group. If connection draining is enabled, VPC Lattice
--     waits for in-flight requests to complete before deregistering the
--     instances.
--
-- -   @Removed@ - All Auto Scaling instances are deregistered from the
--     target group.
trafficSourceState_state :: Lens.Lens' TrafficSourceState (Prelude.Maybe Prelude.Text)
trafficSourceState_state = Lens.lens (\TrafficSourceState' {state} -> state) (\s@TrafficSourceState' {} a -> s {state = a} :: TrafficSourceState)

-- | The unique identifier of the traffic source. Currently, this is the
-- Amazon Resource Name (ARN) for a VPC Lattice target group.
trafficSourceState_trafficSource :: Lens.Lens' TrafficSourceState (Prelude.Maybe Prelude.Text)
trafficSourceState_trafficSource = Lens.lens (\TrafficSourceState' {trafficSource} -> trafficSource) (\s@TrafficSourceState' {} a -> s {trafficSource = a} :: TrafficSourceState)

instance Data.FromXML TrafficSourceState where
  parseXML x =
    TrafficSourceState'
      Prelude.<$> (x Data..@? "State")
      Prelude.<*> (x Data..@? "TrafficSource")

instance Prelude.Hashable TrafficSourceState where
  hashWithSalt _salt TrafficSourceState' {..} =
    _salt
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` trafficSource

instance Prelude.NFData TrafficSourceState where
  rnf TrafficSourceState' {..} =
    Prelude.rnf state `Prelude.seq`
      Prelude.rnf trafficSource
