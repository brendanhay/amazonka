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
-- Module      : Amazonka.EC2.Types.FleetSpotCapacityRebalance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetSpotCapacityRebalance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FleetReplacementStrategy
import qualified Amazonka.Prelude as Prelude

-- | The strategy to use when Amazon EC2 emits a signal that your Spot
-- Instance is at an elevated risk of being interrupted.
--
-- /See:/ 'newFleetSpotCapacityRebalance' smart constructor.
data FleetSpotCapacityRebalance = FleetSpotCapacityRebalance'
  { -- | The amount of time (in seconds) that Amazon EC2 waits before terminating
    -- the old Spot Instance after launching a new replacement Spot Instance.
    --
    -- Required when @ReplacementStrategy@ is set to @launch-before-terminate@.
    --
    -- Not valid when @ReplacementStrategy@ is set to @launch@.
    --
    -- Valid values: Minimum value of @120@ seconds. Maximum value of @7200@
    -- seconds.
    terminationDelay :: Prelude.Maybe Prelude.Int,
    -- | The replacement strategy to use. Only available for fleets of type
    -- @maintain@.
    --
    -- @launch@ - EC2 Fleet launches a new replacement Spot Instance when a
    -- rebalance notification is emitted for an existing Spot Instance in the
    -- fleet. EC2 Fleet does not terminate the instances that receive a
    -- rebalance notification. You can terminate the old instances, or you can
    -- leave them running. You are charged for all instances while they are
    -- running.
    --
    -- @launch-before-terminate@ - EC2 Fleet launches a new replacement Spot
    -- Instance when a rebalance notification is emitted for an existing Spot
    -- Instance in the fleet, and then, after a delay that you specify (in
    -- @TerminationDelay@), terminates the instances that received a rebalance
    -- notification.
    replacementStrategy :: Prelude.Maybe FleetReplacementStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetSpotCapacityRebalance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminationDelay', 'fleetSpotCapacityRebalance_terminationDelay' - The amount of time (in seconds) that Amazon EC2 waits before terminating
-- the old Spot Instance after launching a new replacement Spot Instance.
--
-- Required when @ReplacementStrategy@ is set to @launch-before-terminate@.
--
-- Not valid when @ReplacementStrategy@ is set to @launch@.
--
-- Valid values: Minimum value of @120@ seconds. Maximum value of @7200@
-- seconds.
--
-- 'replacementStrategy', 'fleetSpotCapacityRebalance_replacementStrategy' - The replacement strategy to use. Only available for fleets of type
-- @maintain@.
--
-- @launch@ - EC2 Fleet launches a new replacement Spot Instance when a
-- rebalance notification is emitted for an existing Spot Instance in the
-- fleet. EC2 Fleet does not terminate the instances that receive a
-- rebalance notification. You can terminate the old instances, or you can
-- leave them running. You are charged for all instances while they are
-- running.
--
-- @launch-before-terminate@ - EC2 Fleet launches a new replacement Spot
-- Instance when a rebalance notification is emitted for an existing Spot
-- Instance in the fleet, and then, after a delay that you specify (in
-- @TerminationDelay@), terminates the instances that received a rebalance
-- notification.
newFleetSpotCapacityRebalance ::
  FleetSpotCapacityRebalance
newFleetSpotCapacityRebalance =
  FleetSpotCapacityRebalance'
    { terminationDelay =
        Prelude.Nothing,
      replacementStrategy = Prelude.Nothing
    }

-- | The amount of time (in seconds) that Amazon EC2 waits before terminating
-- the old Spot Instance after launching a new replacement Spot Instance.
--
-- Required when @ReplacementStrategy@ is set to @launch-before-terminate@.
--
-- Not valid when @ReplacementStrategy@ is set to @launch@.
--
-- Valid values: Minimum value of @120@ seconds. Maximum value of @7200@
-- seconds.
fleetSpotCapacityRebalance_terminationDelay :: Lens.Lens' FleetSpotCapacityRebalance (Prelude.Maybe Prelude.Int)
fleetSpotCapacityRebalance_terminationDelay = Lens.lens (\FleetSpotCapacityRebalance' {terminationDelay} -> terminationDelay) (\s@FleetSpotCapacityRebalance' {} a -> s {terminationDelay = a} :: FleetSpotCapacityRebalance)

-- | The replacement strategy to use. Only available for fleets of type
-- @maintain@.
--
-- @launch@ - EC2 Fleet launches a new replacement Spot Instance when a
-- rebalance notification is emitted for an existing Spot Instance in the
-- fleet. EC2 Fleet does not terminate the instances that receive a
-- rebalance notification. You can terminate the old instances, or you can
-- leave them running. You are charged for all instances while they are
-- running.
--
-- @launch-before-terminate@ - EC2 Fleet launches a new replacement Spot
-- Instance when a rebalance notification is emitted for an existing Spot
-- Instance in the fleet, and then, after a delay that you specify (in
-- @TerminationDelay@), terminates the instances that received a rebalance
-- notification.
fleetSpotCapacityRebalance_replacementStrategy :: Lens.Lens' FleetSpotCapacityRebalance (Prelude.Maybe FleetReplacementStrategy)
fleetSpotCapacityRebalance_replacementStrategy = Lens.lens (\FleetSpotCapacityRebalance' {replacementStrategy} -> replacementStrategy) (\s@FleetSpotCapacityRebalance' {} a -> s {replacementStrategy = a} :: FleetSpotCapacityRebalance)

instance Data.FromXML FleetSpotCapacityRebalance where
  parseXML x =
    FleetSpotCapacityRebalance'
      Prelude.<$> (x Data..@? "terminationDelay")
      Prelude.<*> (x Data..@? "replacementStrategy")

instance Prelude.Hashable FleetSpotCapacityRebalance where
  hashWithSalt _salt FleetSpotCapacityRebalance' {..} =
    _salt `Prelude.hashWithSalt` terminationDelay
      `Prelude.hashWithSalt` replacementStrategy

instance Prelude.NFData FleetSpotCapacityRebalance where
  rnf FleetSpotCapacityRebalance' {..} =
    Prelude.rnf terminationDelay
      `Prelude.seq` Prelude.rnf replacementStrategy
