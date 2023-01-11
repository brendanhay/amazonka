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
-- Module      : Amazonka.EC2.Types.SpotCapacityRebalance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotCapacityRebalance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ReplacementStrategy
import qualified Amazonka.Prelude as Prelude

-- | The Spot Instance replacement strategy to use when Amazon EC2 emits a
-- signal that your Spot Instance is at an elevated risk of being
-- interrupted. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-capacity-rebalance.html Capacity rebalancing>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- /See:/ 'newSpotCapacityRebalance' smart constructor.
data SpotCapacityRebalance = SpotCapacityRebalance'
  { -- | The replacement strategy to use. Only available for fleets of type
    -- @maintain@.
    --
    -- @launch@ - Spot Fleet launches a new replacement Spot Instance when a
    -- rebalance notification is emitted for an existing Spot Instance in the
    -- fleet. Spot Fleet does not terminate the instances that receive a
    -- rebalance notification. You can terminate the old instances, or you can
    -- leave them running. You are charged for all instances while they are
    -- running.
    --
    -- @launch-before-terminate@ - Spot Fleet launches a new replacement Spot
    -- Instance when a rebalance notification is emitted for an existing Spot
    -- Instance in the fleet, and then, after a delay that you specify (in
    -- @TerminationDelay@), terminates the instances that received a rebalance
    -- notification.
    replacementStrategy :: Prelude.Maybe ReplacementStrategy,
    -- | The amount of time (in seconds) that Amazon EC2 waits before terminating
    -- the old Spot Instance after launching a new replacement Spot Instance.
    --
    -- Required when @ReplacementStrategy@ is set to @launch-before-terminate@.
    --
    -- Not valid when @ReplacementStrategy@ is set to @launch@.
    --
    -- Valid values: Minimum value of @120@ seconds. Maximum value of @7200@
    -- seconds.
    terminationDelay :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotCapacityRebalance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replacementStrategy', 'spotCapacityRebalance_replacementStrategy' - The replacement strategy to use. Only available for fleets of type
-- @maintain@.
--
-- @launch@ - Spot Fleet launches a new replacement Spot Instance when a
-- rebalance notification is emitted for an existing Spot Instance in the
-- fleet. Spot Fleet does not terminate the instances that receive a
-- rebalance notification. You can terminate the old instances, or you can
-- leave them running. You are charged for all instances while they are
-- running.
--
-- @launch-before-terminate@ - Spot Fleet launches a new replacement Spot
-- Instance when a rebalance notification is emitted for an existing Spot
-- Instance in the fleet, and then, after a delay that you specify (in
-- @TerminationDelay@), terminates the instances that received a rebalance
-- notification.
--
-- 'terminationDelay', 'spotCapacityRebalance_terminationDelay' - The amount of time (in seconds) that Amazon EC2 waits before terminating
-- the old Spot Instance after launching a new replacement Spot Instance.
--
-- Required when @ReplacementStrategy@ is set to @launch-before-terminate@.
--
-- Not valid when @ReplacementStrategy@ is set to @launch@.
--
-- Valid values: Minimum value of @120@ seconds. Maximum value of @7200@
-- seconds.
newSpotCapacityRebalance ::
  SpotCapacityRebalance
newSpotCapacityRebalance =
  SpotCapacityRebalance'
    { replacementStrategy =
        Prelude.Nothing,
      terminationDelay = Prelude.Nothing
    }

-- | The replacement strategy to use. Only available for fleets of type
-- @maintain@.
--
-- @launch@ - Spot Fleet launches a new replacement Spot Instance when a
-- rebalance notification is emitted for an existing Spot Instance in the
-- fleet. Spot Fleet does not terminate the instances that receive a
-- rebalance notification. You can terminate the old instances, or you can
-- leave them running. You are charged for all instances while they are
-- running.
--
-- @launch-before-terminate@ - Spot Fleet launches a new replacement Spot
-- Instance when a rebalance notification is emitted for an existing Spot
-- Instance in the fleet, and then, after a delay that you specify (in
-- @TerminationDelay@), terminates the instances that received a rebalance
-- notification.
spotCapacityRebalance_replacementStrategy :: Lens.Lens' SpotCapacityRebalance (Prelude.Maybe ReplacementStrategy)
spotCapacityRebalance_replacementStrategy = Lens.lens (\SpotCapacityRebalance' {replacementStrategy} -> replacementStrategy) (\s@SpotCapacityRebalance' {} a -> s {replacementStrategy = a} :: SpotCapacityRebalance)

-- | The amount of time (in seconds) that Amazon EC2 waits before terminating
-- the old Spot Instance after launching a new replacement Spot Instance.
--
-- Required when @ReplacementStrategy@ is set to @launch-before-terminate@.
--
-- Not valid when @ReplacementStrategy@ is set to @launch@.
--
-- Valid values: Minimum value of @120@ seconds. Maximum value of @7200@
-- seconds.
spotCapacityRebalance_terminationDelay :: Lens.Lens' SpotCapacityRebalance (Prelude.Maybe Prelude.Int)
spotCapacityRebalance_terminationDelay = Lens.lens (\SpotCapacityRebalance' {terminationDelay} -> terminationDelay) (\s@SpotCapacityRebalance' {} a -> s {terminationDelay = a} :: SpotCapacityRebalance)

instance Data.FromXML SpotCapacityRebalance where
  parseXML x =
    SpotCapacityRebalance'
      Prelude.<$> (x Data..@? "replacementStrategy")
      Prelude.<*> (x Data..@? "terminationDelay")

instance Prelude.Hashable SpotCapacityRebalance where
  hashWithSalt _salt SpotCapacityRebalance' {..} =
    _salt `Prelude.hashWithSalt` replacementStrategy
      `Prelude.hashWithSalt` terminationDelay

instance Prelude.NFData SpotCapacityRebalance where
  rnf SpotCapacityRebalance' {..} =
    Prelude.rnf replacementStrategy
      `Prelude.seq` Prelude.rnf terminationDelay

instance Data.ToQuery SpotCapacityRebalance where
  toQuery SpotCapacityRebalance' {..} =
    Prelude.mconcat
      [ "ReplacementStrategy" Data.=: replacementStrategy,
        "TerminationDelay" Data.=: terminationDelay
      ]
