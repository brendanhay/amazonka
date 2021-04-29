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
-- Module      : Network.AWS.EC2.Types.FleetSpotCapacityRebalance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetSpotCapacityRebalance where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetReplacementStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The strategy to use when Amazon EC2 emits a signal that your Spot
-- Instance is at an elevated risk of being interrupted.
--
-- /See:/ 'newFleetSpotCapacityRebalance' smart constructor.
data FleetSpotCapacityRebalance = FleetSpotCapacityRebalance'
  { -- | To allow EC2 Fleet to launch a replacement Spot Instance when an
    -- instance rebalance notification is emitted for an existing Spot Instance
    -- in the fleet, specify @launch@. Only available for fleets of type
    -- @maintain@.
    --
    -- When a replacement instance is launched, the instance marked for
    -- rebalance is not automatically terminated. You can terminate it, or you
    -- can leave it running. You are charged for both instances while they are
    -- running.
    replacementStrategy :: Prelude.Maybe FleetReplacementStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FleetSpotCapacityRebalance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replacementStrategy', 'fleetSpotCapacityRebalance_replacementStrategy' - To allow EC2 Fleet to launch a replacement Spot Instance when an
-- instance rebalance notification is emitted for an existing Spot Instance
-- in the fleet, specify @launch@. Only available for fleets of type
-- @maintain@.
--
-- When a replacement instance is launched, the instance marked for
-- rebalance is not automatically terminated. You can terminate it, or you
-- can leave it running. You are charged for both instances while they are
-- running.
newFleetSpotCapacityRebalance ::
  FleetSpotCapacityRebalance
newFleetSpotCapacityRebalance =
  FleetSpotCapacityRebalance'
    { replacementStrategy =
        Prelude.Nothing
    }

-- | To allow EC2 Fleet to launch a replacement Spot Instance when an
-- instance rebalance notification is emitted for an existing Spot Instance
-- in the fleet, specify @launch@. Only available for fleets of type
-- @maintain@.
--
-- When a replacement instance is launched, the instance marked for
-- rebalance is not automatically terminated. You can terminate it, or you
-- can leave it running. You are charged for both instances while they are
-- running.
fleetSpotCapacityRebalance_replacementStrategy :: Lens.Lens' FleetSpotCapacityRebalance (Prelude.Maybe FleetReplacementStrategy)
fleetSpotCapacityRebalance_replacementStrategy = Lens.lens (\FleetSpotCapacityRebalance' {replacementStrategy} -> replacementStrategy) (\s@FleetSpotCapacityRebalance' {} a -> s {replacementStrategy = a} :: FleetSpotCapacityRebalance)

instance Prelude.FromXML FleetSpotCapacityRebalance where
  parseXML x =
    FleetSpotCapacityRebalance'
      Prelude.<$> (x Prelude..@? "replacementStrategy")

instance Prelude.Hashable FleetSpotCapacityRebalance

instance Prelude.NFData FleetSpotCapacityRebalance
