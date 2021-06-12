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
-- Module      : Network.AWS.EC2.Types.SpotCapacityRebalance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotCapacityRebalance where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ReplacementStrategy
import qualified Network.AWS.Lens as Lens

-- | The Spot Instance replacement strategy to use when Amazon EC2 emits a
-- signal that your Spot Instance is at an elevated risk of being
-- interrupted. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#spot-fleet-capacity-rebalance Capacity rebalancing>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- /See:/ 'newSpotCapacityRebalance' smart constructor.
data SpotCapacityRebalance = SpotCapacityRebalance'
  { -- | The replacement strategy to use. Only available for fleets of type
    -- @maintain@. You must specify a value, otherwise you get an error.
    --
    -- To allow Spot Fleet to launch a replacement Spot Instance when an
    -- instance rebalance notification is emitted for a Spot Instance in the
    -- fleet, specify @launch@.
    --
    -- When a replacement instance is launched, the instance marked for
    -- rebalance is not automatically terminated. You can terminate it, or you
    -- can leave it running. You are charged for all instances while they are
    -- running.
    replacementStrategy :: Core.Maybe ReplacementStrategy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SpotCapacityRebalance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replacementStrategy', 'spotCapacityRebalance_replacementStrategy' - The replacement strategy to use. Only available for fleets of type
-- @maintain@. You must specify a value, otherwise you get an error.
--
-- To allow Spot Fleet to launch a replacement Spot Instance when an
-- instance rebalance notification is emitted for a Spot Instance in the
-- fleet, specify @launch@.
--
-- When a replacement instance is launched, the instance marked for
-- rebalance is not automatically terminated. You can terminate it, or you
-- can leave it running. You are charged for all instances while they are
-- running.
newSpotCapacityRebalance ::
  SpotCapacityRebalance
newSpotCapacityRebalance =
  SpotCapacityRebalance'
    { replacementStrategy =
        Core.Nothing
    }

-- | The replacement strategy to use. Only available for fleets of type
-- @maintain@. You must specify a value, otherwise you get an error.
--
-- To allow Spot Fleet to launch a replacement Spot Instance when an
-- instance rebalance notification is emitted for a Spot Instance in the
-- fleet, specify @launch@.
--
-- When a replacement instance is launched, the instance marked for
-- rebalance is not automatically terminated. You can terminate it, or you
-- can leave it running. You are charged for all instances while they are
-- running.
spotCapacityRebalance_replacementStrategy :: Lens.Lens' SpotCapacityRebalance (Core.Maybe ReplacementStrategy)
spotCapacityRebalance_replacementStrategy = Lens.lens (\SpotCapacityRebalance' {replacementStrategy} -> replacementStrategy) (\s@SpotCapacityRebalance' {} a -> s {replacementStrategy = a} :: SpotCapacityRebalance)

instance Core.FromXML SpotCapacityRebalance where
  parseXML x =
    SpotCapacityRebalance'
      Core.<$> (x Core..@? "replacementStrategy")

instance Core.Hashable SpotCapacityRebalance

instance Core.NFData SpotCapacityRebalance

instance Core.ToQuery SpotCapacityRebalance where
  toQuery SpotCapacityRebalance' {..} =
    Core.mconcat
      ["ReplacementStrategy" Core.=: replacementStrategy]
