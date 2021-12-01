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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotCapacityRebalance where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ReplacementStrategy
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

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
    replacementStrategy :: Prelude.Maybe ReplacementStrategy
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
        Prelude.Nothing
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
spotCapacityRebalance_replacementStrategy :: Lens.Lens' SpotCapacityRebalance (Prelude.Maybe ReplacementStrategy)
spotCapacityRebalance_replacementStrategy = Lens.lens (\SpotCapacityRebalance' {replacementStrategy} -> replacementStrategy) (\s@SpotCapacityRebalance' {} a -> s {replacementStrategy = a} :: SpotCapacityRebalance)

instance Core.FromXML SpotCapacityRebalance where
  parseXML x =
    SpotCapacityRebalance'
      Prelude.<$> (x Core..@? "replacementStrategy")

instance Prelude.Hashable SpotCapacityRebalance where
  hashWithSalt salt' SpotCapacityRebalance' {..} =
    salt' `Prelude.hashWithSalt` replacementStrategy

instance Prelude.NFData SpotCapacityRebalance where
  rnf SpotCapacityRebalance' {..} =
    Prelude.rnf replacementStrategy

instance Core.ToQuery SpotCapacityRebalance where
  toQuery SpotCapacityRebalance' {..} =
    Prelude.mconcat
      ["ReplacementStrategy" Core.=: replacementStrategy]
