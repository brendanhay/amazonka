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
-- Module      : Network.AWS.GameLift.Types.EC2InstanceCounts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.EC2InstanceCounts where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Current status of fleet capacity. The number of active instances should
-- match or be in the process of matching the number of desired instances.
-- Pending and terminating counts are non-zero only if fleet capacity is
-- adjusting to an UpdateFleetCapacity request, or if access to resources
-- is temporarily affected.
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   DescribeFleetAttributes
--
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
--
-- /See:/ 'newEC2InstanceCounts' smart constructor.
data EC2InstanceCounts = EC2InstanceCounts'
  { -- | Number of active instances in the fleet that are not currently hosting a
    -- game session.
    idle :: Prelude.Maybe Prelude.Natural,
    -- | The minimum value allowed for the fleet\'s instance count.
    minimum :: Prelude.Maybe Prelude.Natural,
    -- | Number of instances in the fleet that are starting but not yet active.
    pending :: Prelude.Maybe Prelude.Natural,
    -- | Actual number of active instances in the fleet.
    active :: Prelude.Maybe Prelude.Natural,
    -- | Number of instances in the fleet that are no longer active but haven\'t
    -- yet been terminated.
    terminating :: Prelude.Maybe Prelude.Natural,
    -- | The maximum value allowed for the fleet\'s instance count.
    maximum :: Prelude.Maybe Prelude.Natural,
    -- | Ideal number of active instances in the fleet.
    desired :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2InstanceCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idle', 'eC2InstanceCounts_idle' - Number of active instances in the fleet that are not currently hosting a
-- game session.
--
-- 'minimum', 'eC2InstanceCounts_minimum' - The minimum value allowed for the fleet\'s instance count.
--
-- 'pending', 'eC2InstanceCounts_pending' - Number of instances in the fleet that are starting but not yet active.
--
-- 'active', 'eC2InstanceCounts_active' - Actual number of active instances in the fleet.
--
-- 'terminating', 'eC2InstanceCounts_terminating' - Number of instances in the fleet that are no longer active but haven\'t
-- yet been terminated.
--
-- 'maximum', 'eC2InstanceCounts_maximum' - The maximum value allowed for the fleet\'s instance count.
--
-- 'desired', 'eC2InstanceCounts_desired' - Ideal number of active instances in the fleet.
newEC2InstanceCounts ::
  EC2InstanceCounts
newEC2InstanceCounts =
  EC2InstanceCounts'
    { idle = Prelude.Nothing,
      minimum = Prelude.Nothing,
      pending = Prelude.Nothing,
      active = Prelude.Nothing,
      terminating = Prelude.Nothing,
      maximum = Prelude.Nothing,
      desired = Prelude.Nothing
    }

-- | Number of active instances in the fleet that are not currently hosting a
-- game session.
eC2InstanceCounts_idle :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_idle = Lens.lens (\EC2InstanceCounts' {idle} -> idle) (\s@EC2InstanceCounts' {} a -> s {idle = a} :: EC2InstanceCounts)

-- | The minimum value allowed for the fleet\'s instance count.
eC2InstanceCounts_minimum :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_minimum = Lens.lens (\EC2InstanceCounts' {minimum} -> minimum) (\s@EC2InstanceCounts' {} a -> s {minimum = a} :: EC2InstanceCounts)

-- | Number of instances in the fleet that are starting but not yet active.
eC2InstanceCounts_pending :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_pending = Lens.lens (\EC2InstanceCounts' {pending} -> pending) (\s@EC2InstanceCounts' {} a -> s {pending = a} :: EC2InstanceCounts)

-- | Actual number of active instances in the fleet.
eC2InstanceCounts_active :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_active = Lens.lens (\EC2InstanceCounts' {active} -> active) (\s@EC2InstanceCounts' {} a -> s {active = a} :: EC2InstanceCounts)

-- | Number of instances in the fleet that are no longer active but haven\'t
-- yet been terminated.
eC2InstanceCounts_terminating :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_terminating = Lens.lens (\EC2InstanceCounts' {terminating} -> terminating) (\s@EC2InstanceCounts' {} a -> s {terminating = a} :: EC2InstanceCounts)

-- | The maximum value allowed for the fleet\'s instance count.
eC2InstanceCounts_maximum :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_maximum = Lens.lens (\EC2InstanceCounts' {maximum} -> maximum) (\s@EC2InstanceCounts' {} a -> s {maximum = a} :: EC2InstanceCounts)

-- | Ideal number of active instances in the fleet.
eC2InstanceCounts_desired :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_desired = Lens.lens (\EC2InstanceCounts' {desired} -> desired) (\s@EC2InstanceCounts' {} a -> s {desired = a} :: EC2InstanceCounts)

instance Core.FromJSON EC2InstanceCounts where
  parseJSON =
    Core.withObject
      "EC2InstanceCounts"
      ( \x ->
          EC2InstanceCounts'
            Prelude.<$> (x Core..:? "IDLE")
            Prelude.<*> (x Core..:? "MINIMUM")
            Prelude.<*> (x Core..:? "PENDING")
            Prelude.<*> (x Core..:? "ACTIVE")
            Prelude.<*> (x Core..:? "TERMINATING")
            Prelude.<*> (x Core..:? "MAXIMUM")
            Prelude.<*> (x Core..:? "DESIRED")
      )

instance Prelude.Hashable EC2InstanceCounts

instance Prelude.NFData EC2InstanceCounts
