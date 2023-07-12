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
-- Module      : Amazonka.GameLift.Types.EC2InstanceCounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.EC2InstanceCounts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Resource capacity settings. Fleet capacity is measured in Amazon EC2
-- instances. Pending and terminating counts are non-zero when the fleet
-- capacity is adjusting to a scaling event or if access to resources is
-- temporarily affected.
--
-- /See:/ 'newEC2InstanceCounts' smart constructor.
data EC2InstanceCounts = EC2InstanceCounts'
  { -- | Actual number of instances that are ready to host game sessions.
    active :: Prelude.Maybe Prelude.Natural,
    -- | Ideal number of active instances. GameLift will always try to maintain
    -- the desired number of instances. Capacity is scaled up or down by
    -- changing the desired instances.
    desired :: Prelude.Maybe Prelude.Natural,
    -- | Number of active instances that are not currently hosting a game
    -- session.
    idle :: Prelude.Maybe Prelude.Natural,
    -- | The maximum instance count value allowed.
    maximum :: Prelude.Maybe Prelude.Natural,
    -- | The minimum instance count value allowed.
    minimum :: Prelude.Maybe Prelude.Natural,
    -- | Number of instances that are starting but not yet active.
    pending :: Prelude.Maybe Prelude.Natural,
    -- | Number of instances that are no longer active but haven\'t yet been
    -- terminated.
    terminating :: Prelude.Maybe Prelude.Natural
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
-- 'active', 'eC2InstanceCounts_active' - Actual number of instances that are ready to host game sessions.
--
-- 'desired', 'eC2InstanceCounts_desired' - Ideal number of active instances. GameLift will always try to maintain
-- the desired number of instances. Capacity is scaled up or down by
-- changing the desired instances.
--
-- 'idle', 'eC2InstanceCounts_idle' - Number of active instances that are not currently hosting a game
-- session.
--
-- 'maximum', 'eC2InstanceCounts_maximum' - The maximum instance count value allowed.
--
-- 'minimum', 'eC2InstanceCounts_minimum' - The minimum instance count value allowed.
--
-- 'pending', 'eC2InstanceCounts_pending' - Number of instances that are starting but not yet active.
--
-- 'terminating', 'eC2InstanceCounts_terminating' - Number of instances that are no longer active but haven\'t yet been
-- terminated.
newEC2InstanceCounts ::
  EC2InstanceCounts
newEC2InstanceCounts =
  EC2InstanceCounts'
    { active = Prelude.Nothing,
      desired = Prelude.Nothing,
      idle = Prelude.Nothing,
      maximum = Prelude.Nothing,
      minimum = Prelude.Nothing,
      pending = Prelude.Nothing,
      terminating = Prelude.Nothing
    }

-- | Actual number of instances that are ready to host game sessions.
eC2InstanceCounts_active :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_active = Lens.lens (\EC2InstanceCounts' {active} -> active) (\s@EC2InstanceCounts' {} a -> s {active = a} :: EC2InstanceCounts)

-- | Ideal number of active instances. GameLift will always try to maintain
-- the desired number of instances. Capacity is scaled up or down by
-- changing the desired instances.
eC2InstanceCounts_desired :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_desired = Lens.lens (\EC2InstanceCounts' {desired} -> desired) (\s@EC2InstanceCounts' {} a -> s {desired = a} :: EC2InstanceCounts)

-- | Number of active instances that are not currently hosting a game
-- session.
eC2InstanceCounts_idle :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_idle = Lens.lens (\EC2InstanceCounts' {idle} -> idle) (\s@EC2InstanceCounts' {} a -> s {idle = a} :: EC2InstanceCounts)

-- | The maximum instance count value allowed.
eC2InstanceCounts_maximum :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_maximum = Lens.lens (\EC2InstanceCounts' {maximum} -> maximum) (\s@EC2InstanceCounts' {} a -> s {maximum = a} :: EC2InstanceCounts)

-- | The minimum instance count value allowed.
eC2InstanceCounts_minimum :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_minimum = Lens.lens (\EC2InstanceCounts' {minimum} -> minimum) (\s@EC2InstanceCounts' {} a -> s {minimum = a} :: EC2InstanceCounts)

-- | Number of instances that are starting but not yet active.
eC2InstanceCounts_pending :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_pending = Lens.lens (\EC2InstanceCounts' {pending} -> pending) (\s@EC2InstanceCounts' {} a -> s {pending = a} :: EC2InstanceCounts)

-- | Number of instances that are no longer active but haven\'t yet been
-- terminated.
eC2InstanceCounts_terminating :: Lens.Lens' EC2InstanceCounts (Prelude.Maybe Prelude.Natural)
eC2InstanceCounts_terminating = Lens.lens (\EC2InstanceCounts' {terminating} -> terminating) (\s@EC2InstanceCounts' {} a -> s {terminating = a} :: EC2InstanceCounts)

instance Data.FromJSON EC2InstanceCounts where
  parseJSON =
    Data.withObject
      "EC2InstanceCounts"
      ( \x ->
          EC2InstanceCounts'
            Prelude.<$> (x Data..:? "ACTIVE")
            Prelude.<*> (x Data..:? "DESIRED")
            Prelude.<*> (x Data..:? "IDLE")
            Prelude.<*> (x Data..:? "MAXIMUM")
            Prelude.<*> (x Data..:? "MINIMUM")
            Prelude.<*> (x Data..:? "PENDING")
            Prelude.<*> (x Data..:? "TERMINATING")
      )

instance Prelude.Hashable EC2InstanceCounts where
  hashWithSalt _salt EC2InstanceCounts' {..} =
    _salt
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` desired
      `Prelude.hashWithSalt` idle
      `Prelude.hashWithSalt` maximum
      `Prelude.hashWithSalt` minimum
      `Prelude.hashWithSalt` pending
      `Prelude.hashWithSalt` terminating

instance Prelude.NFData EC2InstanceCounts where
  rnf EC2InstanceCounts' {..} =
    Prelude.rnf active
      `Prelude.seq` Prelude.rnf desired
      `Prelude.seq` Prelude.rnf idle
      `Prelude.seq` Prelude.rnf maximum
      `Prelude.seq` Prelude.rnf minimum
      `Prelude.seq` Prelude.rnf pending
      `Prelude.seq` Prelude.rnf terminating
