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
-- Module      : Amazonka.SageMaker.Types.Phase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Phase where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines the traffic pattern.
--
-- /See:/ 'newPhase' smart constructor.
data Phase = Phase'
  { -- | Specified how many new users to spawn in a minute.
    spawnRate :: Prelude.Maybe Prelude.Natural,
    -- | Specifies how many concurrent users to start with.
    initialNumberOfUsers :: Prelude.Maybe Prelude.Natural,
    -- | Specifies how long traffic phase should be.
    durationInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Phase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spawnRate', 'phase_spawnRate' - Specified how many new users to spawn in a minute.
--
-- 'initialNumberOfUsers', 'phase_initialNumberOfUsers' - Specifies how many concurrent users to start with.
--
-- 'durationInSeconds', 'phase_durationInSeconds' - Specifies how long traffic phase should be.
newPhase ::
  Phase
newPhase =
  Phase'
    { spawnRate = Prelude.Nothing,
      initialNumberOfUsers = Prelude.Nothing,
      durationInSeconds = Prelude.Nothing
    }

-- | Specified how many new users to spawn in a minute.
phase_spawnRate :: Lens.Lens' Phase (Prelude.Maybe Prelude.Natural)
phase_spawnRate = Lens.lens (\Phase' {spawnRate} -> spawnRate) (\s@Phase' {} a -> s {spawnRate = a} :: Phase)

-- | Specifies how many concurrent users to start with.
phase_initialNumberOfUsers :: Lens.Lens' Phase (Prelude.Maybe Prelude.Natural)
phase_initialNumberOfUsers = Lens.lens (\Phase' {initialNumberOfUsers} -> initialNumberOfUsers) (\s@Phase' {} a -> s {initialNumberOfUsers = a} :: Phase)

-- | Specifies how long traffic phase should be.
phase_durationInSeconds :: Lens.Lens' Phase (Prelude.Maybe Prelude.Natural)
phase_durationInSeconds = Lens.lens (\Phase' {durationInSeconds} -> durationInSeconds) (\s@Phase' {} a -> s {durationInSeconds = a} :: Phase)

instance Core.FromJSON Phase where
  parseJSON =
    Core.withObject
      "Phase"
      ( \x ->
          Phase'
            Prelude.<$> (x Core..:? "SpawnRate")
            Prelude.<*> (x Core..:? "InitialNumberOfUsers")
            Prelude.<*> (x Core..:? "DurationInSeconds")
      )

instance Prelude.Hashable Phase where
  hashWithSalt _salt Phase' {..} =
    _salt `Prelude.hashWithSalt` spawnRate
      `Prelude.hashWithSalt` initialNumberOfUsers
      `Prelude.hashWithSalt` durationInSeconds

instance Prelude.NFData Phase where
  rnf Phase' {..} =
    Prelude.rnf spawnRate
      `Prelude.seq` Prelude.rnf initialNumberOfUsers
      `Prelude.seq` Prelude.rnf durationInSeconds

instance Core.ToJSON Phase where
  toJSON Phase' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SpawnRate" Core..=) Prelude.<$> spawnRate,
            ("InitialNumberOfUsers" Core..=)
              Prelude.<$> initialNumberOfUsers,
            ("DurationInSeconds" Core..=)
              Prelude.<$> durationInSeconds
          ]
      )
