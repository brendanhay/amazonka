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
-- Module      : Amazonka.DrS.Types.LifeCycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.LifeCycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types.LifeCycleLastLaunch
import qualified Amazonka.Prelude as Prelude

-- | An object representing the Source Server Lifecycle.
--
-- /See:/ 'newLifeCycle' smart constructor.
data LifeCycle = LifeCycle'
  { -- | The date and time of when the Source Server was added to the service.
    addedToServiceDateTime :: Prelude.Maybe Prelude.Text,
    -- | An object containing information regarding the last launch of the Source
    -- Server.
    lastLaunch :: Prelude.Maybe LifeCycleLastLaunch,
    -- | The amount of time that the Source Server has been replicating for.
    elapsedReplicationDuration :: Prelude.Maybe Prelude.Text,
    -- | The date and time this Source Server was last seen by the service.
    lastSeenByServiceDateTime :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the first byte that was replicated from the Source
    -- Server.
    firstByteDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifeCycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addedToServiceDateTime', 'lifeCycle_addedToServiceDateTime' - The date and time of when the Source Server was added to the service.
--
-- 'lastLaunch', 'lifeCycle_lastLaunch' - An object containing information regarding the last launch of the Source
-- Server.
--
-- 'elapsedReplicationDuration', 'lifeCycle_elapsedReplicationDuration' - The amount of time that the Source Server has been replicating for.
--
-- 'lastSeenByServiceDateTime', 'lifeCycle_lastSeenByServiceDateTime' - The date and time this Source Server was last seen by the service.
--
-- 'firstByteDateTime', 'lifeCycle_firstByteDateTime' - The date and time of the first byte that was replicated from the Source
-- Server.
newLifeCycle ::
  LifeCycle
newLifeCycle =
  LifeCycle'
    { addedToServiceDateTime =
        Prelude.Nothing,
      lastLaunch = Prelude.Nothing,
      elapsedReplicationDuration = Prelude.Nothing,
      lastSeenByServiceDateTime = Prelude.Nothing,
      firstByteDateTime = Prelude.Nothing
    }

-- | The date and time of when the Source Server was added to the service.
lifeCycle_addedToServiceDateTime :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_addedToServiceDateTime = Lens.lens (\LifeCycle' {addedToServiceDateTime} -> addedToServiceDateTime) (\s@LifeCycle' {} a -> s {addedToServiceDateTime = a} :: LifeCycle)

-- | An object containing information regarding the last launch of the Source
-- Server.
lifeCycle_lastLaunch :: Lens.Lens' LifeCycle (Prelude.Maybe LifeCycleLastLaunch)
lifeCycle_lastLaunch = Lens.lens (\LifeCycle' {lastLaunch} -> lastLaunch) (\s@LifeCycle' {} a -> s {lastLaunch = a} :: LifeCycle)

-- | The amount of time that the Source Server has been replicating for.
lifeCycle_elapsedReplicationDuration :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_elapsedReplicationDuration = Lens.lens (\LifeCycle' {elapsedReplicationDuration} -> elapsedReplicationDuration) (\s@LifeCycle' {} a -> s {elapsedReplicationDuration = a} :: LifeCycle)

-- | The date and time this Source Server was last seen by the service.
lifeCycle_lastSeenByServiceDateTime :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_lastSeenByServiceDateTime = Lens.lens (\LifeCycle' {lastSeenByServiceDateTime} -> lastSeenByServiceDateTime) (\s@LifeCycle' {} a -> s {lastSeenByServiceDateTime = a} :: LifeCycle)

-- | The date and time of the first byte that was replicated from the Source
-- Server.
lifeCycle_firstByteDateTime :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_firstByteDateTime = Lens.lens (\LifeCycle' {firstByteDateTime} -> firstByteDateTime) (\s@LifeCycle' {} a -> s {firstByteDateTime = a} :: LifeCycle)

instance Core.FromJSON LifeCycle where
  parseJSON =
    Core.withObject
      "LifeCycle"
      ( \x ->
          LifeCycle'
            Prelude.<$> (x Core..:? "addedToServiceDateTime")
            Prelude.<*> (x Core..:? "lastLaunch")
            Prelude.<*> (x Core..:? "elapsedReplicationDuration")
            Prelude.<*> (x Core..:? "lastSeenByServiceDateTime")
            Prelude.<*> (x Core..:? "firstByteDateTime")
      )

instance Prelude.Hashable LifeCycle where
  hashWithSalt _salt LifeCycle' {..} =
    _salt `Prelude.hashWithSalt` addedToServiceDateTime
      `Prelude.hashWithSalt` lastLaunch
      `Prelude.hashWithSalt` elapsedReplicationDuration
      `Prelude.hashWithSalt` lastSeenByServiceDateTime
      `Prelude.hashWithSalt` firstByteDateTime

instance Prelude.NFData LifeCycle where
  rnf LifeCycle' {..} =
    Prelude.rnf addedToServiceDateTime
      `Prelude.seq` Prelude.rnf lastLaunch
      `Prelude.seq` Prelude.rnf elapsedReplicationDuration
      `Prelude.seq` Prelude.rnf lastSeenByServiceDateTime
      `Prelude.seq` Prelude.rnf firstByteDateTime
