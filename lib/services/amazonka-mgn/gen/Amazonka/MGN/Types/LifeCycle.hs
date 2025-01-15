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
-- Module      : Amazonka.MGN.Types.LifeCycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LifeCycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.LifeCycleLastCutover
import Amazonka.MGN.Types.LifeCycleLastTest
import Amazonka.MGN.Types.LifeCycleState
import qualified Amazonka.Prelude as Prelude

-- | Lifecycle.
--
-- /See:/ 'newLifeCycle' smart constructor.
data LifeCycle = LifeCycle'
  { -- | Lifecycle added to service data and time.
    addedToServiceDateTime :: Prelude.Maybe Prelude.Text,
    -- | Lifecycle elapsed time and duration.
    elapsedReplicationDuration :: Prelude.Maybe Prelude.Text,
    -- | Lifecycle replication initiation date and time.
    firstByteDateTime :: Prelude.Maybe Prelude.Text,
    -- | Lifecycle last Cutover.
    lastCutover :: Prelude.Maybe LifeCycleLastCutover,
    -- | Lifecycle last seen date and time.
    lastSeenByServiceDateTime :: Prelude.Maybe Prelude.Text,
    -- | Lifecycle last Test.
    lastTest :: Prelude.Maybe LifeCycleLastTest,
    -- | Lifecycle state.
    state :: Prelude.Maybe LifeCycleState
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
-- 'addedToServiceDateTime', 'lifeCycle_addedToServiceDateTime' - Lifecycle added to service data and time.
--
-- 'elapsedReplicationDuration', 'lifeCycle_elapsedReplicationDuration' - Lifecycle elapsed time and duration.
--
-- 'firstByteDateTime', 'lifeCycle_firstByteDateTime' - Lifecycle replication initiation date and time.
--
-- 'lastCutover', 'lifeCycle_lastCutover' - Lifecycle last Cutover.
--
-- 'lastSeenByServiceDateTime', 'lifeCycle_lastSeenByServiceDateTime' - Lifecycle last seen date and time.
--
-- 'lastTest', 'lifeCycle_lastTest' - Lifecycle last Test.
--
-- 'state', 'lifeCycle_state' - Lifecycle state.
newLifeCycle ::
  LifeCycle
newLifeCycle =
  LifeCycle'
    { addedToServiceDateTime =
        Prelude.Nothing,
      elapsedReplicationDuration = Prelude.Nothing,
      firstByteDateTime = Prelude.Nothing,
      lastCutover = Prelude.Nothing,
      lastSeenByServiceDateTime = Prelude.Nothing,
      lastTest = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | Lifecycle added to service data and time.
lifeCycle_addedToServiceDateTime :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_addedToServiceDateTime = Lens.lens (\LifeCycle' {addedToServiceDateTime} -> addedToServiceDateTime) (\s@LifeCycle' {} a -> s {addedToServiceDateTime = a} :: LifeCycle)

-- | Lifecycle elapsed time and duration.
lifeCycle_elapsedReplicationDuration :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_elapsedReplicationDuration = Lens.lens (\LifeCycle' {elapsedReplicationDuration} -> elapsedReplicationDuration) (\s@LifeCycle' {} a -> s {elapsedReplicationDuration = a} :: LifeCycle)

-- | Lifecycle replication initiation date and time.
lifeCycle_firstByteDateTime :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_firstByteDateTime = Lens.lens (\LifeCycle' {firstByteDateTime} -> firstByteDateTime) (\s@LifeCycle' {} a -> s {firstByteDateTime = a} :: LifeCycle)

-- | Lifecycle last Cutover.
lifeCycle_lastCutover :: Lens.Lens' LifeCycle (Prelude.Maybe LifeCycleLastCutover)
lifeCycle_lastCutover = Lens.lens (\LifeCycle' {lastCutover} -> lastCutover) (\s@LifeCycle' {} a -> s {lastCutover = a} :: LifeCycle)

-- | Lifecycle last seen date and time.
lifeCycle_lastSeenByServiceDateTime :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_lastSeenByServiceDateTime = Lens.lens (\LifeCycle' {lastSeenByServiceDateTime} -> lastSeenByServiceDateTime) (\s@LifeCycle' {} a -> s {lastSeenByServiceDateTime = a} :: LifeCycle)

-- | Lifecycle last Test.
lifeCycle_lastTest :: Lens.Lens' LifeCycle (Prelude.Maybe LifeCycleLastTest)
lifeCycle_lastTest = Lens.lens (\LifeCycle' {lastTest} -> lastTest) (\s@LifeCycle' {} a -> s {lastTest = a} :: LifeCycle)

-- | Lifecycle state.
lifeCycle_state :: Lens.Lens' LifeCycle (Prelude.Maybe LifeCycleState)
lifeCycle_state = Lens.lens (\LifeCycle' {state} -> state) (\s@LifeCycle' {} a -> s {state = a} :: LifeCycle)

instance Data.FromJSON LifeCycle where
  parseJSON =
    Data.withObject
      "LifeCycle"
      ( \x ->
          LifeCycle'
            Prelude.<$> (x Data..:? "addedToServiceDateTime")
            Prelude.<*> (x Data..:? "elapsedReplicationDuration")
            Prelude.<*> (x Data..:? "firstByteDateTime")
            Prelude.<*> (x Data..:? "lastCutover")
            Prelude.<*> (x Data..:? "lastSeenByServiceDateTime")
            Prelude.<*> (x Data..:? "lastTest")
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable LifeCycle where
  hashWithSalt _salt LifeCycle' {..} =
    _salt
      `Prelude.hashWithSalt` addedToServiceDateTime
      `Prelude.hashWithSalt` elapsedReplicationDuration
      `Prelude.hashWithSalt` firstByteDateTime
      `Prelude.hashWithSalt` lastCutover
      `Prelude.hashWithSalt` lastSeenByServiceDateTime
      `Prelude.hashWithSalt` lastTest
      `Prelude.hashWithSalt` state

instance Prelude.NFData LifeCycle where
  rnf LifeCycle' {..} =
    Prelude.rnf addedToServiceDateTime `Prelude.seq`
      Prelude.rnf elapsedReplicationDuration `Prelude.seq`
        Prelude.rnf firstByteDateTime `Prelude.seq`
          Prelude.rnf lastCutover `Prelude.seq`
            Prelude.rnf lastSeenByServiceDateTime `Prelude.seq`
              Prelude.rnf lastTest `Prelude.seq`
                Prelude.rnf state
