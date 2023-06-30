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
-- Module      : Amazonka.SimSpaceWeaver.Types.LiveSimulationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.LiveSimulationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SimSpaceWeaver.Types.Domain
import Amazonka.SimSpaceWeaver.Types.SimulationClock

-- | A collection of additional state information, such as domain and clock
-- configuration.
--
-- /See:/ 'newLiveSimulationState' smart constructor.
data LiveSimulationState = LiveSimulationState'
  { -- | A list of simulation clocks.
    --
    -- At this time, a simulation has only one clock.
    clocks :: Prelude.Maybe [SimulationClock],
    -- | A list of domains for the simulation. For more information about
    -- domains, see
    -- <https://docs.aws.amazon.com/simspaceweaver/latest/userguide/what-is_key-concepts.html Key concepts>
    -- in the /Amazon Web Services SimSpace Weaver User Guide/.
    domains :: Prelude.Maybe [Domain]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LiveSimulationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clocks', 'liveSimulationState_clocks' - A list of simulation clocks.
--
-- At this time, a simulation has only one clock.
--
-- 'domains', 'liveSimulationState_domains' - A list of domains for the simulation. For more information about
-- domains, see
-- <https://docs.aws.amazon.com/simspaceweaver/latest/userguide/what-is_key-concepts.html Key concepts>
-- in the /Amazon Web Services SimSpace Weaver User Guide/.
newLiveSimulationState ::
  LiveSimulationState
newLiveSimulationState =
  LiveSimulationState'
    { clocks = Prelude.Nothing,
      domains = Prelude.Nothing
    }

-- | A list of simulation clocks.
--
-- At this time, a simulation has only one clock.
liveSimulationState_clocks :: Lens.Lens' LiveSimulationState (Prelude.Maybe [SimulationClock])
liveSimulationState_clocks = Lens.lens (\LiveSimulationState' {clocks} -> clocks) (\s@LiveSimulationState' {} a -> s {clocks = a} :: LiveSimulationState) Prelude.. Lens.mapping Lens.coerced

-- | A list of domains for the simulation. For more information about
-- domains, see
-- <https://docs.aws.amazon.com/simspaceweaver/latest/userguide/what-is_key-concepts.html Key concepts>
-- in the /Amazon Web Services SimSpace Weaver User Guide/.
liveSimulationState_domains :: Lens.Lens' LiveSimulationState (Prelude.Maybe [Domain])
liveSimulationState_domains = Lens.lens (\LiveSimulationState' {domains} -> domains) (\s@LiveSimulationState' {} a -> s {domains = a} :: LiveSimulationState) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LiveSimulationState where
  parseJSON =
    Data.withObject
      "LiveSimulationState"
      ( \x ->
          LiveSimulationState'
            Prelude.<$> (x Data..:? "Clocks" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Domains" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LiveSimulationState where
  hashWithSalt _salt LiveSimulationState' {..} =
    _salt
      `Prelude.hashWithSalt` clocks
      `Prelude.hashWithSalt` domains

instance Prelude.NFData LiveSimulationState where
  rnf LiveSimulationState' {..} =
    Prelude.rnf clocks
      `Prelude.seq` Prelude.rnf domains
