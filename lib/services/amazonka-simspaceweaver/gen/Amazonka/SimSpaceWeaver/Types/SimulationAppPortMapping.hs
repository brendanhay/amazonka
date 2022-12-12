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
-- Module      : Amazonka.SimSpaceWeaver.Types.SimulationAppPortMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.SimulationAppPortMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A collection of TCP\/UDP ports for a custom or service app.
--
-- /See:/ 'newSimulationAppPortMapping' smart constructor.
data SimulationAppPortMapping = SimulationAppPortMapping'
  { -- | The TCP\/UDP port number of the running app. SimSpace Weaver dynamically
    -- assigns this port number when the app starts. SimSpace Weaver maps the
    -- @Declared@ port to the @Actual@ port. Clients connect to the app using
    -- the app\'s IP address and the @Actual@ port number.
    actual :: Prelude.Maybe Prelude.Natural,
    -- | The TCP\/UDP port number of the app, declared in the simulation schema.
    -- SimSpace Weaver maps the @Declared@ port to the @Actual@ port. The
    -- source code for the app should bind to the @Declared@ port.
    declared :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationAppPortMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actual', 'simulationAppPortMapping_actual' - The TCP\/UDP port number of the running app. SimSpace Weaver dynamically
-- assigns this port number when the app starts. SimSpace Weaver maps the
-- @Declared@ port to the @Actual@ port. Clients connect to the app using
-- the app\'s IP address and the @Actual@ port number.
--
-- 'declared', 'simulationAppPortMapping_declared' - The TCP\/UDP port number of the app, declared in the simulation schema.
-- SimSpace Weaver maps the @Declared@ port to the @Actual@ port. The
-- source code for the app should bind to the @Declared@ port.
newSimulationAppPortMapping ::
  SimulationAppPortMapping
newSimulationAppPortMapping =
  SimulationAppPortMapping'
    { actual = Prelude.Nothing,
      declared = Prelude.Nothing
    }

-- | The TCP\/UDP port number of the running app. SimSpace Weaver dynamically
-- assigns this port number when the app starts. SimSpace Weaver maps the
-- @Declared@ port to the @Actual@ port. Clients connect to the app using
-- the app\'s IP address and the @Actual@ port number.
simulationAppPortMapping_actual :: Lens.Lens' SimulationAppPortMapping (Prelude.Maybe Prelude.Natural)
simulationAppPortMapping_actual = Lens.lens (\SimulationAppPortMapping' {actual} -> actual) (\s@SimulationAppPortMapping' {} a -> s {actual = a} :: SimulationAppPortMapping)

-- | The TCP\/UDP port number of the app, declared in the simulation schema.
-- SimSpace Weaver maps the @Declared@ port to the @Actual@ port. The
-- source code for the app should bind to the @Declared@ port.
simulationAppPortMapping_declared :: Lens.Lens' SimulationAppPortMapping (Prelude.Maybe Prelude.Natural)
simulationAppPortMapping_declared = Lens.lens (\SimulationAppPortMapping' {declared} -> declared) (\s@SimulationAppPortMapping' {} a -> s {declared = a} :: SimulationAppPortMapping)

instance Data.FromJSON SimulationAppPortMapping where
  parseJSON =
    Data.withObject
      "SimulationAppPortMapping"
      ( \x ->
          SimulationAppPortMapping'
            Prelude.<$> (x Data..:? "Actual")
            Prelude.<*> (x Data..:? "Declared")
      )

instance Prelude.Hashable SimulationAppPortMapping where
  hashWithSalt _salt SimulationAppPortMapping' {..} =
    _salt `Prelude.hashWithSalt` actual
      `Prelude.hashWithSalt` declared

instance Prelude.NFData SimulationAppPortMapping where
  rnf SimulationAppPortMapping' {..} =
    Prelude.rnf actual
      `Prelude.seq` Prelude.rnf declared
