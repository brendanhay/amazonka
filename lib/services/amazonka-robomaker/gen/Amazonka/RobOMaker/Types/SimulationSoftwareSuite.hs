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
-- Module      : Amazonka.RobOMaker.Types.SimulationSoftwareSuite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SimulationSoftwareSuite where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.SimulationSoftwareSuiteType

-- | Information about a simulation software suite.
--
-- /See:/ 'newSimulationSoftwareSuite' smart constructor.
data SimulationSoftwareSuite = SimulationSoftwareSuite'
  { -- | The name of the simulation software suite.
    name :: Prelude.Maybe SimulationSoftwareSuiteType,
    -- | The version of the simulation software suite.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationSoftwareSuite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'simulationSoftwareSuite_name' - The name of the simulation software suite.
--
-- 'version', 'simulationSoftwareSuite_version' - The version of the simulation software suite.
newSimulationSoftwareSuite ::
  SimulationSoftwareSuite
newSimulationSoftwareSuite =
  SimulationSoftwareSuite'
    { name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the simulation software suite.
simulationSoftwareSuite_name :: Lens.Lens' SimulationSoftwareSuite (Prelude.Maybe SimulationSoftwareSuiteType)
simulationSoftwareSuite_name = Lens.lens (\SimulationSoftwareSuite' {name} -> name) (\s@SimulationSoftwareSuite' {} a -> s {name = a} :: SimulationSoftwareSuite)

-- | The version of the simulation software suite.
simulationSoftwareSuite_version :: Lens.Lens' SimulationSoftwareSuite (Prelude.Maybe Prelude.Text)
simulationSoftwareSuite_version = Lens.lens (\SimulationSoftwareSuite' {version} -> version) (\s@SimulationSoftwareSuite' {} a -> s {version = a} :: SimulationSoftwareSuite)

instance Data.FromJSON SimulationSoftwareSuite where
  parseJSON =
    Data.withObject
      "SimulationSoftwareSuite"
      ( \x ->
          SimulationSoftwareSuite'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable SimulationSoftwareSuite where
  hashWithSalt _salt SimulationSoftwareSuite' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData SimulationSoftwareSuite where
  rnf SimulationSoftwareSuite' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version

instance Data.ToJSON SimulationSoftwareSuite where
  toJSON SimulationSoftwareSuite' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("version" Data..=) Prelude.<$> version
          ]
      )
