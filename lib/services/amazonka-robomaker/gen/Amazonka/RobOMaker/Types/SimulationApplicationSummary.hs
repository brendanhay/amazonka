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
-- Module      : Amazonka.RobOMaker.Types.SimulationApplicationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SimulationApplicationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.RobotSoftwareSuite
import Amazonka.RobOMaker.Types.SimulationSoftwareSuite

-- | Summary information for a simulation application.
--
-- /See:/ 'newSimulationApplicationSummary' smart constructor.
data SimulationApplicationSummary = SimulationApplicationSummary'
  { -- | The name of the simulation application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the simulation
    -- application was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the simulation application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Information about a robot software suite (ROS distribution).
    robotSoftwareSuite :: Prelude.Maybe RobotSoftwareSuite,
    -- | Information about a simulation software suite.
    simulationSoftwareSuite :: Prelude.Maybe SimulationSoftwareSuite,
    -- | The version of the simulation application.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'simulationApplicationSummary_name' - The name of the simulation application.
--
-- 'lastUpdatedAt', 'simulationApplicationSummary_lastUpdatedAt' - The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
--
-- 'arn', 'simulationApplicationSummary_arn' - The Amazon Resource Name (ARN) of the simulation application.
--
-- 'robotSoftwareSuite', 'simulationApplicationSummary_robotSoftwareSuite' - Information about a robot software suite (ROS distribution).
--
-- 'simulationSoftwareSuite', 'simulationApplicationSummary_simulationSoftwareSuite' - Information about a simulation software suite.
--
-- 'version', 'simulationApplicationSummary_version' - The version of the simulation application.
newSimulationApplicationSummary ::
  SimulationApplicationSummary
newSimulationApplicationSummary =
  SimulationApplicationSummary'
    { name =
        Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      robotSoftwareSuite = Prelude.Nothing,
      simulationSoftwareSuite = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the simulation application.
simulationApplicationSummary_name :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe Prelude.Text)
simulationApplicationSummary_name = Lens.lens (\SimulationApplicationSummary' {name} -> name) (\s@SimulationApplicationSummary' {} a -> s {name = a} :: SimulationApplicationSummary)

-- | The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
simulationApplicationSummary_lastUpdatedAt :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe Prelude.UTCTime)
simulationApplicationSummary_lastUpdatedAt = Lens.lens (\SimulationApplicationSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@SimulationApplicationSummary' {} a -> s {lastUpdatedAt = a} :: SimulationApplicationSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the simulation application.
simulationApplicationSummary_arn :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe Prelude.Text)
simulationApplicationSummary_arn = Lens.lens (\SimulationApplicationSummary' {arn} -> arn) (\s@SimulationApplicationSummary' {} a -> s {arn = a} :: SimulationApplicationSummary)

-- | Information about a robot software suite (ROS distribution).
simulationApplicationSummary_robotSoftwareSuite :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe RobotSoftwareSuite)
simulationApplicationSummary_robotSoftwareSuite = Lens.lens (\SimulationApplicationSummary' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@SimulationApplicationSummary' {} a -> s {robotSoftwareSuite = a} :: SimulationApplicationSummary)

-- | Information about a simulation software suite.
simulationApplicationSummary_simulationSoftwareSuite :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe SimulationSoftwareSuite)
simulationApplicationSummary_simulationSoftwareSuite = Lens.lens (\SimulationApplicationSummary' {simulationSoftwareSuite} -> simulationSoftwareSuite) (\s@SimulationApplicationSummary' {} a -> s {simulationSoftwareSuite = a} :: SimulationApplicationSummary)

-- | The version of the simulation application.
simulationApplicationSummary_version :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe Prelude.Text)
simulationApplicationSummary_version = Lens.lens (\SimulationApplicationSummary' {version} -> version) (\s@SimulationApplicationSummary' {} a -> s {version = a} :: SimulationApplicationSummary)

instance Core.FromJSON SimulationApplicationSummary where
  parseJSON =
    Core.withObject
      "SimulationApplicationSummary"
      ( \x ->
          SimulationApplicationSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "robotSoftwareSuite")
            Prelude.<*> (x Core..:? "simulationSoftwareSuite")
            Prelude.<*> (x Core..:? "version")
      )

instance
  Prelude.Hashable
    SimulationApplicationSummary
  where
  hashWithSalt _salt SimulationApplicationSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` robotSoftwareSuite
      `Prelude.hashWithSalt` simulationSoftwareSuite
      `Prelude.hashWithSalt` version

instance Prelude.NFData SimulationApplicationSummary where
  rnf SimulationApplicationSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf robotSoftwareSuite
      `Prelude.seq` Prelude.rnf simulationSoftwareSuite
      `Prelude.seq` Prelude.rnf version
