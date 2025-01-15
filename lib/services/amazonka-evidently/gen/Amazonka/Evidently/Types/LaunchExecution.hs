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
-- Module      : Amazonka.Evidently.Types.LaunchExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.LaunchExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains information about the start and end times of the
-- launch.
--
-- /See:/ 'newLaunchExecution' smart constructor.
data LaunchExecution = LaunchExecution'
  { -- | The date and time that the launch ended.
    endedTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the launch started.
    startedTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endedTime', 'launchExecution_endedTime' - The date and time that the launch ended.
--
-- 'startedTime', 'launchExecution_startedTime' - The date and time that the launch started.
newLaunchExecution ::
  LaunchExecution
newLaunchExecution =
  LaunchExecution'
    { endedTime = Prelude.Nothing,
      startedTime = Prelude.Nothing
    }

-- | The date and time that the launch ended.
launchExecution_endedTime :: Lens.Lens' LaunchExecution (Prelude.Maybe Prelude.UTCTime)
launchExecution_endedTime = Lens.lens (\LaunchExecution' {endedTime} -> endedTime) (\s@LaunchExecution' {} a -> s {endedTime = a} :: LaunchExecution) Prelude.. Lens.mapping Data._Time

-- | The date and time that the launch started.
launchExecution_startedTime :: Lens.Lens' LaunchExecution (Prelude.Maybe Prelude.UTCTime)
launchExecution_startedTime = Lens.lens (\LaunchExecution' {startedTime} -> startedTime) (\s@LaunchExecution' {} a -> s {startedTime = a} :: LaunchExecution) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON LaunchExecution where
  parseJSON =
    Data.withObject
      "LaunchExecution"
      ( \x ->
          LaunchExecution'
            Prelude.<$> (x Data..:? "endedTime")
            Prelude.<*> (x Data..:? "startedTime")
      )

instance Prelude.Hashable LaunchExecution where
  hashWithSalt _salt LaunchExecution' {..} =
    _salt
      `Prelude.hashWithSalt` endedTime
      `Prelude.hashWithSalt` startedTime

instance Prelude.NFData LaunchExecution where
  rnf LaunchExecution' {..} =
    Prelude.rnf endedTime `Prelude.seq`
      Prelude.rnf startedTime
