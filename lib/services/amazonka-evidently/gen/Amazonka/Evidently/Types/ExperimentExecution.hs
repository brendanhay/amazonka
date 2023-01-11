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
-- Module      : Amazonka.Evidently.Types.ExperimentExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ExperimentExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains the date and time that the experiment started
-- and ended.
--
-- /See:/ 'newExperimentExecution' smart constructor.
data ExperimentExecution = ExperimentExecution'
  { -- | The date and time that the experiment ended.
    endedTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the experiment started.
    startedTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endedTime', 'experimentExecution_endedTime' - The date and time that the experiment ended.
--
-- 'startedTime', 'experimentExecution_startedTime' - The date and time that the experiment started.
newExperimentExecution ::
  ExperimentExecution
newExperimentExecution =
  ExperimentExecution'
    { endedTime = Prelude.Nothing,
      startedTime = Prelude.Nothing
    }

-- | The date and time that the experiment ended.
experimentExecution_endedTime :: Lens.Lens' ExperimentExecution (Prelude.Maybe Prelude.UTCTime)
experimentExecution_endedTime = Lens.lens (\ExperimentExecution' {endedTime} -> endedTime) (\s@ExperimentExecution' {} a -> s {endedTime = a} :: ExperimentExecution) Prelude.. Lens.mapping Data._Time

-- | The date and time that the experiment started.
experimentExecution_startedTime :: Lens.Lens' ExperimentExecution (Prelude.Maybe Prelude.UTCTime)
experimentExecution_startedTime = Lens.lens (\ExperimentExecution' {startedTime} -> startedTime) (\s@ExperimentExecution' {} a -> s {startedTime = a} :: ExperimentExecution) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ExperimentExecution where
  parseJSON =
    Data.withObject
      "ExperimentExecution"
      ( \x ->
          ExperimentExecution'
            Prelude.<$> (x Data..:? "endedTime")
            Prelude.<*> (x Data..:? "startedTime")
      )

instance Prelude.Hashable ExperimentExecution where
  hashWithSalt _salt ExperimentExecution' {..} =
    _salt `Prelude.hashWithSalt` endedTime
      `Prelude.hashWithSalt` startedTime

instance Prelude.NFData ExperimentExecution where
  rnf ExperimentExecution' {..} =
    Prelude.rnf endedTime
      `Prelude.seq` Prelude.rnf startedTime
