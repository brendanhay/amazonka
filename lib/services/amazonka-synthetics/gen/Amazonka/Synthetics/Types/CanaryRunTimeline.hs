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
-- Module      : Amazonka.Synthetics.Types.CanaryRunTimeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryRunTimeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains the start and end times of a single canary run.
--
-- /See:/ 'newCanaryRunTimeline' smart constructor.
data CanaryRunTimeline = CanaryRunTimeline'
  { -- | The end time of the run.
    completed :: Prelude.Maybe Data.POSIX,
    -- | The start time of the run.
    started :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanaryRunTimeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completed', 'canaryRunTimeline_completed' - The end time of the run.
--
-- 'started', 'canaryRunTimeline_started' - The start time of the run.
newCanaryRunTimeline ::
  CanaryRunTimeline
newCanaryRunTimeline =
  CanaryRunTimeline'
    { completed = Prelude.Nothing,
      started = Prelude.Nothing
    }

-- | The end time of the run.
canaryRunTimeline_completed :: Lens.Lens' CanaryRunTimeline (Prelude.Maybe Prelude.UTCTime)
canaryRunTimeline_completed = Lens.lens (\CanaryRunTimeline' {completed} -> completed) (\s@CanaryRunTimeline' {} a -> s {completed = a} :: CanaryRunTimeline) Prelude.. Lens.mapping Data._Time

-- | The start time of the run.
canaryRunTimeline_started :: Lens.Lens' CanaryRunTimeline (Prelude.Maybe Prelude.UTCTime)
canaryRunTimeline_started = Lens.lens (\CanaryRunTimeline' {started} -> started) (\s@CanaryRunTimeline' {} a -> s {started = a} :: CanaryRunTimeline) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CanaryRunTimeline where
  parseJSON =
    Data.withObject
      "CanaryRunTimeline"
      ( \x ->
          CanaryRunTimeline'
            Prelude.<$> (x Data..:? "Completed")
            Prelude.<*> (x Data..:? "Started")
      )

instance Prelude.Hashable CanaryRunTimeline where
  hashWithSalt _salt CanaryRunTimeline' {..} =
    _salt
      `Prelude.hashWithSalt` completed
      `Prelude.hashWithSalt` started

instance Prelude.NFData CanaryRunTimeline where
  rnf CanaryRunTimeline' {..} =
    Prelude.rnf completed
      `Prelude.seq` Prelude.rnf started
