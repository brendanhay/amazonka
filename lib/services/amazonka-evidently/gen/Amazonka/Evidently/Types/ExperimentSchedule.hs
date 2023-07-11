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
-- Module      : Amazonka.Evidently.Types.ExperimentSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ExperimentSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains the time and date that Evidently completed the
-- analysis of the experiment.
--
-- /See:/ 'newExperimentSchedule' smart constructor.
data ExperimentSchedule = ExperimentSchedule'
  { -- | The time and date that Evidently completed the analysis of the
    -- experiment.
    analysisCompleteTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisCompleteTime', 'experimentSchedule_analysisCompleteTime' - The time and date that Evidently completed the analysis of the
-- experiment.
newExperimentSchedule ::
  ExperimentSchedule
newExperimentSchedule =
  ExperimentSchedule'
    { analysisCompleteTime =
        Prelude.Nothing
    }

-- | The time and date that Evidently completed the analysis of the
-- experiment.
experimentSchedule_analysisCompleteTime :: Lens.Lens' ExperimentSchedule (Prelude.Maybe Prelude.UTCTime)
experimentSchedule_analysisCompleteTime = Lens.lens (\ExperimentSchedule' {analysisCompleteTime} -> analysisCompleteTime) (\s@ExperimentSchedule' {} a -> s {analysisCompleteTime = a} :: ExperimentSchedule) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ExperimentSchedule where
  parseJSON =
    Data.withObject
      "ExperimentSchedule"
      ( \x ->
          ExperimentSchedule'
            Prelude.<$> (x Data..:? "analysisCompleteTime")
      )

instance Prelude.Hashable ExperimentSchedule where
  hashWithSalt _salt ExperimentSchedule' {..} =
    _salt `Prelude.hashWithSalt` analysisCompleteTime

instance Prelude.NFData ExperimentSchedule where
  rnf ExperimentSchedule' {..} =
    Prelude.rnf analysisCompleteTime
