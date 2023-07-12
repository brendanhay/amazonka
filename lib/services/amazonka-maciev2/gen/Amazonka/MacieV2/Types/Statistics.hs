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
-- Module      : Amazonka.MacieV2.Types.Statistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Statistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides processing statistics for a classification job.
--
-- /See:/ 'newStatistics' smart constructor.
data Statistics = Statistics'
  { -- | The approximate number of objects that the job has yet to process during
    -- its current run.
    approximateNumberOfObjectsToProcess :: Prelude.Maybe Prelude.Double,
    -- | The number of times that the job has run.
    numberOfRuns :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Statistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateNumberOfObjectsToProcess', 'statistics_approximateNumberOfObjectsToProcess' - The approximate number of objects that the job has yet to process during
-- its current run.
--
-- 'numberOfRuns', 'statistics_numberOfRuns' - The number of times that the job has run.
newStatistics ::
  Statistics
newStatistics =
  Statistics'
    { approximateNumberOfObjectsToProcess =
        Prelude.Nothing,
      numberOfRuns = Prelude.Nothing
    }

-- | The approximate number of objects that the job has yet to process during
-- its current run.
statistics_approximateNumberOfObjectsToProcess :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_approximateNumberOfObjectsToProcess = Lens.lens (\Statistics' {approximateNumberOfObjectsToProcess} -> approximateNumberOfObjectsToProcess) (\s@Statistics' {} a -> s {approximateNumberOfObjectsToProcess = a} :: Statistics)

-- | The number of times that the job has run.
statistics_numberOfRuns :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_numberOfRuns = Lens.lens (\Statistics' {numberOfRuns} -> numberOfRuns) (\s@Statistics' {} a -> s {numberOfRuns = a} :: Statistics)

instance Data.FromJSON Statistics where
  parseJSON =
    Data.withObject
      "Statistics"
      ( \x ->
          Statistics'
            Prelude.<$> (x Data..:? "approximateNumberOfObjectsToProcess")
            Prelude.<*> (x Data..:? "numberOfRuns")
      )

instance Prelude.Hashable Statistics where
  hashWithSalt _salt Statistics' {..} =
    _salt
      `Prelude.hashWithSalt` approximateNumberOfObjectsToProcess
      `Prelude.hashWithSalt` numberOfRuns

instance Prelude.NFData Statistics where
  rnf Statistics' {..} =
    Prelude.rnf approximateNumberOfObjectsToProcess
      `Prelude.seq` Prelude.rnf numberOfRuns
