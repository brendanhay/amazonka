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
-- Module      : Amazonka.Athena.Types.CalculationStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CalculationStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains statistics for a notebook calculation.
--
-- /See:/ 'newCalculationStatistics' smart constructor.
data CalculationStatistics = CalculationStatistics'
  { -- | The data processing unit execution time in milliseconds for the
    -- calculation.
    dpuExecutionInMillis :: Prelude.Maybe Prelude.Integer,
    -- | The progress of the calculation.
    progress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculationStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dpuExecutionInMillis', 'calculationStatistics_dpuExecutionInMillis' - The data processing unit execution time in milliseconds for the
-- calculation.
--
-- 'progress', 'calculationStatistics_progress' - The progress of the calculation.
newCalculationStatistics ::
  CalculationStatistics
newCalculationStatistics =
  CalculationStatistics'
    { dpuExecutionInMillis =
        Prelude.Nothing,
      progress = Prelude.Nothing
    }

-- | The data processing unit execution time in milliseconds for the
-- calculation.
calculationStatistics_dpuExecutionInMillis :: Lens.Lens' CalculationStatistics (Prelude.Maybe Prelude.Integer)
calculationStatistics_dpuExecutionInMillis = Lens.lens (\CalculationStatistics' {dpuExecutionInMillis} -> dpuExecutionInMillis) (\s@CalculationStatistics' {} a -> s {dpuExecutionInMillis = a} :: CalculationStatistics)

-- | The progress of the calculation.
calculationStatistics_progress :: Lens.Lens' CalculationStatistics (Prelude.Maybe Prelude.Text)
calculationStatistics_progress = Lens.lens (\CalculationStatistics' {progress} -> progress) (\s@CalculationStatistics' {} a -> s {progress = a} :: CalculationStatistics)

instance Data.FromJSON CalculationStatistics where
  parseJSON =
    Data.withObject
      "CalculationStatistics"
      ( \x ->
          CalculationStatistics'
            Prelude.<$> (x Data..:? "DpuExecutionInMillis")
            Prelude.<*> (x Data..:? "Progress")
      )

instance Prelude.Hashable CalculationStatistics where
  hashWithSalt _salt CalculationStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` dpuExecutionInMillis
      `Prelude.hashWithSalt` progress

instance Prelude.NFData CalculationStatistics where
  rnf CalculationStatistics' {..} =
    Prelude.rnf dpuExecutionInMillis
      `Prelude.seq` Prelude.rnf progress
