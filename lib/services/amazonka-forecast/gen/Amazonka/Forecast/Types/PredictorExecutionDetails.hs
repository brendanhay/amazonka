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
-- Module      : Amazonka.Forecast.Types.PredictorExecutionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.PredictorExecutionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.PredictorExecution
import qualified Amazonka.Prelude as Prelude

-- | Contains details on the backtests performed to evaluate the accuracy of
-- the predictor. The tests are returned in descending order of accuracy,
-- with the most accurate backtest appearing first. You specify the number
-- of backtests to perform when you call the operation.
--
-- /See:/ 'newPredictorExecutionDetails' smart constructor.
data PredictorExecutionDetails = PredictorExecutionDetails'
  { -- | An array of the backtests performed to evaluate the accuracy of the
    -- predictor against a particular algorithm. The @NumberOfBacktestWindows@
    -- from the object determines the number of windows in the array.
    predictorExecutions :: Prelude.Maybe (Prelude.NonEmpty PredictorExecution)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictorExecutionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictorExecutions', 'predictorExecutionDetails_predictorExecutions' - An array of the backtests performed to evaluate the accuracy of the
-- predictor against a particular algorithm. The @NumberOfBacktestWindows@
-- from the object determines the number of windows in the array.
newPredictorExecutionDetails ::
  PredictorExecutionDetails
newPredictorExecutionDetails =
  PredictorExecutionDetails'
    { predictorExecutions =
        Prelude.Nothing
    }

-- | An array of the backtests performed to evaluate the accuracy of the
-- predictor against a particular algorithm. The @NumberOfBacktestWindows@
-- from the object determines the number of windows in the array.
predictorExecutionDetails_predictorExecutions :: Lens.Lens' PredictorExecutionDetails (Prelude.Maybe (Prelude.NonEmpty PredictorExecution))
predictorExecutionDetails_predictorExecutions = Lens.lens (\PredictorExecutionDetails' {predictorExecutions} -> predictorExecutions) (\s@PredictorExecutionDetails' {} a -> s {predictorExecutions = a} :: PredictorExecutionDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PredictorExecutionDetails where
  parseJSON =
    Data.withObject
      "PredictorExecutionDetails"
      ( \x ->
          PredictorExecutionDetails'
            Prelude.<$> (x Data..:? "PredictorExecutions")
      )

instance Prelude.Hashable PredictorExecutionDetails where
  hashWithSalt _salt PredictorExecutionDetails' {..} =
    _salt `Prelude.hashWithSalt` predictorExecutions

instance Prelude.NFData PredictorExecutionDetails where
  rnf PredictorExecutionDetails' {..} =
    Prelude.rnf predictorExecutions
