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
-- Module      : Amazonka.Forecast.Types.PredictorBaseline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.PredictorBaseline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.BaselineMetric
import qualified Amazonka.Prelude as Prelude

-- | Metrics you can use as a baseline for comparison purposes. Use these
-- metrics when you interpret monitoring results for an auto predictor.
--
-- /See:/ 'newPredictorBaseline' smart constructor.
data PredictorBaseline = PredictorBaseline'
  { -- | The initial
    -- <https://docs.aws.amazon.com/forecast/latest/dg/metrics.html accuracy metrics>
    -- for the predictor. Use these metrics as a baseline for comparison
    -- purposes as you use your predictor and the metrics change.
    baselineMetrics :: Prelude.Maybe [BaselineMetric]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictorBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineMetrics', 'predictorBaseline_baselineMetrics' - The initial
-- <https://docs.aws.amazon.com/forecast/latest/dg/metrics.html accuracy metrics>
-- for the predictor. Use these metrics as a baseline for comparison
-- purposes as you use your predictor and the metrics change.
newPredictorBaseline ::
  PredictorBaseline
newPredictorBaseline =
  PredictorBaseline'
    { baselineMetrics =
        Prelude.Nothing
    }

-- | The initial
-- <https://docs.aws.amazon.com/forecast/latest/dg/metrics.html accuracy metrics>
-- for the predictor. Use these metrics as a baseline for comparison
-- purposes as you use your predictor and the metrics change.
predictorBaseline_baselineMetrics :: Lens.Lens' PredictorBaseline (Prelude.Maybe [BaselineMetric])
predictorBaseline_baselineMetrics = Lens.lens (\PredictorBaseline' {baselineMetrics} -> baselineMetrics) (\s@PredictorBaseline' {} a -> s {baselineMetrics = a} :: PredictorBaseline) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PredictorBaseline where
  parseJSON =
    Data.withObject
      "PredictorBaseline"
      ( \x ->
          PredictorBaseline'
            Prelude.<$> ( x Data..:? "BaselineMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PredictorBaseline where
  hashWithSalt _salt PredictorBaseline' {..} =
    _salt `Prelude.hashWithSalt` baselineMetrics

instance Prelude.NFData PredictorBaseline where
  rnf PredictorBaseline' {..} =
    Prelude.rnf baselineMetrics
