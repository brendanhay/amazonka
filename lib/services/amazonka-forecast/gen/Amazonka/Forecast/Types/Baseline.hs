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
-- Module      : Amazonka.Forecast.Types.Baseline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.Baseline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.PredictorBaseline
import qualified Amazonka.Prelude as Prelude

-- | Metrics you can use as a baseline for comparison purposes. Use these
-- metrics when you interpret monitoring results for an auto predictor.
--
-- /See:/ 'newBaseline' smart constructor.
data Baseline = Baseline'
  { -- | The initial
    -- <https://docs.aws.amazon.com/forecast/latest/dg/metrics.html accuracy metrics>
    -- for the predictor you are monitoring. Use these metrics as a baseline
    -- for comparison purposes as you use your predictor and the metrics
    -- change.
    predictorBaseline :: Prelude.Maybe PredictorBaseline
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Baseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictorBaseline', 'baseline_predictorBaseline' - The initial
-- <https://docs.aws.amazon.com/forecast/latest/dg/metrics.html accuracy metrics>
-- for the predictor you are monitoring. Use these metrics as a baseline
-- for comparison purposes as you use your predictor and the metrics
-- change.
newBaseline ::
  Baseline
newBaseline =
  Baseline' {predictorBaseline = Prelude.Nothing}

-- | The initial
-- <https://docs.aws.amazon.com/forecast/latest/dg/metrics.html accuracy metrics>
-- for the predictor you are monitoring. Use these metrics as a baseline
-- for comparison purposes as you use your predictor and the metrics
-- change.
baseline_predictorBaseline :: Lens.Lens' Baseline (Prelude.Maybe PredictorBaseline)
baseline_predictorBaseline = Lens.lens (\Baseline' {predictorBaseline} -> predictorBaseline) (\s@Baseline' {} a -> s {predictorBaseline = a} :: Baseline)

instance Data.FromJSON Baseline where
  parseJSON =
    Data.withObject
      "Baseline"
      ( \x ->
          Baseline'
            Prelude.<$> (x Data..:? "PredictorBaseline")
      )

instance Prelude.Hashable Baseline where
  hashWithSalt _salt Baseline' {..} =
    _salt `Prelude.hashWithSalt` predictorBaseline

instance Prelude.NFData Baseline where
  rnf Baseline' {..} = Prelude.rnf predictorBaseline
