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
-- Module      : Amazonka.FraudDetector.Types.OFITrainingMetricsValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.OFITrainingMetricsValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.OFIMetricDataPoint
import Amazonka.FraudDetector.Types.OFIModelPerformance
import qualified Amazonka.Prelude as Prelude

-- | The Online Fraud Insights (OFI) model training metric details.
--
-- /See:/ 'newOFITrainingMetricsValue' smart constructor.
data OFITrainingMetricsValue = OFITrainingMetricsValue'
  { -- | The model\'s performance metrics data points.
    metricDataPoints :: Prelude.Maybe [OFIMetricDataPoint],
    -- | The model\'s overall performance score.
    modelPerformance :: Prelude.Maybe OFIModelPerformance
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OFITrainingMetricsValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDataPoints', 'oFITrainingMetricsValue_metricDataPoints' - The model\'s performance metrics data points.
--
-- 'modelPerformance', 'oFITrainingMetricsValue_modelPerformance' - The model\'s overall performance score.
newOFITrainingMetricsValue ::
  OFITrainingMetricsValue
newOFITrainingMetricsValue =
  OFITrainingMetricsValue'
    { metricDataPoints =
        Prelude.Nothing,
      modelPerformance = Prelude.Nothing
    }

-- | The model\'s performance metrics data points.
oFITrainingMetricsValue_metricDataPoints :: Lens.Lens' OFITrainingMetricsValue (Prelude.Maybe [OFIMetricDataPoint])
oFITrainingMetricsValue_metricDataPoints = Lens.lens (\OFITrainingMetricsValue' {metricDataPoints} -> metricDataPoints) (\s@OFITrainingMetricsValue' {} a -> s {metricDataPoints = a} :: OFITrainingMetricsValue) Prelude.. Lens.mapping Lens.coerced

-- | The model\'s overall performance score.
oFITrainingMetricsValue_modelPerformance :: Lens.Lens' OFITrainingMetricsValue (Prelude.Maybe OFIModelPerformance)
oFITrainingMetricsValue_modelPerformance = Lens.lens (\OFITrainingMetricsValue' {modelPerformance} -> modelPerformance) (\s@OFITrainingMetricsValue' {} a -> s {modelPerformance = a} :: OFITrainingMetricsValue)

instance Data.FromJSON OFITrainingMetricsValue where
  parseJSON =
    Data.withObject
      "OFITrainingMetricsValue"
      ( \x ->
          OFITrainingMetricsValue'
            Prelude.<$> ( x
                            Data..:? "metricDataPoints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "modelPerformance")
      )

instance Prelude.Hashable OFITrainingMetricsValue where
  hashWithSalt _salt OFITrainingMetricsValue' {..} =
    _salt
      `Prelude.hashWithSalt` metricDataPoints
      `Prelude.hashWithSalt` modelPerformance

instance Prelude.NFData OFITrainingMetricsValue where
  rnf OFITrainingMetricsValue' {..} =
    Prelude.rnf metricDataPoints
      `Prelude.seq` Prelude.rnf modelPerformance
