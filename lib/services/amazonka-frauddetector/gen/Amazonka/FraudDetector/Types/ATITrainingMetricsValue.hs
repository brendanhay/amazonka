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
-- Module      : Amazonka.FraudDetector.Types.ATITrainingMetricsValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ATITrainingMetricsValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.ATIMetricDataPoint
import Amazonka.FraudDetector.Types.ATIModelPerformance
import qualified Amazonka.Prelude as Prelude

-- | The Account Takeover Insights (ATI) model training metric details.
--
-- /See:/ 'newATITrainingMetricsValue' smart constructor.
data ATITrainingMetricsValue = ATITrainingMetricsValue'
  { -- | The model\'s performance metrics data points.
    metricDataPoints :: Prelude.Maybe [ATIMetricDataPoint],
    -- | The model\'s overall performance scores.
    modelPerformance :: Prelude.Maybe ATIModelPerformance
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ATITrainingMetricsValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDataPoints', 'aTITrainingMetricsValue_metricDataPoints' - The model\'s performance metrics data points.
--
-- 'modelPerformance', 'aTITrainingMetricsValue_modelPerformance' - The model\'s overall performance scores.
newATITrainingMetricsValue ::
  ATITrainingMetricsValue
newATITrainingMetricsValue =
  ATITrainingMetricsValue'
    { metricDataPoints =
        Prelude.Nothing,
      modelPerformance = Prelude.Nothing
    }

-- | The model\'s performance metrics data points.
aTITrainingMetricsValue_metricDataPoints :: Lens.Lens' ATITrainingMetricsValue (Prelude.Maybe [ATIMetricDataPoint])
aTITrainingMetricsValue_metricDataPoints = Lens.lens (\ATITrainingMetricsValue' {metricDataPoints} -> metricDataPoints) (\s@ATITrainingMetricsValue' {} a -> s {metricDataPoints = a} :: ATITrainingMetricsValue) Prelude.. Lens.mapping Lens.coerced

-- | The model\'s overall performance scores.
aTITrainingMetricsValue_modelPerformance :: Lens.Lens' ATITrainingMetricsValue (Prelude.Maybe ATIModelPerformance)
aTITrainingMetricsValue_modelPerformance = Lens.lens (\ATITrainingMetricsValue' {modelPerformance} -> modelPerformance) (\s@ATITrainingMetricsValue' {} a -> s {modelPerformance = a} :: ATITrainingMetricsValue)

instance Data.FromJSON ATITrainingMetricsValue where
  parseJSON =
    Data.withObject
      "ATITrainingMetricsValue"
      ( \x ->
          ATITrainingMetricsValue'
            Prelude.<$> ( x Data..:? "metricDataPoints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "modelPerformance")
      )

instance Prelude.Hashable ATITrainingMetricsValue where
  hashWithSalt _salt ATITrainingMetricsValue' {..} =
    _salt `Prelude.hashWithSalt` metricDataPoints
      `Prelude.hashWithSalt` modelPerformance

instance Prelude.NFData ATITrainingMetricsValue where
  rnf ATITrainingMetricsValue' {..} =
    Prelude.rnf metricDataPoints
      `Prelude.seq` Prelude.rnf modelPerformance
