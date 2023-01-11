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
-- Module      : Amazonka.SageMaker.Types.DataQualityBaselineConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DataQualityBaselineConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringConstraintsResource
import Amazonka.SageMaker.Types.MonitoringStatisticsResource

-- | Configuration for monitoring constraints and monitoring statistics.
-- These baseline resources are compared against the results of the current
-- job from the series of jobs scheduled to collect data periodically.
--
-- /See:/ 'newDataQualityBaselineConfig' smart constructor.
data DataQualityBaselineConfig = DataQualityBaselineConfig'
  { -- | The name of the job that performs baselining for the data quality
    -- monitoring job.
    baseliningJobName :: Prelude.Maybe Prelude.Text,
    constraintsResource :: Prelude.Maybe MonitoringConstraintsResource,
    statisticsResource :: Prelude.Maybe MonitoringStatisticsResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityBaselineConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseliningJobName', 'dataQualityBaselineConfig_baseliningJobName' - The name of the job that performs baselining for the data quality
-- monitoring job.
--
-- 'constraintsResource', 'dataQualityBaselineConfig_constraintsResource' - Undocumented member.
--
-- 'statisticsResource', 'dataQualityBaselineConfig_statisticsResource' - Undocumented member.
newDataQualityBaselineConfig ::
  DataQualityBaselineConfig
newDataQualityBaselineConfig =
  DataQualityBaselineConfig'
    { baseliningJobName =
        Prelude.Nothing,
      constraintsResource = Prelude.Nothing,
      statisticsResource = Prelude.Nothing
    }

-- | The name of the job that performs baselining for the data quality
-- monitoring job.
dataQualityBaselineConfig_baseliningJobName :: Lens.Lens' DataQualityBaselineConfig (Prelude.Maybe Prelude.Text)
dataQualityBaselineConfig_baseliningJobName = Lens.lens (\DataQualityBaselineConfig' {baseliningJobName} -> baseliningJobName) (\s@DataQualityBaselineConfig' {} a -> s {baseliningJobName = a} :: DataQualityBaselineConfig)

-- | Undocumented member.
dataQualityBaselineConfig_constraintsResource :: Lens.Lens' DataQualityBaselineConfig (Prelude.Maybe MonitoringConstraintsResource)
dataQualityBaselineConfig_constraintsResource = Lens.lens (\DataQualityBaselineConfig' {constraintsResource} -> constraintsResource) (\s@DataQualityBaselineConfig' {} a -> s {constraintsResource = a} :: DataQualityBaselineConfig)

-- | Undocumented member.
dataQualityBaselineConfig_statisticsResource :: Lens.Lens' DataQualityBaselineConfig (Prelude.Maybe MonitoringStatisticsResource)
dataQualityBaselineConfig_statisticsResource = Lens.lens (\DataQualityBaselineConfig' {statisticsResource} -> statisticsResource) (\s@DataQualityBaselineConfig' {} a -> s {statisticsResource = a} :: DataQualityBaselineConfig)

instance Data.FromJSON DataQualityBaselineConfig where
  parseJSON =
    Data.withObject
      "DataQualityBaselineConfig"
      ( \x ->
          DataQualityBaselineConfig'
            Prelude.<$> (x Data..:? "BaseliningJobName")
            Prelude.<*> (x Data..:? "ConstraintsResource")
            Prelude.<*> (x Data..:? "StatisticsResource")
      )

instance Prelude.Hashable DataQualityBaselineConfig where
  hashWithSalt _salt DataQualityBaselineConfig' {..} =
    _salt `Prelude.hashWithSalt` baseliningJobName
      `Prelude.hashWithSalt` constraintsResource
      `Prelude.hashWithSalt` statisticsResource

instance Prelude.NFData DataQualityBaselineConfig where
  rnf DataQualityBaselineConfig' {..} =
    Prelude.rnf baseliningJobName
      `Prelude.seq` Prelude.rnf constraintsResource
      `Prelude.seq` Prelude.rnf statisticsResource

instance Data.ToJSON DataQualityBaselineConfig where
  toJSON DataQualityBaselineConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BaseliningJobName" Data..=)
              Prelude.<$> baseliningJobName,
            ("ConstraintsResource" Data..=)
              Prelude.<$> constraintsResource,
            ("StatisticsResource" Data..=)
              Prelude.<$> statisticsResource
          ]
      )
