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
-- Module      : Amazonka.LookoutMetrics.Types.MetricSetDataQualityMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.MetricSetDataQualityMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.DataQualityMetric
import qualified Amazonka.Prelude as Prelude

-- | An array of @DataQualityMetric@ objects that describes one or more data
-- quality metrics.
--
-- /See:/ 'newMetricSetDataQualityMetric' smart constructor.
data MetricSetDataQualityMetric = MetricSetDataQualityMetric'
  { -- | The array of data quality metrics contained in the data quality metric
    -- set.
    dataQualityMetricList :: Prelude.Maybe [DataQualityMetric],
    -- | The Amazon Resource Name (ARN) of the data quality metric array.
    metricSetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricSetDataQualityMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataQualityMetricList', 'metricSetDataQualityMetric_dataQualityMetricList' - The array of data quality metrics contained in the data quality metric
-- set.
--
-- 'metricSetArn', 'metricSetDataQualityMetric_metricSetArn' - The Amazon Resource Name (ARN) of the data quality metric array.
newMetricSetDataQualityMetric ::
  MetricSetDataQualityMetric
newMetricSetDataQualityMetric =
  MetricSetDataQualityMetric'
    { dataQualityMetricList =
        Prelude.Nothing,
      metricSetArn = Prelude.Nothing
    }

-- | The array of data quality metrics contained in the data quality metric
-- set.
metricSetDataQualityMetric_dataQualityMetricList :: Lens.Lens' MetricSetDataQualityMetric (Prelude.Maybe [DataQualityMetric])
metricSetDataQualityMetric_dataQualityMetricList = Lens.lens (\MetricSetDataQualityMetric' {dataQualityMetricList} -> dataQualityMetricList) (\s@MetricSetDataQualityMetric' {} a -> s {dataQualityMetricList = a} :: MetricSetDataQualityMetric) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the data quality metric array.
metricSetDataQualityMetric_metricSetArn :: Lens.Lens' MetricSetDataQualityMetric (Prelude.Maybe Prelude.Text)
metricSetDataQualityMetric_metricSetArn = Lens.lens (\MetricSetDataQualityMetric' {metricSetArn} -> metricSetArn) (\s@MetricSetDataQualityMetric' {} a -> s {metricSetArn = a} :: MetricSetDataQualityMetric)

instance Data.FromJSON MetricSetDataQualityMetric where
  parseJSON =
    Data.withObject
      "MetricSetDataQualityMetric"
      ( \x ->
          MetricSetDataQualityMetric'
            Prelude.<$> ( x
                            Data..:? "DataQualityMetricList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MetricSetArn")
      )

instance Prelude.Hashable MetricSetDataQualityMetric where
  hashWithSalt _salt MetricSetDataQualityMetric' {..} =
    _salt
      `Prelude.hashWithSalt` dataQualityMetricList
      `Prelude.hashWithSalt` metricSetArn

instance Prelude.NFData MetricSetDataQualityMetric where
  rnf MetricSetDataQualityMetric' {..} =
    Prelude.rnf dataQualityMetricList
      `Prelude.seq` Prelude.rnf metricSetArn
