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
-- Module      : Amazonka.LookoutMetrics.Types.DataQualityMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DataQualityMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.DataQualityMetricType
import qualified Amazonka.Prelude as Prelude

-- | An array that describes a data quality metric. Each @DataQualityMetric@
-- object contains the data quality metric name, its value, a description
-- of the metric, and the affected column.
--
-- /See:/ 'newDataQualityMetric' smart constructor.
data DataQualityMetric = DataQualityMetric'
  { -- | A description of the data quality metric.
    metricDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the data quality metric.
    metricType :: Prelude.Maybe DataQualityMetricType,
    -- | The value of the data quality metric.
    metricValue :: Prelude.Maybe Prelude.Double,
    -- | The column that is being monitored.
    relatedColumnName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDescription', 'dataQualityMetric_metricDescription' - A description of the data quality metric.
--
-- 'metricType', 'dataQualityMetric_metricType' - The name of the data quality metric.
--
-- 'metricValue', 'dataQualityMetric_metricValue' - The value of the data quality metric.
--
-- 'relatedColumnName', 'dataQualityMetric_relatedColumnName' - The column that is being monitored.
newDataQualityMetric ::
  DataQualityMetric
newDataQualityMetric =
  DataQualityMetric'
    { metricDescription =
        Prelude.Nothing,
      metricType = Prelude.Nothing,
      metricValue = Prelude.Nothing,
      relatedColumnName = Prelude.Nothing
    }

-- | A description of the data quality metric.
dataQualityMetric_metricDescription :: Lens.Lens' DataQualityMetric (Prelude.Maybe Prelude.Text)
dataQualityMetric_metricDescription = Lens.lens (\DataQualityMetric' {metricDescription} -> metricDescription) (\s@DataQualityMetric' {} a -> s {metricDescription = a} :: DataQualityMetric)

-- | The name of the data quality metric.
dataQualityMetric_metricType :: Lens.Lens' DataQualityMetric (Prelude.Maybe DataQualityMetricType)
dataQualityMetric_metricType = Lens.lens (\DataQualityMetric' {metricType} -> metricType) (\s@DataQualityMetric' {} a -> s {metricType = a} :: DataQualityMetric)

-- | The value of the data quality metric.
dataQualityMetric_metricValue :: Lens.Lens' DataQualityMetric (Prelude.Maybe Prelude.Double)
dataQualityMetric_metricValue = Lens.lens (\DataQualityMetric' {metricValue} -> metricValue) (\s@DataQualityMetric' {} a -> s {metricValue = a} :: DataQualityMetric)

-- | The column that is being monitored.
dataQualityMetric_relatedColumnName :: Lens.Lens' DataQualityMetric (Prelude.Maybe Prelude.Text)
dataQualityMetric_relatedColumnName = Lens.lens (\DataQualityMetric' {relatedColumnName} -> relatedColumnName) (\s@DataQualityMetric' {} a -> s {relatedColumnName = a} :: DataQualityMetric)

instance Data.FromJSON DataQualityMetric where
  parseJSON =
    Data.withObject
      "DataQualityMetric"
      ( \x ->
          DataQualityMetric'
            Prelude.<$> (x Data..:? "MetricDescription")
            Prelude.<*> (x Data..:? "MetricType")
            Prelude.<*> (x Data..:? "MetricValue")
            Prelude.<*> (x Data..:? "RelatedColumnName")
      )

instance Prelude.Hashable DataQualityMetric where
  hashWithSalt _salt DataQualityMetric' {..} =
    _salt `Prelude.hashWithSalt` metricDescription
      `Prelude.hashWithSalt` metricType
      `Prelude.hashWithSalt` metricValue
      `Prelude.hashWithSalt` relatedColumnName

instance Prelude.NFData DataQualityMetric where
  rnf DataQualityMetric' {..} =
    Prelude.rnf metricDescription
      `Prelude.seq` Prelude.rnf metricType
      `Prelude.seq` Prelude.rnf metricValue
      `Prelude.seq` Prelude.rnf relatedColumnName
