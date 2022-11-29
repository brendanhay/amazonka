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
-- Module      : Amazonka.CloudWatch.Types.AnomalyDetectorConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.AnomalyDetectorConfiguration where

import Amazonka.CloudWatch.Types.Range
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration specifies details about how the anomaly detection
-- model is to be trained, including time ranges to exclude from use for
-- training the model and the time zone to use for the metric.
--
-- /See:/ 'newAnomalyDetectorConfiguration' smart constructor.
data AnomalyDetectorConfiguration = AnomalyDetectorConfiguration'
  { -- | The time zone to use for the metric. This is useful to enable the model
    -- to automatically account for daylight savings time changes if the metric
    -- is sensitive to such time changes.
    --
    -- To specify a time zone, use the name of the time zone as specified in
    -- the standard tz database. For more information, see
    -- <https://en.wikipedia.org/wiki/Tz_database tz database>.
    metricTimezone :: Prelude.Maybe Prelude.Text,
    -- | An array of time ranges to exclude from use when the anomaly detection
    -- model is trained. Use this to make sure that events that could cause
    -- unusual values for the metric, such as deployments, aren\'t used when
    -- CloudWatch creates the model.
    excludedTimeRanges :: Prelude.Maybe [Range]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyDetectorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricTimezone', 'anomalyDetectorConfiguration_metricTimezone' - The time zone to use for the metric. This is useful to enable the model
-- to automatically account for daylight savings time changes if the metric
-- is sensitive to such time changes.
--
-- To specify a time zone, use the name of the time zone as specified in
-- the standard tz database. For more information, see
-- <https://en.wikipedia.org/wiki/Tz_database tz database>.
--
-- 'excludedTimeRanges', 'anomalyDetectorConfiguration_excludedTimeRanges' - An array of time ranges to exclude from use when the anomaly detection
-- model is trained. Use this to make sure that events that could cause
-- unusual values for the metric, such as deployments, aren\'t used when
-- CloudWatch creates the model.
newAnomalyDetectorConfiguration ::
  AnomalyDetectorConfiguration
newAnomalyDetectorConfiguration =
  AnomalyDetectorConfiguration'
    { metricTimezone =
        Prelude.Nothing,
      excludedTimeRanges = Prelude.Nothing
    }

-- | The time zone to use for the metric. This is useful to enable the model
-- to automatically account for daylight savings time changes if the metric
-- is sensitive to such time changes.
--
-- To specify a time zone, use the name of the time zone as specified in
-- the standard tz database. For more information, see
-- <https://en.wikipedia.org/wiki/Tz_database tz database>.
anomalyDetectorConfiguration_metricTimezone :: Lens.Lens' AnomalyDetectorConfiguration (Prelude.Maybe Prelude.Text)
anomalyDetectorConfiguration_metricTimezone = Lens.lens (\AnomalyDetectorConfiguration' {metricTimezone} -> metricTimezone) (\s@AnomalyDetectorConfiguration' {} a -> s {metricTimezone = a} :: AnomalyDetectorConfiguration)

-- | An array of time ranges to exclude from use when the anomaly detection
-- model is trained. Use this to make sure that events that could cause
-- unusual values for the metric, such as deployments, aren\'t used when
-- CloudWatch creates the model.
anomalyDetectorConfiguration_excludedTimeRanges :: Lens.Lens' AnomalyDetectorConfiguration (Prelude.Maybe [Range])
anomalyDetectorConfiguration_excludedTimeRanges = Lens.lens (\AnomalyDetectorConfiguration' {excludedTimeRanges} -> excludedTimeRanges) (\s@AnomalyDetectorConfiguration' {} a -> s {excludedTimeRanges = a} :: AnomalyDetectorConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML AnomalyDetectorConfiguration where
  parseXML x =
    AnomalyDetectorConfiguration'
      Prelude.<$> (x Core..@? "MetricTimezone")
      Prelude.<*> ( x Core..@? "ExcludedTimeRanges"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance
  Prelude.Hashable
    AnomalyDetectorConfiguration
  where
  hashWithSalt _salt AnomalyDetectorConfiguration' {..} =
    _salt `Prelude.hashWithSalt` metricTimezone
      `Prelude.hashWithSalt` excludedTimeRanges

instance Prelude.NFData AnomalyDetectorConfiguration where
  rnf AnomalyDetectorConfiguration' {..} =
    Prelude.rnf metricTimezone
      `Prelude.seq` Prelude.rnf excludedTimeRanges

instance Core.ToQuery AnomalyDetectorConfiguration where
  toQuery AnomalyDetectorConfiguration' {..} =
    Prelude.mconcat
      [ "MetricTimezone" Core.=: metricTimezone,
        "ExcludedTimeRanges"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> excludedTimeRanges
            )
      ]
