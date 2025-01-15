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
-- Module      : Amazonka.CodeGuruProfiler.Types.Anomaly
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.Anomaly where

import Amazonka.CodeGuruProfiler.Types.AnomalyInstance
import Amazonka.CodeGuruProfiler.Types.Metric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about an anomaly in a specific metric of application profile.
-- The anomaly is detected using analysis of the metric data over a period
-- of time.
--
-- /See:/ 'newAnomaly' smart constructor.
data Anomaly = Anomaly'
  { -- | A list of the instances of the detected anomalies during the requested
    -- period.
    instances :: [AnomalyInstance],
    -- | Details about the metric that the analysis used when it detected the
    -- anomaly. The metric includes the name of the frame that was analyzed
    -- with the type and thread states used to derive the metric value for that
    -- frame.
    metric :: Metric,
    -- | The reason for which metric was flagged as anomalous.
    reason :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Anomaly' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'anomaly_instances' - A list of the instances of the detected anomalies during the requested
-- period.
--
-- 'metric', 'anomaly_metric' - Details about the metric that the analysis used when it detected the
-- anomaly. The metric includes the name of the frame that was analyzed
-- with the type and thread states used to derive the metric value for that
-- frame.
--
-- 'reason', 'anomaly_reason' - The reason for which metric was flagged as anomalous.
newAnomaly ::
  -- | 'metric'
  Metric ->
  -- | 'reason'
  Prelude.Text ->
  Anomaly
newAnomaly pMetric_ pReason_ =
  Anomaly'
    { instances = Prelude.mempty,
      metric = pMetric_,
      reason = pReason_
    }

-- | A list of the instances of the detected anomalies during the requested
-- period.
anomaly_instances :: Lens.Lens' Anomaly [AnomalyInstance]
anomaly_instances = Lens.lens (\Anomaly' {instances} -> instances) (\s@Anomaly' {} a -> s {instances = a} :: Anomaly) Prelude.. Lens.coerced

-- | Details about the metric that the analysis used when it detected the
-- anomaly. The metric includes the name of the frame that was analyzed
-- with the type and thread states used to derive the metric value for that
-- frame.
anomaly_metric :: Lens.Lens' Anomaly Metric
anomaly_metric = Lens.lens (\Anomaly' {metric} -> metric) (\s@Anomaly' {} a -> s {metric = a} :: Anomaly)

-- | The reason for which metric was flagged as anomalous.
anomaly_reason :: Lens.Lens' Anomaly Prelude.Text
anomaly_reason = Lens.lens (\Anomaly' {reason} -> reason) (\s@Anomaly' {} a -> s {reason = a} :: Anomaly)

instance Data.FromJSON Anomaly where
  parseJSON =
    Data.withObject
      "Anomaly"
      ( \x ->
          Anomaly'
            Prelude.<$> (x Data..:? "instances" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "metric")
            Prelude.<*> (x Data..: "reason")
      )

instance Prelude.Hashable Anomaly where
  hashWithSalt _salt Anomaly' {..} =
    _salt
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` reason

instance Prelude.NFData Anomaly where
  rnf Anomaly' {..} =
    Prelude.rnf instances `Prelude.seq`
      Prelude.rnf metric `Prelude.seq`
        Prelude.rnf reason
