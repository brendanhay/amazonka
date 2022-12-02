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
-- Module      : Amazonka.CodeGuruProfiler.Types.Metric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.Metric where

import Amazonka.CodeGuruProfiler.Types.MetricType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the metric that the analysis used when it detected the
-- anomaly. The metric what is analyzed to create recommendations. It
-- includes the name of the frame that was analyzed and the type and thread
-- states used to derive the metric value for that frame.
--
-- /See:/ 'newMetric' smart constructor.
data Metric = Metric'
  { -- | The name of the method that appears as a frame in any stack in a
    -- profile.
    frameName :: Prelude.Text,
    -- | The list of application runtime thread states that is used to calculate
    -- the metric value for the frame.
    threadStates :: [Prelude.Text],
    -- | A type that specifies how a metric for a frame is analyzed. The
    -- supported value @AggregatedRelativeTotalTime@ is an aggregation of the
    -- metric value for one frame that is calculated across the occurences of
    -- all frames in a profile.
    type' :: MetricType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Metric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameName', 'metric_frameName' - The name of the method that appears as a frame in any stack in a
-- profile.
--
-- 'threadStates', 'metric_threadStates' - The list of application runtime thread states that is used to calculate
-- the metric value for the frame.
--
-- 'type'', 'metric_type' - A type that specifies how a metric for a frame is analyzed. The
-- supported value @AggregatedRelativeTotalTime@ is an aggregation of the
-- metric value for one frame that is calculated across the occurences of
-- all frames in a profile.
newMetric ::
  -- | 'frameName'
  Prelude.Text ->
  -- | 'type''
  MetricType ->
  Metric
newMetric pFrameName_ pType_ =
  Metric'
    { frameName = pFrameName_,
      threadStates = Prelude.mempty,
      type' = pType_
    }

-- | The name of the method that appears as a frame in any stack in a
-- profile.
metric_frameName :: Lens.Lens' Metric Prelude.Text
metric_frameName = Lens.lens (\Metric' {frameName} -> frameName) (\s@Metric' {} a -> s {frameName = a} :: Metric)

-- | The list of application runtime thread states that is used to calculate
-- the metric value for the frame.
metric_threadStates :: Lens.Lens' Metric [Prelude.Text]
metric_threadStates = Lens.lens (\Metric' {threadStates} -> threadStates) (\s@Metric' {} a -> s {threadStates = a} :: Metric) Prelude.. Lens.coerced

-- | A type that specifies how a metric for a frame is analyzed. The
-- supported value @AggregatedRelativeTotalTime@ is an aggregation of the
-- metric value for one frame that is calculated across the occurences of
-- all frames in a profile.
metric_type :: Lens.Lens' Metric MetricType
metric_type = Lens.lens (\Metric' {type'} -> type') (\s@Metric' {} a -> s {type' = a} :: Metric)

instance Data.FromJSON Metric where
  parseJSON =
    Data.withObject
      "Metric"
      ( \x ->
          Metric'
            Prelude.<$> (x Data..: "frameName")
            Prelude.<*> (x Data..:? "threadStates" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable Metric where
  hashWithSalt _salt Metric' {..} =
    _salt `Prelude.hashWithSalt` frameName
      `Prelude.hashWithSalt` threadStates
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Metric where
  rnf Metric' {..} =
    Prelude.rnf frameName
      `Prelude.seq` Prelude.rnf threadStates
      `Prelude.seq` Prelude.rnf type'
