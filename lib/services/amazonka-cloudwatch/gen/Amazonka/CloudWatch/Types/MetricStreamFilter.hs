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
-- Module      : Amazonka.CloudWatch.Types.MetricStreamFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.MetricStreamFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains a metric namespace and optionally, a list of
-- metric names, to either include in a metric stream or exclude from a
-- metric stream.
--
-- A metric stream\'s filters can include up to 1000 total names. This
-- limit applies to the sum of namespace names and metric names in the
-- filters. For example, this could include 10 metric namespace filters
-- with 99 metrics each, or 20 namespace filters with 49 metrics specified
-- in each filter.
--
-- /See:/ 'newMetricStreamFilter' smart constructor.
data MetricStreamFilter = MetricStreamFilter'
  { -- | The names of the metrics to either include or exclude from the metric
    -- stream.
    --
    -- If you omit this parameter, all metrics in the namespace are included or
    -- excluded, depending on whether this filter is specified as an exclude
    -- filter or an include filter.
    --
    -- Each metric name can contain only ASCII printable characters (ASCII
    -- range 32 through 126). Each metric name must contain at least one
    -- non-whitespace character.
    metricNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the metric namespace for this filter.
    --
    -- The namespace can contain only ASCII printable characters (ASCII range
    -- 32 through 126). It must contain at least one non-whitespace character.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricStreamFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricNames', 'metricStreamFilter_metricNames' - The names of the metrics to either include or exclude from the metric
-- stream.
--
-- If you omit this parameter, all metrics in the namespace are included or
-- excluded, depending on whether this filter is specified as an exclude
-- filter or an include filter.
--
-- Each metric name can contain only ASCII printable characters (ASCII
-- range 32 through 126). Each metric name must contain at least one
-- non-whitespace character.
--
-- 'namespace', 'metricStreamFilter_namespace' - The name of the metric namespace for this filter.
--
-- The namespace can contain only ASCII printable characters (ASCII range
-- 32 through 126). It must contain at least one non-whitespace character.
newMetricStreamFilter ::
  MetricStreamFilter
newMetricStreamFilter =
  MetricStreamFilter'
    { metricNames = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | The names of the metrics to either include or exclude from the metric
-- stream.
--
-- If you omit this parameter, all metrics in the namespace are included or
-- excluded, depending on whether this filter is specified as an exclude
-- filter or an include filter.
--
-- Each metric name can contain only ASCII printable characters (ASCII
-- range 32 through 126). Each metric name must contain at least one
-- non-whitespace character.
metricStreamFilter_metricNames :: Lens.Lens' MetricStreamFilter (Prelude.Maybe [Prelude.Text])
metricStreamFilter_metricNames = Lens.lens (\MetricStreamFilter' {metricNames} -> metricNames) (\s@MetricStreamFilter' {} a -> s {metricNames = a} :: MetricStreamFilter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the metric namespace for this filter.
--
-- The namespace can contain only ASCII printable characters (ASCII range
-- 32 through 126). It must contain at least one non-whitespace character.
metricStreamFilter_namespace :: Lens.Lens' MetricStreamFilter (Prelude.Maybe Prelude.Text)
metricStreamFilter_namespace = Lens.lens (\MetricStreamFilter' {namespace} -> namespace) (\s@MetricStreamFilter' {} a -> s {namespace = a} :: MetricStreamFilter)

instance Data.FromXML MetricStreamFilter where
  parseXML x =
    MetricStreamFilter'
      Prelude.<$> ( x
                      Data..@? "MetricNames"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Namespace")

instance Prelude.Hashable MetricStreamFilter where
  hashWithSalt _salt MetricStreamFilter' {..} =
    _salt
      `Prelude.hashWithSalt` metricNames
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData MetricStreamFilter where
  rnf MetricStreamFilter' {..} =
    Prelude.rnf metricNames
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToQuery MetricStreamFilter where
  toQuery MetricStreamFilter' {..} =
    Prelude.mconcat
      [ "MetricNames"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> metricNames),
        "Namespace" Data.=: namespace
      ]
