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
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.MetricStreamFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains the name of one of the metric namespaces that is
-- listed in a filter of a metric stream.
--
-- /See:/ 'newMetricStreamFilter' smart constructor.
data MetricStreamFilter = MetricStreamFilter'
  { -- | The name of the metric namespace in the filter.
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
-- 'namespace', 'metricStreamFilter_namespace' - The name of the metric namespace in the filter.
newMetricStreamFilter ::
  MetricStreamFilter
newMetricStreamFilter =
  MetricStreamFilter' {namespace = Prelude.Nothing}

-- | The name of the metric namespace in the filter.
metricStreamFilter_namespace :: Lens.Lens' MetricStreamFilter (Prelude.Maybe Prelude.Text)
metricStreamFilter_namespace = Lens.lens (\MetricStreamFilter' {namespace} -> namespace) (\s@MetricStreamFilter' {} a -> s {namespace = a} :: MetricStreamFilter)

instance Data.FromXML MetricStreamFilter where
  parseXML x =
    MetricStreamFilter'
      Prelude.<$> (x Data..@? "Namespace")

instance Prelude.Hashable MetricStreamFilter where
  hashWithSalt _salt MetricStreamFilter' {..} =
    _salt `Prelude.hashWithSalt` namespace

instance Prelude.NFData MetricStreamFilter where
  rnf MetricStreamFilter' {..} = Prelude.rnf namespace

instance Data.ToQuery MetricStreamFilter where
  toQuery MetricStreamFilter' {..} =
    Prelude.mconcat ["Namespace" Data.=: namespace]
