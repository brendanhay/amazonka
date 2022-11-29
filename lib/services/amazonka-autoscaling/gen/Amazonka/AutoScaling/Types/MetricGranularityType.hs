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
-- Module      : Amazonka.AutoScaling.Types.MetricGranularityType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.MetricGranularityType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a granularity of a metric.
--
-- /See:/ 'newMetricGranularityType' smart constructor.
data MetricGranularityType = MetricGranularityType'
  { -- | The granularity. The only valid value is @1Minute@.
    granularity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricGranularityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'granularity', 'metricGranularityType_granularity' - The granularity. The only valid value is @1Minute@.
newMetricGranularityType ::
  MetricGranularityType
newMetricGranularityType =
  MetricGranularityType'
    { granularity =
        Prelude.Nothing
    }

-- | The granularity. The only valid value is @1Minute@.
metricGranularityType_granularity :: Lens.Lens' MetricGranularityType (Prelude.Maybe Prelude.Text)
metricGranularityType_granularity = Lens.lens (\MetricGranularityType' {granularity} -> granularity) (\s@MetricGranularityType' {} a -> s {granularity = a} :: MetricGranularityType)

instance Core.FromXML MetricGranularityType where
  parseXML x =
    MetricGranularityType'
      Prelude.<$> (x Core..@? "Granularity")

instance Prelude.Hashable MetricGranularityType where
  hashWithSalt _salt MetricGranularityType' {..} =
    _salt `Prelude.hashWithSalt` granularity

instance Prelude.NFData MetricGranularityType where
  rnf MetricGranularityType' {..} =
    Prelude.rnf granularity
