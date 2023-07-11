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
-- Module      : Amazonka.CodeGuruProfiler.Types.FrameMetricDatum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.FrameMetricDatum where

import Amazonka.CodeGuruProfiler.Types.FrameMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a frame metric and its values.
--
-- /See:/ 'newFrameMetricDatum' smart constructor.
data FrameMetricDatum = FrameMetricDatum'
  { frameMetric :: FrameMetric,
    -- | A list of values that are associated with a frame metric.
    values :: [Prelude.Double]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameMetricDatum' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameMetric', 'frameMetricDatum_frameMetric' - Undocumented member.
--
-- 'values', 'frameMetricDatum_values' - A list of values that are associated with a frame metric.
newFrameMetricDatum ::
  -- | 'frameMetric'
  FrameMetric ->
  FrameMetricDatum
newFrameMetricDatum pFrameMetric_ =
  FrameMetricDatum'
    { frameMetric = pFrameMetric_,
      values = Prelude.mempty
    }

-- | Undocumented member.
frameMetricDatum_frameMetric :: Lens.Lens' FrameMetricDatum FrameMetric
frameMetricDatum_frameMetric = Lens.lens (\FrameMetricDatum' {frameMetric} -> frameMetric) (\s@FrameMetricDatum' {} a -> s {frameMetric = a} :: FrameMetricDatum)

-- | A list of values that are associated with a frame metric.
frameMetricDatum_values :: Lens.Lens' FrameMetricDatum [Prelude.Double]
frameMetricDatum_values = Lens.lens (\FrameMetricDatum' {values} -> values) (\s@FrameMetricDatum' {} a -> s {values = a} :: FrameMetricDatum) Prelude.. Lens.coerced

instance Data.FromJSON FrameMetricDatum where
  parseJSON =
    Data.withObject
      "FrameMetricDatum"
      ( \x ->
          FrameMetricDatum'
            Prelude.<$> (x Data..: "frameMetric")
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FrameMetricDatum where
  hashWithSalt _salt FrameMetricDatum' {..} =
    _salt
      `Prelude.hashWithSalt` frameMetric
      `Prelude.hashWithSalt` values

instance Prelude.NFData FrameMetricDatum where
  rnf FrameMetricDatum' {..} =
    Prelude.rnf frameMetric
      `Prelude.seq` Prelude.rnf values
