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
-- Module      : Amazonka.ComputeOptimizer.Types.ECSServiceProjectedMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ECSServiceProjectedMetric where

import Amazonka.ComputeOptimizer.Types.ECSServiceMetricName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the projected metrics of an Amazon ECS service recommendation
-- option.
--
-- To determine the performance difference between your current ECS service
-- and the recommended option, compare the metric data of your service
-- against its projected metric data.
--
-- /See:/ 'newECSServiceProjectedMetric' smart constructor.
data ECSServiceProjectedMetric = ECSServiceProjectedMetric'
  { -- | The lower bound values for the projected metric.
    lowerBoundValues :: Prelude.Maybe [Prelude.Double],
    -- | The name of the projected metric.
    --
    -- The following metrics are available:
    --
    -- -   @CPU@ — The percentage of allocated compute units that are currently
    --     in use on the ECS service tasks.
    --
    -- -   @Memory@ — The percentage of memory that is currently in use on the
    --     ECS service tasks.
    name :: Prelude.Maybe ECSServiceMetricName,
    -- | The timestamps of the projected metric.
    timestamps :: Prelude.Maybe [Data.POSIX],
    -- | The upper bound values for the projected metric.
    upperBoundValues :: Prelude.Maybe [Prelude.Double]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ECSServiceProjectedMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowerBoundValues', 'eCSServiceProjectedMetric_lowerBoundValues' - The lower bound values for the projected metric.
--
-- 'name', 'eCSServiceProjectedMetric_name' - The name of the projected metric.
--
-- The following metrics are available:
--
-- -   @CPU@ — The percentage of allocated compute units that are currently
--     in use on the ECS service tasks.
--
-- -   @Memory@ — The percentage of memory that is currently in use on the
--     ECS service tasks.
--
-- 'timestamps', 'eCSServiceProjectedMetric_timestamps' - The timestamps of the projected metric.
--
-- 'upperBoundValues', 'eCSServiceProjectedMetric_upperBoundValues' - The upper bound values for the projected metric.
newECSServiceProjectedMetric ::
  ECSServiceProjectedMetric
newECSServiceProjectedMetric =
  ECSServiceProjectedMetric'
    { lowerBoundValues =
        Prelude.Nothing,
      name = Prelude.Nothing,
      timestamps = Prelude.Nothing,
      upperBoundValues = Prelude.Nothing
    }

-- | The lower bound values for the projected metric.
eCSServiceProjectedMetric_lowerBoundValues :: Lens.Lens' ECSServiceProjectedMetric (Prelude.Maybe [Prelude.Double])
eCSServiceProjectedMetric_lowerBoundValues = Lens.lens (\ECSServiceProjectedMetric' {lowerBoundValues} -> lowerBoundValues) (\s@ECSServiceProjectedMetric' {} a -> s {lowerBoundValues = a} :: ECSServiceProjectedMetric) Prelude.. Lens.mapping Lens.coerced

-- | The name of the projected metric.
--
-- The following metrics are available:
--
-- -   @CPU@ — The percentage of allocated compute units that are currently
--     in use on the ECS service tasks.
--
-- -   @Memory@ — The percentage of memory that is currently in use on the
--     ECS service tasks.
eCSServiceProjectedMetric_name :: Lens.Lens' ECSServiceProjectedMetric (Prelude.Maybe ECSServiceMetricName)
eCSServiceProjectedMetric_name = Lens.lens (\ECSServiceProjectedMetric' {name} -> name) (\s@ECSServiceProjectedMetric' {} a -> s {name = a} :: ECSServiceProjectedMetric)

-- | The timestamps of the projected metric.
eCSServiceProjectedMetric_timestamps :: Lens.Lens' ECSServiceProjectedMetric (Prelude.Maybe [Prelude.UTCTime])
eCSServiceProjectedMetric_timestamps = Lens.lens (\ECSServiceProjectedMetric' {timestamps} -> timestamps) (\s@ECSServiceProjectedMetric' {} a -> s {timestamps = a} :: ECSServiceProjectedMetric) Prelude.. Lens.mapping Lens.coerced

-- | The upper bound values for the projected metric.
eCSServiceProjectedMetric_upperBoundValues :: Lens.Lens' ECSServiceProjectedMetric (Prelude.Maybe [Prelude.Double])
eCSServiceProjectedMetric_upperBoundValues = Lens.lens (\ECSServiceProjectedMetric' {upperBoundValues} -> upperBoundValues) (\s@ECSServiceProjectedMetric' {} a -> s {upperBoundValues = a} :: ECSServiceProjectedMetric) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ECSServiceProjectedMetric where
  parseJSON =
    Data.withObject
      "ECSServiceProjectedMetric"
      ( \x ->
          ECSServiceProjectedMetric'
            Prelude.<$> ( x Data..:? "lowerBoundValues"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "timestamps" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "upperBoundValues"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ECSServiceProjectedMetric where
  hashWithSalt _salt ECSServiceProjectedMetric' {..} =
    _salt `Prelude.hashWithSalt` lowerBoundValues
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` timestamps
      `Prelude.hashWithSalt` upperBoundValues

instance Prelude.NFData ECSServiceProjectedMetric where
  rnf ECSServiceProjectedMetric' {..} =
    Prelude.rnf lowerBoundValues
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf timestamps
      `Prelude.seq` Prelude.rnf upperBoundValues
