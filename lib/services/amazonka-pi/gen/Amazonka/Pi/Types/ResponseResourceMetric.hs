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
-- Module      : Amazonka.Pi.Types.ResponseResourceMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.ResponseResourceMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the full name, description, and unit of a
-- metric.
--
-- /See:/ 'newResponseResourceMetric' smart constructor.
data ResponseResourceMetric = ResponseResourceMetric'
  { -- | The description of the metric.
    description :: Prelude.Maybe Prelude.Text,
    -- | The full name of the metric.
    metric :: Prelude.Maybe Prelude.Text,
    -- | The unit of the metric.
    unit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseResourceMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'responseResourceMetric_description' - The description of the metric.
--
-- 'metric', 'responseResourceMetric_metric' - The full name of the metric.
--
-- 'unit', 'responseResourceMetric_unit' - The unit of the metric.
newResponseResourceMetric ::
  ResponseResourceMetric
newResponseResourceMetric =
  ResponseResourceMetric'
    { description =
        Prelude.Nothing,
      metric = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The description of the metric.
responseResourceMetric_description :: Lens.Lens' ResponseResourceMetric (Prelude.Maybe Prelude.Text)
responseResourceMetric_description = Lens.lens (\ResponseResourceMetric' {description} -> description) (\s@ResponseResourceMetric' {} a -> s {description = a} :: ResponseResourceMetric)

-- | The full name of the metric.
responseResourceMetric_metric :: Lens.Lens' ResponseResourceMetric (Prelude.Maybe Prelude.Text)
responseResourceMetric_metric = Lens.lens (\ResponseResourceMetric' {metric} -> metric) (\s@ResponseResourceMetric' {} a -> s {metric = a} :: ResponseResourceMetric)

-- | The unit of the metric.
responseResourceMetric_unit :: Lens.Lens' ResponseResourceMetric (Prelude.Maybe Prelude.Text)
responseResourceMetric_unit = Lens.lens (\ResponseResourceMetric' {unit} -> unit) (\s@ResponseResourceMetric' {} a -> s {unit = a} :: ResponseResourceMetric)

instance Data.FromJSON ResponseResourceMetric where
  parseJSON =
    Data.withObject
      "ResponseResourceMetric"
      ( \x ->
          ResponseResourceMetric'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Metric")
            Prelude.<*> (x Data..:? "Unit")
      )

instance Prelude.Hashable ResponseResourceMetric where
  hashWithSalt _salt ResponseResourceMetric' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` unit

instance Prelude.NFData ResponseResourceMetric where
  rnf ResponseResourceMetric' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf unit
