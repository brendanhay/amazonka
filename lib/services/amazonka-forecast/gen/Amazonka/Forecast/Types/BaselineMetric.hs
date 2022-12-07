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
-- Module      : Amazonka.Forecast.Types.BaselineMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.BaselineMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An individual metric that you can use for comparison as you evaluate
-- your monitoring results.
--
-- /See:/ 'newBaselineMetric' smart constructor.
data BaselineMetric = BaselineMetric'
  { -- | The name of the metric.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value for the metric.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BaselineMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'baselineMetric_name' - The name of the metric.
--
-- 'value', 'baselineMetric_value' - The value for the metric.
newBaselineMetric ::
  BaselineMetric
newBaselineMetric =
  BaselineMetric'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the metric.
baselineMetric_name :: Lens.Lens' BaselineMetric (Prelude.Maybe Prelude.Text)
baselineMetric_name = Lens.lens (\BaselineMetric' {name} -> name) (\s@BaselineMetric' {} a -> s {name = a} :: BaselineMetric)

-- | The value for the metric.
baselineMetric_value :: Lens.Lens' BaselineMetric (Prelude.Maybe Prelude.Double)
baselineMetric_value = Lens.lens (\BaselineMetric' {value} -> value) (\s@BaselineMetric' {} a -> s {value = a} :: BaselineMetric)

instance Data.FromJSON BaselineMetric where
  parseJSON =
    Data.withObject
      "BaselineMetric"
      ( \x ->
          BaselineMetric'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable BaselineMetric where
  hashWithSalt _salt BaselineMetric' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData BaselineMetric where
  rnf BaselineMetric' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
