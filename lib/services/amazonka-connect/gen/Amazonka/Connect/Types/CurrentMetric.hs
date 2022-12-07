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
-- Module      : Amazonka.Connect.Types.CurrentMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.CurrentMetric where

import Amazonka.Connect.Types.CurrentMetricName
import Amazonka.Connect.Types.Unit
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a real-time metric. For a description of each
-- metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
--
-- /See:/ 'newCurrentMetric' smart constructor.
data CurrentMetric = CurrentMetric'
  { -- | The name of the metric.
    name :: Prelude.Maybe CurrentMetricName,
    -- | The unit for the metric.
    unit :: Prelude.Maybe Unit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CurrentMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'currentMetric_name' - The name of the metric.
--
-- 'unit', 'currentMetric_unit' - The unit for the metric.
newCurrentMetric ::
  CurrentMetric
newCurrentMetric =
  CurrentMetric'
    { name = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The name of the metric.
currentMetric_name :: Lens.Lens' CurrentMetric (Prelude.Maybe CurrentMetricName)
currentMetric_name = Lens.lens (\CurrentMetric' {name} -> name) (\s@CurrentMetric' {} a -> s {name = a} :: CurrentMetric)

-- | The unit for the metric.
currentMetric_unit :: Lens.Lens' CurrentMetric (Prelude.Maybe Unit)
currentMetric_unit = Lens.lens (\CurrentMetric' {unit} -> unit) (\s@CurrentMetric' {} a -> s {unit = a} :: CurrentMetric)

instance Data.FromJSON CurrentMetric where
  parseJSON =
    Data.withObject
      "CurrentMetric"
      ( \x ->
          CurrentMetric'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Unit")
      )

instance Prelude.Hashable CurrentMetric where
  hashWithSalt _salt CurrentMetric' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` unit

instance Prelude.NFData CurrentMetric where
  rnf CurrentMetric' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf unit

instance Data.ToJSON CurrentMetric where
  toJSON CurrentMetric' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Unit" Data..=) Prelude.<$> unit
          ]
      )
