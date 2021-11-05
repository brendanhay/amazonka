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
-- Module      : Amazonka.ForecastQuery.Types.DataPoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ForecastQuery.Types.DataPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The forecast value for a specific date. Part of the Forecast object.
--
-- /See:/ 'newDataPoint' smart constructor.
data DataPoint = DataPoint'
  { -- | The forecast value.
    value :: Prelude.Maybe Prelude.Double,
    -- | The timestamp of the specific forecast.
    timestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'dataPoint_value' - The forecast value.
--
-- 'timestamp', 'dataPoint_timestamp' - The timestamp of the specific forecast.
newDataPoint ::
  DataPoint
newDataPoint =
  DataPoint'
    { value = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The forecast value.
dataPoint_value :: Lens.Lens' DataPoint (Prelude.Maybe Prelude.Double)
dataPoint_value = Lens.lens (\DataPoint' {value} -> value) (\s@DataPoint' {} a -> s {value = a} :: DataPoint)

-- | The timestamp of the specific forecast.
dataPoint_timestamp :: Lens.Lens' DataPoint (Prelude.Maybe Prelude.Text)
dataPoint_timestamp = Lens.lens (\DataPoint' {timestamp} -> timestamp) (\s@DataPoint' {} a -> s {timestamp = a} :: DataPoint)

instance Core.FromJSON DataPoint where
  parseJSON =
    Core.withObject
      "DataPoint"
      ( \x ->
          DataPoint'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Timestamp")
      )

instance Prelude.Hashable DataPoint

instance Prelude.NFData DataPoint
