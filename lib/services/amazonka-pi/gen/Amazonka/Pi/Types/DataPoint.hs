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
-- Module      : Amazonka.Pi.Types.DataPoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.DataPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A timestamp, and a single numerical value, which together represent a
-- measurement at a particular point in time.
--
-- /See:/ 'newDataPoint' smart constructor.
data DataPoint = DataPoint'
  { -- | The time, in epoch format, associated with a particular @Value@.
    timestamp :: Data.POSIX,
    -- | The actual value associated with a particular @Timestamp@.
    value :: Prelude.Double
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
-- 'timestamp', 'dataPoint_timestamp' - The time, in epoch format, associated with a particular @Value@.
--
-- 'value', 'dataPoint_value' - The actual value associated with a particular @Timestamp@.
newDataPoint ::
  -- | 'timestamp'
  Prelude.UTCTime ->
  -- | 'value'
  Prelude.Double ->
  DataPoint
newDataPoint pTimestamp_ pValue_ =
  DataPoint'
    { timestamp =
        Data._Time Lens.# pTimestamp_,
      value = pValue_
    }

-- | The time, in epoch format, associated with a particular @Value@.
dataPoint_timestamp :: Lens.Lens' DataPoint Prelude.UTCTime
dataPoint_timestamp = Lens.lens (\DataPoint' {timestamp} -> timestamp) (\s@DataPoint' {} a -> s {timestamp = a} :: DataPoint) Prelude.. Data._Time

-- | The actual value associated with a particular @Timestamp@.
dataPoint_value :: Lens.Lens' DataPoint Prelude.Double
dataPoint_value = Lens.lens (\DataPoint' {value} -> value) (\s@DataPoint' {} a -> s {value = a} :: DataPoint)

instance Data.FromJSON DataPoint where
  parseJSON =
    Data.withObject
      "DataPoint"
      ( \x ->
          DataPoint'
            Prelude.<$> (x Data..: "Timestamp")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable DataPoint where
  hashWithSalt _salt DataPoint' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` value

instance Prelude.NFData DataPoint where
  rnf DataPoint' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf value
