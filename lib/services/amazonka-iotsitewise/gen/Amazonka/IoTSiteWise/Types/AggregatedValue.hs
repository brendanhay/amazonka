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
-- Module      : Amazonka.IoTSiteWise.Types.AggregatedValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AggregatedValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.Aggregates
import Amazonka.IoTSiteWise.Types.Quality
import qualified Amazonka.Prelude as Prelude

-- | Contains aggregated asset property values (for example, average,
-- minimum, and maximum).
--
-- /See:/ 'newAggregatedValue' smart constructor.
data AggregatedValue = AggregatedValue'
  { -- | The quality of the aggregated data.
    quality :: Prelude.Maybe Quality,
    -- | The date the aggregating computations occurred, in Unix epoch time.
    timestamp :: Data.POSIX,
    -- | The value of the aggregates.
    value :: Aggregates
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregatedValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quality', 'aggregatedValue_quality' - The quality of the aggregated data.
--
-- 'timestamp', 'aggregatedValue_timestamp' - The date the aggregating computations occurred, in Unix epoch time.
--
-- 'value', 'aggregatedValue_value' - The value of the aggregates.
newAggregatedValue ::
  -- | 'timestamp'
  Prelude.UTCTime ->
  -- | 'value'
  Aggregates ->
  AggregatedValue
newAggregatedValue pTimestamp_ pValue_ =
  AggregatedValue'
    { quality = Prelude.Nothing,
      timestamp = Data._Time Lens.# pTimestamp_,
      value = pValue_
    }

-- | The quality of the aggregated data.
aggregatedValue_quality :: Lens.Lens' AggregatedValue (Prelude.Maybe Quality)
aggregatedValue_quality = Lens.lens (\AggregatedValue' {quality} -> quality) (\s@AggregatedValue' {} a -> s {quality = a} :: AggregatedValue)

-- | The date the aggregating computations occurred, in Unix epoch time.
aggregatedValue_timestamp :: Lens.Lens' AggregatedValue Prelude.UTCTime
aggregatedValue_timestamp = Lens.lens (\AggregatedValue' {timestamp} -> timestamp) (\s@AggregatedValue' {} a -> s {timestamp = a} :: AggregatedValue) Prelude.. Data._Time

-- | The value of the aggregates.
aggregatedValue_value :: Lens.Lens' AggregatedValue Aggregates
aggregatedValue_value = Lens.lens (\AggregatedValue' {value} -> value) (\s@AggregatedValue' {} a -> s {value = a} :: AggregatedValue)

instance Data.FromJSON AggregatedValue where
  parseJSON =
    Data.withObject
      "AggregatedValue"
      ( \x ->
          AggregatedValue'
            Prelude.<$> (x Data..:? "quality")
            Prelude.<*> (x Data..: "timestamp")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable AggregatedValue where
  hashWithSalt _salt AggregatedValue' {..} =
    _salt
      `Prelude.hashWithSalt` quality
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` value

instance Prelude.NFData AggregatedValue where
  rnf AggregatedValue' {..} =
    Prelude.rnf quality
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf value
