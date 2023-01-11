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
-- Module      : Amazonka.IoTSiteWise.Types.TimeInNanos
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.TimeInNanos where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a timestamp with optional nanosecond granularity.
--
-- /See:/ 'newTimeInNanos' smart constructor.
data TimeInNanos = TimeInNanos'
  { -- | The nanosecond offset from @timeInSeconds@.
    offsetInNanos :: Prelude.Maybe Prelude.Natural,
    -- | The timestamp date, in seconds, in the Unix epoch format. Fractional
    -- nanosecond data is provided by @offsetInNanos@.
    timeInSeconds :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeInNanos' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offsetInNanos', 'timeInNanos_offsetInNanos' - The nanosecond offset from @timeInSeconds@.
--
-- 'timeInSeconds', 'timeInNanos_timeInSeconds' - The timestamp date, in seconds, in the Unix epoch format. Fractional
-- nanosecond data is provided by @offsetInNanos@.
newTimeInNanos ::
  -- | 'timeInSeconds'
  Prelude.Natural ->
  TimeInNanos
newTimeInNanos pTimeInSeconds_ =
  TimeInNanos'
    { offsetInNanos = Prelude.Nothing,
      timeInSeconds = pTimeInSeconds_
    }

-- | The nanosecond offset from @timeInSeconds@.
timeInNanos_offsetInNanos :: Lens.Lens' TimeInNanos (Prelude.Maybe Prelude.Natural)
timeInNanos_offsetInNanos = Lens.lens (\TimeInNanos' {offsetInNanos} -> offsetInNanos) (\s@TimeInNanos' {} a -> s {offsetInNanos = a} :: TimeInNanos)

-- | The timestamp date, in seconds, in the Unix epoch format. Fractional
-- nanosecond data is provided by @offsetInNanos@.
timeInNanos_timeInSeconds :: Lens.Lens' TimeInNanos Prelude.Natural
timeInNanos_timeInSeconds = Lens.lens (\TimeInNanos' {timeInSeconds} -> timeInSeconds) (\s@TimeInNanos' {} a -> s {timeInSeconds = a} :: TimeInNanos)

instance Data.FromJSON TimeInNanos where
  parseJSON =
    Data.withObject
      "TimeInNanos"
      ( \x ->
          TimeInNanos'
            Prelude.<$> (x Data..:? "offsetInNanos")
            Prelude.<*> (x Data..: "timeInSeconds")
      )

instance Prelude.Hashable TimeInNanos where
  hashWithSalt _salt TimeInNanos' {..} =
    _salt `Prelude.hashWithSalt` offsetInNanos
      `Prelude.hashWithSalt` timeInSeconds

instance Prelude.NFData TimeInNanos where
  rnf TimeInNanos' {..} =
    Prelude.rnf offsetInNanos
      `Prelude.seq` Prelude.rnf timeInSeconds

instance Data.ToJSON TimeInNanos where
  toJSON TimeInNanos' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("offsetInNanos" Data..=) Prelude.<$> offsetInNanos,
            Prelude.Just
              ("timeInSeconds" Data..= timeInSeconds)
          ]
      )
