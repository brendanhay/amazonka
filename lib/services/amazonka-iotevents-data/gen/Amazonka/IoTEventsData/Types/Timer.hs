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
-- Module      : Amazonka.IoTEventsData.Types.Timer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.Timer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current state of a timer.
--
-- /See:/ 'newTimer' smart constructor.
data Timer = Timer'
  { -- | The name of the timer.
    name :: Prelude.Text,
    -- | The expiration time for the timer.
    timestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Timer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'timer_name' - The name of the timer.
--
-- 'timestamp', 'timer_timestamp' - The expiration time for the timer.
newTimer ::
  -- | 'name'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  Timer
newTimer pName_ pTimestamp_ =
  Timer'
    { name = pName_,
      timestamp = Data._Time Lens.# pTimestamp_
    }

-- | The name of the timer.
timer_name :: Lens.Lens' Timer Prelude.Text
timer_name = Lens.lens (\Timer' {name} -> name) (\s@Timer' {} a -> s {name = a} :: Timer)

-- | The expiration time for the timer.
timer_timestamp :: Lens.Lens' Timer Prelude.UTCTime
timer_timestamp = Lens.lens (\Timer' {timestamp} -> timestamp) (\s@Timer' {} a -> s {timestamp = a} :: Timer) Prelude.. Data._Time

instance Data.FromJSON Timer where
  parseJSON =
    Data.withObject
      "Timer"
      ( \x ->
          Timer'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "timestamp")
      )

instance Prelude.Hashable Timer where
  hashWithSalt _salt Timer' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData Timer where
  rnf Timer' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf timestamp
