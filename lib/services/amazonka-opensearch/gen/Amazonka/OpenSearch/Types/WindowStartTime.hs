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
-- Module      : Amazonka.OpenSearch.Types.WindowStartTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.WindowStartTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The desired start time for an
-- <https://docs.aws.amazon.com/opensearch-service/latest/APIReference/API_OffPeakWindow.html off-peak maintenance window>.
--
-- /See:/ 'newWindowStartTime' smart constructor.
data WindowStartTime = WindowStartTime'
  { -- | The start hour of the window in Coordinated Universal Time (UTC), using
    -- 24-hour time. For example, @17@ refers to 5:00 P.M. UTC.
    hours :: Prelude.Natural,
    -- | The start minute of the window, in UTC.
    minutes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WindowStartTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hours', 'windowStartTime_hours' - The start hour of the window in Coordinated Universal Time (UTC), using
-- 24-hour time. For example, @17@ refers to 5:00 P.M. UTC.
--
-- 'minutes', 'windowStartTime_minutes' - The start minute of the window, in UTC.
newWindowStartTime ::
  -- | 'hours'
  Prelude.Natural ->
  -- | 'minutes'
  Prelude.Natural ->
  WindowStartTime
newWindowStartTime pHours_ pMinutes_ =
  WindowStartTime'
    { hours = pHours_,
      minutes = pMinutes_
    }

-- | The start hour of the window in Coordinated Universal Time (UTC), using
-- 24-hour time. For example, @17@ refers to 5:00 P.M. UTC.
windowStartTime_hours :: Lens.Lens' WindowStartTime Prelude.Natural
windowStartTime_hours = Lens.lens (\WindowStartTime' {hours} -> hours) (\s@WindowStartTime' {} a -> s {hours = a} :: WindowStartTime)

-- | The start minute of the window, in UTC.
windowStartTime_minutes :: Lens.Lens' WindowStartTime Prelude.Natural
windowStartTime_minutes = Lens.lens (\WindowStartTime' {minutes} -> minutes) (\s@WindowStartTime' {} a -> s {minutes = a} :: WindowStartTime)

instance Data.FromJSON WindowStartTime where
  parseJSON =
    Data.withObject
      "WindowStartTime"
      ( \x ->
          WindowStartTime'
            Prelude.<$> (x Data..: "Hours")
            Prelude.<*> (x Data..: "Minutes")
      )

instance Prelude.Hashable WindowStartTime where
  hashWithSalt _salt WindowStartTime' {..} =
    _salt
      `Prelude.hashWithSalt` hours
      `Prelude.hashWithSalt` minutes

instance Prelude.NFData WindowStartTime where
  rnf WindowStartTime' {..} =
    Prelude.rnf hours `Prelude.seq` Prelude.rnf minutes

instance Data.ToJSON WindowStartTime where
  toJSON WindowStartTime' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Hours" Data..= hours),
            Prelude.Just ("Minutes" Data..= minutes)
          ]
      )
