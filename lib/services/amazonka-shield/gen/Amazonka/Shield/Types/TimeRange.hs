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
-- Module      : Amazonka.Shield.Types.TimeRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.TimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The time range.
--
-- /See:/ 'newTimeRange' smart constructor.
data TimeRange = TimeRange'
  { -- | The end time, in Unix time in seconds.
    toExclusive :: Prelude.Maybe Core.POSIX,
    -- | The start time, in Unix time in seconds.
    fromInclusive :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'toExclusive', 'timeRange_toExclusive' - The end time, in Unix time in seconds.
--
-- 'fromInclusive', 'timeRange_fromInclusive' - The start time, in Unix time in seconds.
newTimeRange ::
  TimeRange
newTimeRange =
  TimeRange'
    { toExclusive = Prelude.Nothing,
      fromInclusive = Prelude.Nothing
    }

-- | The end time, in Unix time in seconds.
timeRange_toExclusive :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.UTCTime)
timeRange_toExclusive = Lens.lens (\TimeRange' {toExclusive} -> toExclusive) (\s@TimeRange' {} a -> s {toExclusive = a} :: TimeRange) Prelude.. Lens.mapping Core._Time

-- | The start time, in Unix time in seconds.
timeRange_fromInclusive :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.UTCTime)
timeRange_fromInclusive = Lens.lens (\TimeRange' {fromInclusive} -> fromInclusive) (\s@TimeRange' {} a -> s {fromInclusive = a} :: TimeRange) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON TimeRange where
  parseJSON =
    Core.withObject
      "TimeRange"
      ( \x ->
          TimeRange'
            Prelude.<$> (x Core..:? "ToExclusive")
            Prelude.<*> (x Core..:? "FromInclusive")
      )

instance Prelude.Hashable TimeRange where
  hashWithSalt _salt TimeRange' {..} =
    _salt `Prelude.hashWithSalt` toExclusive
      `Prelude.hashWithSalt` fromInclusive

instance Prelude.NFData TimeRange where
  rnf TimeRange' {..} =
    Prelude.rnf toExclusive
      `Prelude.seq` Prelude.rnf fromInclusive

instance Core.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ToExclusive" Core..=) Prelude.<$> toExclusive,
            ("FromInclusive" Core..=) Prelude.<$> fromInclusive
          ]
      )
