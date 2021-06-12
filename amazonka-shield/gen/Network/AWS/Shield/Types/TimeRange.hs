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
-- Module      : Network.AWS.Shield.Types.TimeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.TimeRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The time range.
--
-- /See:/ 'newTimeRange' smart constructor.
data TimeRange = TimeRange'
  { -- | The start time, in Unix time in seconds. For more information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    fromInclusive :: Core.Maybe Core.POSIX,
    -- | The end time, in Unix time in seconds. For more information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    toExclusive :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromInclusive', 'timeRange_fromInclusive' - The start time, in Unix time in seconds. For more information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
--
-- 'toExclusive', 'timeRange_toExclusive' - The end time, in Unix time in seconds. For more information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
newTimeRange ::
  TimeRange
newTimeRange =
  TimeRange'
    { fromInclusive = Core.Nothing,
      toExclusive = Core.Nothing
    }

-- | The start time, in Unix time in seconds. For more information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
timeRange_fromInclusive :: Lens.Lens' TimeRange (Core.Maybe Core.UTCTime)
timeRange_fromInclusive = Lens.lens (\TimeRange' {fromInclusive} -> fromInclusive) (\s@TimeRange' {} a -> s {fromInclusive = a} :: TimeRange) Core.. Lens.mapping Core._Time

-- | The end time, in Unix time in seconds. For more information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
timeRange_toExclusive :: Lens.Lens' TimeRange (Core.Maybe Core.UTCTime)
timeRange_toExclusive = Lens.lens (\TimeRange' {toExclusive} -> toExclusive) (\s@TimeRange' {} a -> s {toExclusive = a} :: TimeRange) Core.. Lens.mapping Core._Time

instance Core.FromJSON TimeRange where
  parseJSON =
    Core.withObject
      "TimeRange"
      ( \x ->
          TimeRange'
            Core.<$> (x Core..:? "FromInclusive")
            Core.<*> (x Core..:? "ToExclusive")
      )

instance Core.Hashable TimeRange

instance Core.NFData TimeRange

instance Core.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FromInclusive" Core..=) Core.<$> fromInclusive,
            ("ToExclusive" Core..=) Core.<$> toExclusive
          ]
      )
