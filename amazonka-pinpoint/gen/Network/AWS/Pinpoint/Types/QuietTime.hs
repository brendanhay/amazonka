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
-- Module      : Network.AWS.Pinpoint.Types.QuietTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.QuietTime where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the start and end times that define a time range when messages
-- aren\'t sent to endpoints.
--
-- /See:/ 'newQuietTime' smart constructor.
data QuietTime = QuietTime'
  { -- | The specific time when quiet time ends. This value has to use 24-hour
    -- notation and be in HH:MM format, where HH is the hour (with a leading
    -- zero, if applicable) and MM is the minutes. For example, use 02:30 to
    -- represent 2:30 AM, or 14:30 to represent 2:30 PM.
    end :: Core.Maybe Core.Text,
    -- | The specific time when quiet time begins. This value has to use 24-hour
    -- notation and be in HH:MM format, where HH is the hour (with a leading
    -- zero, if applicable) and MM is the minutes. For example, use 02:30 to
    -- represent 2:30 AM, or 14:30 to represent 2:30 PM.
    start :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QuietTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'quietTime_end' - The specific time when quiet time ends. This value has to use 24-hour
-- notation and be in HH:MM format, where HH is the hour (with a leading
-- zero, if applicable) and MM is the minutes. For example, use 02:30 to
-- represent 2:30 AM, or 14:30 to represent 2:30 PM.
--
-- 'start', 'quietTime_start' - The specific time when quiet time begins. This value has to use 24-hour
-- notation and be in HH:MM format, where HH is the hour (with a leading
-- zero, if applicable) and MM is the minutes. For example, use 02:30 to
-- represent 2:30 AM, or 14:30 to represent 2:30 PM.
newQuietTime ::
  QuietTime
newQuietTime =
  QuietTime'
    { end = Core.Nothing,
      start = Core.Nothing
    }

-- | The specific time when quiet time ends. This value has to use 24-hour
-- notation and be in HH:MM format, where HH is the hour (with a leading
-- zero, if applicable) and MM is the minutes. For example, use 02:30 to
-- represent 2:30 AM, or 14:30 to represent 2:30 PM.
quietTime_end :: Lens.Lens' QuietTime (Core.Maybe Core.Text)
quietTime_end = Lens.lens (\QuietTime' {end} -> end) (\s@QuietTime' {} a -> s {end = a} :: QuietTime)

-- | The specific time when quiet time begins. This value has to use 24-hour
-- notation and be in HH:MM format, where HH is the hour (with a leading
-- zero, if applicable) and MM is the minutes. For example, use 02:30 to
-- represent 2:30 AM, or 14:30 to represent 2:30 PM.
quietTime_start :: Lens.Lens' QuietTime (Core.Maybe Core.Text)
quietTime_start = Lens.lens (\QuietTime' {start} -> start) (\s@QuietTime' {} a -> s {start = a} :: QuietTime)

instance Core.FromJSON QuietTime where
  parseJSON =
    Core.withObject
      "QuietTime"
      ( \x ->
          QuietTime'
            Core.<$> (x Core..:? "End") Core.<*> (x Core..:? "Start")
      )

instance Core.Hashable QuietTime

instance Core.NFData QuietTime

instance Core.ToJSON QuietTime where
  toJSON QuietTime' {..} =
    Core.object
      ( Core.catMaybes
          [ ("End" Core..=) Core.<$> end,
            ("Start" Core..=) Core.<$> start
          ]
      )
