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
-- Module      : Amazonka.IoTSiteWise.Types.TumblingWindow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.TumblingWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains a tumbling window, which is a repeating fixed-sized,
-- non-overlapping, and contiguous time window. You can use this window in
-- metrics to aggregate data from properties and other assets.
--
-- You can use @m@, @h@, @d@, and @w@ when you specify an interval or
-- offset. Note that @m@ represents minutes, @h@ represents hours, @d@
-- represents days, and @w@ represents weeks. You can also use @s@ to
-- represent seconds in @offset@.
--
-- The @interval@ and @offset@ parameters support the
-- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format>. For example,
-- @PT5S@ represents 5 seconds, @PT5M@ represents 5 minutes, and @PT5H@
-- represents 5 hours.
--
-- /See:/ 'newTumblingWindow' smart constructor.
data TumblingWindow = TumblingWindow'
  { -- | The offset for the tumbling window. The @offset@ parameter accepts the
    -- following:
    --
    -- -   The offset time.
    --
    --     For example, if you specify @18h@ for @offset@ and @1d@ for
    --     @interval@, IoT SiteWise aggregates data in one of the following
    --     ways:
    --
    --     -   If you create the metric before or at 6 PM (UTC), you get the
    --         first aggregation result at 6 PM (UTC) on the day when you
    --         create the metric.
    --
    --     -   If you create the metric after 6 PM (UTC), you get the first
    --         aggregation result at 6 PM (UTC) the next day.
    --
    -- -   The ISO 8601 format.
    --
    --     For example, if you specify @PT18H@ for @offset@ and @1d@ for
    --     @interval@, IoT SiteWise aggregates data in one of the following
    --     ways:
    --
    --     -   If you create the metric before or at 6 PM (UTC), you get the
    --         first aggregation result at 6 PM (UTC) on the day when you
    --         create the metric.
    --
    --     -   If you create the metric after 6 PM (UTC), you get the first
    --         aggregation result at 6 PM (UTC) the next day.
    --
    -- -   The 24-hour clock.
    --
    --     For example, if you specify @00:03:00@ for @offset@, @5m@ for
    --     @interval@, and you create the metric at 2 PM (UTC), you get the
    --     first aggregation result at 2:03 PM (UTC). You get the second
    --     aggregation result at 2:08 PM (UTC).
    --
    -- -   The offset time zone.
    --
    --     For example, if you specify @2021-07-23T18:00-08@ for @offset@ and
    --     @1d@ for @interval@, IoT SiteWise aggregates data in one of the
    --     following ways:
    --
    --     -   If you create the metric before or at 6 PM (PST), you get the
    --         first aggregation result at 6 PM (PST) on the day when you
    --         create the metric.
    --
    --     -   If you create the metric after 6 PM (PST), you get the first
    --         aggregation result at 6 PM (PST) the next day.
    offset :: Prelude.Maybe Prelude.Text,
    -- | The time interval for the tumbling window. The interval time must be
    -- between 1 minute and 1 week.
    --
    -- IoT SiteWise computes the @1w@ interval the end of Sunday at midnight
    -- each week (UTC), the @1d@ interval at the end of each day at midnight
    -- (UTC), the @1h@ interval at the end of each hour, and so on.
    --
    -- When IoT SiteWise aggregates data points for metric computations, the
    -- start of each interval is exclusive and the end of each interval is
    -- inclusive. IoT SiteWise places the computed data point at the end of the
    -- interval.
    interval :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TumblingWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offset', 'tumblingWindow_offset' - The offset for the tumbling window. The @offset@ parameter accepts the
-- following:
--
-- -   The offset time.
--
--     For example, if you specify @18h@ for @offset@ and @1d@ for
--     @interval@, IoT SiteWise aggregates data in one of the following
--     ways:
--
--     -   If you create the metric before or at 6 PM (UTC), you get the
--         first aggregation result at 6 PM (UTC) on the day when you
--         create the metric.
--
--     -   If you create the metric after 6 PM (UTC), you get the first
--         aggregation result at 6 PM (UTC) the next day.
--
-- -   The ISO 8601 format.
--
--     For example, if you specify @PT18H@ for @offset@ and @1d@ for
--     @interval@, IoT SiteWise aggregates data in one of the following
--     ways:
--
--     -   If you create the metric before or at 6 PM (UTC), you get the
--         first aggregation result at 6 PM (UTC) on the day when you
--         create the metric.
--
--     -   If you create the metric after 6 PM (UTC), you get the first
--         aggregation result at 6 PM (UTC) the next day.
--
-- -   The 24-hour clock.
--
--     For example, if you specify @00:03:00@ for @offset@, @5m@ for
--     @interval@, and you create the metric at 2 PM (UTC), you get the
--     first aggregation result at 2:03 PM (UTC). You get the second
--     aggregation result at 2:08 PM (UTC).
--
-- -   The offset time zone.
--
--     For example, if you specify @2021-07-23T18:00-08@ for @offset@ and
--     @1d@ for @interval@, IoT SiteWise aggregates data in one of the
--     following ways:
--
--     -   If you create the metric before or at 6 PM (PST), you get the
--         first aggregation result at 6 PM (PST) on the day when you
--         create the metric.
--
--     -   If you create the metric after 6 PM (PST), you get the first
--         aggregation result at 6 PM (PST) the next day.
--
-- 'interval', 'tumblingWindow_interval' - The time interval for the tumbling window. The interval time must be
-- between 1 minute and 1 week.
--
-- IoT SiteWise computes the @1w@ interval the end of Sunday at midnight
-- each week (UTC), the @1d@ interval at the end of each day at midnight
-- (UTC), the @1h@ interval at the end of each hour, and so on.
--
-- When IoT SiteWise aggregates data points for metric computations, the
-- start of each interval is exclusive and the end of each interval is
-- inclusive. IoT SiteWise places the computed data point at the end of the
-- interval.
newTumblingWindow ::
  -- | 'interval'
  Prelude.Text ->
  TumblingWindow
newTumblingWindow pInterval_ =
  TumblingWindow'
    { offset = Prelude.Nothing,
      interval = pInterval_
    }

-- | The offset for the tumbling window. The @offset@ parameter accepts the
-- following:
--
-- -   The offset time.
--
--     For example, if you specify @18h@ for @offset@ and @1d@ for
--     @interval@, IoT SiteWise aggregates data in one of the following
--     ways:
--
--     -   If you create the metric before or at 6 PM (UTC), you get the
--         first aggregation result at 6 PM (UTC) on the day when you
--         create the metric.
--
--     -   If you create the metric after 6 PM (UTC), you get the first
--         aggregation result at 6 PM (UTC) the next day.
--
-- -   The ISO 8601 format.
--
--     For example, if you specify @PT18H@ for @offset@ and @1d@ for
--     @interval@, IoT SiteWise aggregates data in one of the following
--     ways:
--
--     -   If you create the metric before or at 6 PM (UTC), you get the
--         first aggregation result at 6 PM (UTC) on the day when you
--         create the metric.
--
--     -   If you create the metric after 6 PM (UTC), you get the first
--         aggregation result at 6 PM (UTC) the next day.
--
-- -   The 24-hour clock.
--
--     For example, if you specify @00:03:00@ for @offset@, @5m@ for
--     @interval@, and you create the metric at 2 PM (UTC), you get the
--     first aggregation result at 2:03 PM (UTC). You get the second
--     aggregation result at 2:08 PM (UTC).
--
-- -   The offset time zone.
--
--     For example, if you specify @2021-07-23T18:00-08@ for @offset@ and
--     @1d@ for @interval@, IoT SiteWise aggregates data in one of the
--     following ways:
--
--     -   If you create the metric before or at 6 PM (PST), you get the
--         first aggregation result at 6 PM (PST) on the day when you
--         create the metric.
--
--     -   If you create the metric after 6 PM (PST), you get the first
--         aggregation result at 6 PM (PST) the next day.
tumblingWindow_offset :: Lens.Lens' TumblingWindow (Prelude.Maybe Prelude.Text)
tumblingWindow_offset = Lens.lens (\TumblingWindow' {offset} -> offset) (\s@TumblingWindow' {} a -> s {offset = a} :: TumblingWindow)

-- | The time interval for the tumbling window. The interval time must be
-- between 1 minute and 1 week.
--
-- IoT SiteWise computes the @1w@ interval the end of Sunday at midnight
-- each week (UTC), the @1d@ interval at the end of each day at midnight
-- (UTC), the @1h@ interval at the end of each hour, and so on.
--
-- When IoT SiteWise aggregates data points for metric computations, the
-- start of each interval is exclusive and the end of each interval is
-- inclusive. IoT SiteWise places the computed data point at the end of the
-- interval.
tumblingWindow_interval :: Lens.Lens' TumblingWindow Prelude.Text
tumblingWindow_interval = Lens.lens (\TumblingWindow' {interval} -> interval) (\s@TumblingWindow' {} a -> s {interval = a} :: TumblingWindow)

instance Core.FromJSON TumblingWindow where
  parseJSON =
    Core.withObject
      "TumblingWindow"
      ( \x ->
          TumblingWindow'
            Prelude.<$> (x Core..:? "offset")
            Prelude.<*> (x Core..: "interval")
      )

instance Prelude.Hashable TumblingWindow where
  hashWithSalt _salt TumblingWindow' {..} =
    _salt `Prelude.hashWithSalt` offset
      `Prelude.hashWithSalt` interval

instance Prelude.NFData TumblingWindow where
  rnf TumblingWindow' {..} =
    Prelude.rnf offset
      `Prelude.seq` Prelude.rnf interval

instance Core.ToJSON TumblingWindow where
  toJSON TumblingWindow' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("offset" Core..=) Prelude.<$> offset,
            Prelude.Just ("interval" Core..= interval)
          ]
      )
