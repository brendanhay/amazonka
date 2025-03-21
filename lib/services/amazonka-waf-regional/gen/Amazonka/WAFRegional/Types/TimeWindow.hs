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
-- Module      : Amazonka.WAFRegional.Types.TimeWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFRegional.Types.TimeWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- In a GetSampledRequests request, the @StartTime@ and @EndTime@ objects
-- specify the time range for which you want AWS WAF to return a sample of
-- web requests.
--
-- You must specify the times in Coordinated Universal Time (UTC) format.
-- UTC format includes the special designator, @Z@. For example,
-- @\"2016-09-27T14:50Z\"@.
--
-- In a GetSampledRequests response, the @StartTime@ and @EndTime@ objects
-- specify the time range for which AWS WAF actually returned a sample of
-- web requests. AWS WAF gets the specified number of requests from among
-- the first 5,000 requests that your AWS resource receives during the
-- specified time period. If your resource receives more than 5,000
-- requests during that period, AWS WAF stops sampling after the 5,000th
-- request. In that case, @EndTime@ is the time that AWS WAF received the
-- 5,000th request.
--
-- /See:/ 'newTimeWindow' smart constructor.
data TimeWindow = TimeWindow'
  { -- | The beginning of the time range from which you want @GetSampledRequests@
    -- to return a sample of the requests that your AWS resource received. You
    -- must specify the date and time in Coordinated Universal Time (UTC)
    -- format. UTC format includes the special designator, @Z@. For example,
    -- @\"2016-09-27T14:50Z\"@. You can specify any time range in the previous
    -- three hours.
    startTime :: Data.POSIX,
    -- | The end of the time range from which you want @GetSampledRequests@ to
    -- return a sample of the requests that your AWS resource received. You
    -- must specify the date and time in Coordinated Universal Time (UTC)
    -- format. UTC format includes the special designator, @Z@. For example,
    -- @\"2016-09-27T14:50Z\"@. You can specify any time range in the previous
    -- three hours.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'timeWindow_startTime' - The beginning of the time range from which you want @GetSampledRequests@
-- to return a sample of the requests that your AWS resource received. You
-- must specify the date and time in Coordinated Universal Time (UTC)
-- format. UTC format includes the special designator, @Z@. For example,
-- @\"2016-09-27T14:50Z\"@. You can specify any time range in the previous
-- three hours.
--
-- 'endTime', 'timeWindow_endTime' - The end of the time range from which you want @GetSampledRequests@ to
-- return a sample of the requests that your AWS resource received. You
-- must specify the date and time in Coordinated Universal Time (UTC)
-- format. UTC format includes the special designator, @Z@. For example,
-- @\"2016-09-27T14:50Z\"@. You can specify any time range in the previous
-- three hours.
newTimeWindow ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  TimeWindow
newTimeWindow pStartTime_ pEndTime_ =
  TimeWindow'
    { startTime =
        Data._Time Lens.# pStartTime_,
      endTime = Data._Time Lens.# pEndTime_
    }

-- | The beginning of the time range from which you want @GetSampledRequests@
-- to return a sample of the requests that your AWS resource received. You
-- must specify the date and time in Coordinated Universal Time (UTC)
-- format. UTC format includes the special designator, @Z@. For example,
-- @\"2016-09-27T14:50Z\"@. You can specify any time range in the previous
-- three hours.
timeWindow_startTime :: Lens.Lens' TimeWindow Prelude.UTCTime
timeWindow_startTime = Lens.lens (\TimeWindow' {startTime} -> startTime) (\s@TimeWindow' {} a -> s {startTime = a} :: TimeWindow) Prelude.. Data._Time

-- | The end of the time range from which you want @GetSampledRequests@ to
-- return a sample of the requests that your AWS resource received. You
-- must specify the date and time in Coordinated Universal Time (UTC)
-- format. UTC format includes the special designator, @Z@. For example,
-- @\"2016-09-27T14:50Z\"@. You can specify any time range in the previous
-- three hours.
timeWindow_endTime :: Lens.Lens' TimeWindow Prelude.UTCTime
timeWindow_endTime = Lens.lens (\TimeWindow' {endTime} -> endTime) (\s@TimeWindow' {} a -> s {endTime = a} :: TimeWindow) Prelude.. Data._Time

instance Data.FromJSON TimeWindow where
  parseJSON =
    Data.withObject
      "TimeWindow"
      ( \x ->
          TimeWindow'
            Prelude.<$> (x Data..: "StartTime")
            Prelude.<*> (x Data..: "EndTime")
      )

instance Prelude.Hashable TimeWindow where
  hashWithSalt _salt TimeWindow' {..} =
    _salt
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData TimeWindow where
  rnf TimeWindow' {..} =
    Prelude.rnf startTime `Prelude.seq`
      Prelude.rnf endTime

instance Data.ToJSON TimeWindow where
  toJSON TimeWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime)
          ]
      )
