{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DevOpsGuru.DescribeAccountOverview
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For the time range passed in, returns the number of open reactive
-- insight that were created, the number of open proactive insights that
-- were created, and the Mean Time to Recover (MTTR) for all closed
-- reactive insights.
module Amazonka.DevOpsGuru.DescribeAccountOverview
  ( -- * Creating a Request
    DescribeAccountOverview (..),
    newDescribeAccountOverview,

    -- * Request Lenses
    describeAccountOverview_toTime,
    describeAccountOverview_fromTime,

    -- * Destructuring the Response
    DescribeAccountOverviewResponse (..),
    newDescribeAccountOverviewResponse,

    -- * Response Lenses
    describeAccountOverviewResponse_httpStatus,
    describeAccountOverviewResponse_reactiveInsights,
    describeAccountOverviewResponse_proactiveInsights,
    describeAccountOverviewResponse_meanTimeToRecoverInMilliseconds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountOverview' smart constructor.
data DescribeAccountOverview = DescribeAccountOverview'
  { -- | The end of the time range passed in. The start time granularity is at
    -- the day level. The floor of the start time is used. Returned information
    -- occurred before this day. If this is not specified, then the current day
    -- is used.
    toTime :: Prelude.Maybe Core.POSIX,
    -- | The start of the time range passed in. The start time granularity is at
    -- the day level. The floor of the start time is used. Returned information
    -- occurred after this day.
    fromTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountOverview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'toTime', 'describeAccountOverview_toTime' - The end of the time range passed in. The start time granularity is at
-- the day level. The floor of the start time is used. Returned information
-- occurred before this day. If this is not specified, then the current day
-- is used.
--
-- 'fromTime', 'describeAccountOverview_fromTime' - The start of the time range passed in. The start time granularity is at
-- the day level. The floor of the start time is used. Returned information
-- occurred after this day.
newDescribeAccountOverview ::
  -- | 'fromTime'
  Prelude.UTCTime ->
  DescribeAccountOverview
newDescribeAccountOverview pFromTime_ =
  DescribeAccountOverview'
    { toTime = Prelude.Nothing,
      fromTime = Core._Time Lens.# pFromTime_
    }

-- | The end of the time range passed in. The start time granularity is at
-- the day level. The floor of the start time is used. Returned information
-- occurred before this day. If this is not specified, then the current day
-- is used.
describeAccountOverview_toTime :: Lens.Lens' DescribeAccountOverview (Prelude.Maybe Prelude.UTCTime)
describeAccountOverview_toTime = Lens.lens (\DescribeAccountOverview' {toTime} -> toTime) (\s@DescribeAccountOverview' {} a -> s {toTime = a} :: DescribeAccountOverview) Prelude.. Lens.mapping Core._Time

-- | The start of the time range passed in. The start time granularity is at
-- the day level. The floor of the start time is used. Returned information
-- occurred after this day.
describeAccountOverview_fromTime :: Lens.Lens' DescribeAccountOverview Prelude.UTCTime
describeAccountOverview_fromTime = Lens.lens (\DescribeAccountOverview' {fromTime} -> fromTime) (\s@DescribeAccountOverview' {} a -> s {fromTime = a} :: DescribeAccountOverview) Prelude.. Core._Time

instance Core.AWSRequest DescribeAccountOverview where
  type
    AWSResponse DescribeAccountOverview =
      DescribeAccountOverviewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountOverviewResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ReactiveInsights")
            Prelude.<*> (x Core..:> "ProactiveInsights")
            Prelude.<*> (x Core..:> "MeanTimeToRecoverInMilliseconds")
      )

instance Prelude.Hashable DescribeAccountOverview where
  hashWithSalt _salt DescribeAccountOverview' {..} =
    _salt `Prelude.hashWithSalt` toTime
      `Prelude.hashWithSalt` fromTime

instance Prelude.NFData DescribeAccountOverview where
  rnf DescribeAccountOverview' {..} =
    Prelude.rnf toTime
      `Prelude.seq` Prelude.rnf fromTime

instance Core.ToHeaders DescribeAccountOverview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAccountOverview where
  toJSON DescribeAccountOverview' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ToTime" Core..=) Prelude.<$> toTime,
            Prelude.Just ("FromTime" Core..= fromTime)
          ]
      )

instance Core.ToPath DescribeAccountOverview where
  toPath = Prelude.const "/accounts/overview"

instance Core.ToQuery DescribeAccountOverview where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountOverviewResponse' smart constructor.
data DescribeAccountOverviewResponse = DescribeAccountOverviewResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An integer that specifies the number of open reactive insights in your
    -- Amazon Web Services account that were created during the time range
    -- passed in.
    reactiveInsights :: Prelude.Int,
    -- | An integer that specifies the number of open proactive insights in your
    -- Amazon Web Services account that were created during the time range
    -- passed in.
    proactiveInsights :: Prelude.Int,
    -- | The Mean Time to Recover (MTTR) for all closed insights that were
    -- created during the time range passed in.
    meanTimeToRecoverInMilliseconds :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountOverviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAccountOverviewResponse_httpStatus' - The response's http status code.
--
-- 'reactiveInsights', 'describeAccountOverviewResponse_reactiveInsights' - An integer that specifies the number of open reactive insights in your
-- Amazon Web Services account that were created during the time range
-- passed in.
--
-- 'proactiveInsights', 'describeAccountOverviewResponse_proactiveInsights' - An integer that specifies the number of open proactive insights in your
-- Amazon Web Services account that were created during the time range
-- passed in.
--
-- 'meanTimeToRecoverInMilliseconds', 'describeAccountOverviewResponse_meanTimeToRecoverInMilliseconds' - The Mean Time to Recover (MTTR) for all closed insights that were
-- created during the time range passed in.
newDescribeAccountOverviewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'reactiveInsights'
  Prelude.Int ->
  -- | 'proactiveInsights'
  Prelude.Int ->
  -- | 'meanTimeToRecoverInMilliseconds'
  Prelude.Integer ->
  DescribeAccountOverviewResponse
newDescribeAccountOverviewResponse
  pHttpStatus_
  pReactiveInsights_
  pProactiveInsights_
  pMeanTimeToRecoverInMilliseconds_ =
    DescribeAccountOverviewResponse'
      { httpStatus =
          pHttpStatus_,
        reactiveInsights = pReactiveInsights_,
        proactiveInsights = pProactiveInsights_,
        meanTimeToRecoverInMilliseconds =
          pMeanTimeToRecoverInMilliseconds_
      }

-- | The response's http status code.
describeAccountOverviewResponse_httpStatus :: Lens.Lens' DescribeAccountOverviewResponse Prelude.Int
describeAccountOverviewResponse_httpStatus = Lens.lens (\DescribeAccountOverviewResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountOverviewResponse' {} a -> s {httpStatus = a} :: DescribeAccountOverviewResponse)

-- | An integer that specifies the number of open reactive insights in your
-- Amazon Web Services account that were created during the time range
-- passed in.
describeAccountOverviewResponse_reactiveInsights :: Lens.Lens' DescribeAccountOverviewResponse Prelude.Int
describeAccountOverviewResponse_reactiveInsights = Lens.lens (\DescribeAccountOverviewResponse' {reactiveInsights} -> reactiveInsights) (\s@DescribeAccountOverviewResponse' {} a -> s {reactiveInsights = a} :: DescribeAccountOverviewResponse)

-- | An integer that specifies the number of open proactive insights in your
-- Amazon Web Services account that were created during the time range
-- passed in.
describeAccountOverviewResponse_proactiveInsights :: Lens.Lens' DescribeAccountOverviewResponse Prelude.Int
describeAccountOverviewResponse_proactiveInsights = Lens.lens (\DescribeAccountOverviewResponse' {proactiveInsights} -> proactiveInsights) (\s@DescribeAccountOverviewResponse' {} a -> s {proactiveInsights = a} :: DescribeAccountOverviewResponse)

-- | The Mean Time to Recover (MTTR) for all closed insights that were
-- created during the time range passed in.
describeAccountOverviewResponse_meanTimeToRecoverInMilliseconds :: Lens.Lens' DescribeAccountOverviewResponse Prelude.Integer
describeAccountOverviewResponse_meanTimeToRecoverInMilliseconds = Lens.lens (\DescribeAccountOverviewResponse' {meanTimeToRecoverInMilliseconds} -> meanTimeToRecoverInMilliseconds) (\s@DescribeAccountOverviewResponse' {} a -> s {meanTimeToRecoverInMilliseconds = a} :: DescribeAccountOverviewResponse)

instance
  Prelude.NFData
    DescribeAccountOverviewResponse
  where
  rnf DescribeAccountOverviewResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf reactiveInsights
      `Prelude.seq` Prelude.rnf proactiveInsights
      `Prelude.seq` Prelude.rnf meanTimeToRecoverInMilliseconds
