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
-- Module      : Network.AWS.Shield.DescribeAttackStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the number and type of attacks Shield has
-- detected in the last year for all resources that belong to your account,
-- regardless of whether you\'ve defined Shield protections for them. This
-- operation is available to Shield customers as well as to Shield Advanced
-- customers.
--
-- The operation returns data for the time range of midnight UTC, one year
-- ago, to midnight UTC, today. For example, if the current time is
-- @2020-10-26 15:39:32 PDT@, equal to @2020-10-26 22:39:32 UTC@, then the
-- time range for the attack data returned is from
-- @2019-10-26 00:00:00 UTC@ to @2020-10-26 00:00:00 UTC@.
--
-- The time range indicates the period covered by the attack statistics
-- data items.
module Network.AWS.Shield.DescribeAttackStatistics
  ( -- * Creating a Request
    DescribeAttackStatistics (..),
    newDescribeAttackStatistics,

    -- * Destructuring the Response
    DescribeAttackStatisticsResponse (..),
    newDescribeAttackStatisticsResponse,

    -- * Response Lenses
    describeAttackStatisticsResponse_httpStatus,
    describeAttackStatisticsResponse_timeRange,
    describeAttackStatisticsResponse_dataItems,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDescribeAttackStatistics' smart constructor.
data DescribeAttackStatistics = DescribeAttackStatistics'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAttackStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAttackStatistics ::
  DescribeAttackStatistics
newDescribeAttackStatistics =
  DescribeAttackStatistics'

instance Core.AWSRequest DescribeAttackStatistics where
  type
    AWSResponse DescribeAttackStatistics =
      DescribeAttackStatisticsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAttackStatisticsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "TimeRange")
            Prelude.<*> (x Core..?> "DataItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeAttackStatistics

instance Prelude.NFData DescribeAttackStatistics

instance Core.ToHeaders DescribeAttackStatistics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DescribeAttackStatistics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAttackStatistics where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DescribeAttackStatistics where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAttackStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAttackStatisticsResponse' smart constructor.
data DescribeAttackStatisticsResponse = DescribeAttackStatisticsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    timeRange :: TimeRange,
    -- | The data that describes the attacks detected during the time period.
    dataItems :: [AttackStatisticsDataItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAttackStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAttackStatisticsResponse_httpStatus' - The response's http status code.
--
-- 'timeRange', 'describeAttackStatisticsResponse_timeRange' - Undocumented member.
--
-- 'dataItems', 'describeAttackStatisticsResponse_dataItems' - The data that describes the attacks detected during the time period.
newDescribeAttackStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'timeRange'
  TimeRange ->
  DescribeAttackStatisticsResponse
newDescribeAttackStatisticsResponse
  pHttpStatus_
  pTimeRange_ =
    DescribeAttackStatisticsResponse'
      { httpStatus =
          pHttpStatus_,
        timeRange = pTimeRange_,
        dataItems = Prelude.mempty
      }

-- | The response's http status code.
describeAttackStatisticsResponse_httpStatus :: Lens.Lens' DescribeAttackStatisticsResponse Prelude.Int
describeAttackStatisticsResponse_httpStatus = Lens.lens (\DescribeAttackStatisticsResponse' {httpStatus} -> httpStatus) (\s@DescribeAttackStatisticsResponse' {} a -> s {httpStatus = a} :: DescribeAttackStatisticsResponse)

-- | Undocumented member.
describeAttackStatisticsResponse_timeRange :: Lens.Lens' DescribeAttackStatisticsResponse TimeRange
describeAttackStatisticsResponse_timeRange = Lens.lens (\DescribeAttackStatisticsResponse' {timeRange} -> timeRange) (\s@DescribeAttackStatisticsResponse' {} a -> s {timeRange = a} :: DescribeAttackStatisticsResponse)

-- | The data that describes the attacks detected during the time period.
describeAttackStatisticsResponse_dataItems :: Lens.Lens' DescribeAttackStatisticsResponse [AttackStatisticsDataItem]
describeAttackStatisticsResponse_dataItems = Lens.lens (\DescribeAttackStatisticsResponse' {dataItems} -> dataItems) (\s@DescribeAttackStatisticsResponse' {} a -> s {dataItems = a} :: DescribeAttackStatisticsResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    DescribeAttackStatisticsResponse
