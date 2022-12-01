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
-- Module      : Amazonka.EC2.DescribeScheduledInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Scheduled Instances or all your Scheduled
-- Instances.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeScheduledInstances
  ( -- * Creating a Request
    DescribeScheduledInstances (..),
    newDescribeScheduledInstances,

    -- * Request Lenses
    describeScheduledInstances_nextToken,
    describeScheduledInstances_slotStartTimeRange,
    describeScheduledInstances_filters,
    describeScheduledInstances_dryRun,
    describeScheduledInstances_maxResults,
    describeScheduledInstances_scheduledInstanceIds,

    -- * Destructuring the Response
    DescribeScheduledInstancesResponse (..),
    newDescribeScheduledInstancesResponse,

    -- * Response Lenses
    describeScheduledInstancesResponse_scheduledInstanceSet,
    describeScheduledInstancesResponse_nextToken,
    describeScheduledInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeScheduledInstances.
--
-- /See:/ 'newDescribeScheduledInstances' smart constructor.
data DescribeScheduledInstances = DescribeScheduledInstances'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time period for the first schedule to start.
    slotStartTimeRange :: Prelude.Maybe SlotStartTimeRangeRequest,
    -- | The filters.
    --
    -- -   @availability-zone@ - The Availability Zone (for example,
    --     @us-west-2a@).
    --
    -- -   @instance-type@ - The instance type (for example, @c4.large@).
    --
    -- -   @network-platform@ - The network platform (@EC2-Classic@ or
    --     @EC2-VPC@).
    --
    -- -   @platform@ - The platform (@Linux\/UNIX@ or @Windows@).
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. This value can
    -- be between 5 and 300. The default value is 100. To retrieve the
    -- remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The Scheduled Instance IDs.
    scheduledInstanceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScheduledInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScheduledInstances_nextToken' - The token for the next set of results.
--
-- 'slotStartTimeRange', 'describeScheduledInstances_slotStartTimeRange' - The time period for the first schedule to start.
--
-- 'filters', 'describeScheduledInstances_filters' - The filters.
--
-- -   @availability-zone@ - The Availability Zone (for example,
--     @us-west-2a@).
--
-- -   @instance-type@ - The instance type (for example, @c4.large@).
--
-- -   @network-platform@ - The network platform (@EC2-Classic@ or
--     @EC2-VPC@).
--
-- -   @platform@ - The platform (@Linux\/UNIX@ or @Windows@).
--
-- 'dryRun', 'describeScheduledInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeScheduledInstances_maxResults' - The maximum number of results to return in a single call. This value can
-- be between 5 and 300. The default value is 100. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'scheduledInstanceIds', 'describeScheduledInstances_scheduledInstanceIds' - The Scheduled Instance IDs.
newDescribeScheduledInstances ::
  DescribeScheduledInstances
newDescribeScheduledInstances =
  DescribeScheduledInstances'
    { nextToken =
        Prelude.Nothing,
      slotStartTimeRange = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      scheduledInstanceIds = Prelude.Nothing
    }

-- | The token for the next set of results.
describeScheduledInstances_nextToken :: Lens.Lens' DescribeScheduledInstances (Prelude.Maybe Prelude.Text)
describeScheduledInstances_nextToken = Lens.lens (\DescribeScheduledInstances' {nextToken} -> nextToken) (\s@DescribeScheduledInstances' {} a -> s {nextToken = a} :: DescribeScheduledInstances)

-- | The time period for the first schedule to start.
describeScheduledInstances_slotStartTimeRange :: Lens.Lens' DescribeScheduledInstances (Prelude.Maybe SlotStartTimeRangeRequest)
describeScheduledInstances_slotStartTimeRange = Lens.lens (\DescribeScheduledInstances' {slotStartTimeRange} -> slotStartTimeRange) (\s@DescribeScheduledInstances' {} a -> s {slotStartTimeRange = a} :: DescribeScheduledInstances)

-- | The filters.
--
-- -   @availability-zone@ - The Availability Zone (for example,
--     @us-west-2a@).
--
-- -   @instance-type@ - The instance type (for example, @c4.large@).
--
-- -   @network-platform@ - The network platform (@EC2-Classic@ or
--     @EC2-VPC@).
--
-- -   @platform@ - The platform (@Linux\/UNIX@ or @Windows@).
describeScheduledInstances_filters :: Lens.Lens' DescribeScheduledInstances (Prelude.Maybe [Filter])
describeScheduledInstances_filters = Lens.lens (\DescribeScheduledInstances' {filters} -> filters) (\s@DescribeScheduledInstances' {} a -> s {filters = a} :: DescribeScheduledInstances) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeScheduledInstances_dryRun :: Lens.Lens' DescribeScheduledInstances (Prelude.Maybe Prelude.Bool)
describeScheduledInstances_dryRun = Lens.lens (\DescribeScheduledInstances' {dryRun} -> dryRun) (\s@DescribeScheduledInstances' {} a -> s {dryRun = a} :: DescribeScheduledInstances)

-- | The maximum number of results to return in a single call. This value can
-- be between 5 and 300. The default value is 100. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeScheduledInstances_maxResults :: Lens.Lens' DescribeScheduledInstances (Prelude.Maybe Prelude.Int)
describeScheduledInstances_maxResults = Lens.lens (\DescribeScheduledInstances' {maxResults} -> maxResults) (\s@DescribeScheduledInstances' {} a -> s {maxResults = a} :: DescribeScheduledInstances)

-- | The Scheduled Instance IDs.
describeScheduledInstances_scheduledInstanceIds :: Lens.Lens' DescribeScheduledInstances (Prelude.Maybe [Prelude.Text])
describeScheduledInstances_scheduledInstanceIds = Lens.lens (\DescribeScheduledInstances' {scheduledInstanceIds} -> scheduledInstanceIds) (\s@DescribeScheduledInstances' {} a -> s {scheduledInstanceIds = a} :: DescribeScheduledInstances) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeScheduledInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScheduledInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScheduledInstancesResponse_scheduledInstanceSet
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeScheduledInstances_nextToken
          Lens..~ rs
          Lens.^? describeScheduledInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeScheduledInstances where
  type
    AWSResponse DescribeScheduledInstances =
      DescribeScheduledInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeScheduledInstancesResponse'
            Prelude.<$> ( x Core..@? "scheduledInstanceSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeScheduledInstances where
  hashWithSalt _salt DescribeScheduledInstances' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` slotStartTimeRange
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` scheduledInstanceIds

instance Prelude.NFData DescribeScheduledInstances where
  rnf DescribeScheduledInstances' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf slotStartTimeRange
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf scheduledInstanceIds

instance Core.ToHeaders DescribeScheduledInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeScheduledInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeScheduledInstances where
  toQuery DescribeScheduledInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeScheduledInstances" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "SlotStartTimeRange" Core.=: slotStartTimeRange,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "ScheduledInstanceId"
              Prelude.<$> scheduledInstanceIds
          )
      ]

-- | Contains the output of DescribeScheduledInstances.
--
-- /See:/ 'newDescribeScheduledInstancesResponse' smart constructor.
data DescribeScheduledInstancesResponse = DescribeScheduledInstancesResponse'
  { -- | Information about the Scheduled Instances.
    scheduledInstanceSet :: Prelude.Maybe [ScheduledInstance],
    -- | The token required to retrieve the next set of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScheduledInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledInstanceSet', 'describeScheduledInstancesResponse_scheduledInstanceSet' - Information about the Scheduled Instances.
--
-- 'nextToken', 'describeScheduledInstancesResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeScheduledInstancesResponse_httpStatus' - The response's http status code.
newDescribeScheduledInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeScheduledInstancesResponse
newDescribeScheduledInstancesResponse pHttpStatus_ =
  DescribeScheduledInstancesResponse'
    { scheduledInstanceSet =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Scheduled Instances.
describeScheduledInstancesResponse_scheduledInstanceSet :: Lens.Lens' DescribeScheduledInstancesResponse (Prelude.Maybe [ScheduledInstance])
describeScheduledInstancesResponse_scheduledInstanceSet = Lens.lens (\DescribeScheduledInstancesResponse' {scheduledInstanceSet} -> scheduledInstanceSet) (\s@DescribeScheduledInstancesResponse' {} a -> s {scheduledInstanceSet = a} :: DescribeScheduledInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
describeScheduledInstancesResponse_nextToken :: Lens.Lens' DescribeScheduledInstancesResponse (Prelude.Maybe Prelude.Text)
describeScheduledInstancesResponse_nextToken = Lens.lens (\DescribeScheduledInstancesResponse' {nextToken} -> nextToken) (\s@DescribeScheduledInstancesResponse' {} a -> s {nextToken = a} :: DescribeScheduledInstancesResponse)

-- | The response's http status code.
describeScheduledInstancesResponse_httpStatus :: Lens.Lens' DescribeScheduledInstancesResponse Prelude.Int
describeScheduledInstancesResponse_httpStatus = Lens.lens (\DescribeScheduledInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduledInstancesResponse' {} a -> s {httpStatus = a} :: DescribeScheduledInstancesResponse)

instance
  Prelude.NFData
    DescribeScheduledInstancesResponse
  where
  rnf DescribeScheduledInstancesResponse' {..} =
    Prelude.rnf scheduledInstanceSet
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
