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
-- Module      : Amazonka.EC2.DescribeScheduledInstanceAvailability
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds available schedules that meet the specified criteria.
--
-- You can search for an available schedule no more than 3 months in
-- advance. You must meet the minimum required duration of 1,200 hours per
-- year. For example, the minimum daily schedule is 4 hours, the minimum
-- weekly schedule is 24 hours, and the minimum monthly schedule is 100
-- hours.
--
-- After you find a schedule that meets your needs, call
-- PurchaseScheduledInstances to purchase Scheduled Instances with that
-- schedule.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeScheduledInstanceAvailability
  ( -- * Creating a Request
    DescribeScheduledInstanceAvailability (..),
    newDescribeScheduledInstanceAvailability,

    -- * Request Lenses
    describeScheduledInstanceAvailability_dryRun,
    describeScheduledInstanceAvailability_filters,
    describeScheduledInstanceAvailability_maxResults,
    describeScheduledInstanceAvailability_maxSlotDurationInHours,
    describeScheduledInstanceAvailability_minSlotDurationInHours,
    describeScheduledInstanceAvailability_nextToken,
    describeScheduledInstanceAvailability_firstSlotStartTimeRange,
    describeScheduledInstanceAvailability_recurrence,

    -- * Destructuring the Response
    DescribeScheduledInstanceAvailabilityResponse (..),
    newDescribeScheduledInstanceAvailabilityResponse,

    -- * Response Lenses
    describeScheduledInstanceAvailabilityResponse_nextToken,
    describeScheduledInstanceAvailabilityResponse_scheduledInstanceAvailabilitySet,
    describeScheduledInstanceAvailabilityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeScheduledInstanceAvailability.
--
-- /See:/ 'newDescribeScheduledInstanceAvailability' smart constructor.
data DescribeScheduledInstanceAvailability = DescribeScheduledInstanceAvailability'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    -- | The maximum number of results to return in a single call. This value can
    -- be between 5 and 300. The default value is 300. To retrieve the
    -- remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The maximum available duration, in hours. This value must be greater
    -- than @MinSlotDurationInHours@ and less than 1,720.
    maxSlotDurationInHours :: Prelude.Maybe Prelude.Int,
    -- | The minimum available duration, in hours. The minimum required duration
    -- is 1,200 hours per year. For example, the minimum daily schedule is 4
    -- hours, the minimum weekly schedule is 24 hours, and the minimum monthly
    -- schedule is 100 hours.
    minSlotDurationInHours :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time period for the first schedule to start.
    firstSlotStartTimeRange :: SlotDateTimeRangeRequest,
    -- | The schedule recurrence.
    recurrence :: ScheduledInstanceRecurrenceRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScheduledInstanceAvailability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeScheduledInstanceAvailability_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeScheduledInstanceAvailability_filters' - The filters.
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
-- 'maxResults', 'describeScheduledInstanceAvailability_maxResults' - The maximum number of results to return in a single call. This value can
-- be between 5 and 300. The default value is 300. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'maxSlotDurationInHours', 'describeScheduledInstanceAvailability_maxSlotDurationInHours' - The maximum available duration, in hours. This value must be greater
-- than @MinSlotDurationInHours@ and less than 1,720.
--
-- 'minSlotDurationInHours', 'describeScheduledInstanceAvailability_minSlotDurationInHours' - The minimum available duration, in hours. The minimum required duration
-- is 1,200 hours per year. For example, the minimum daily schedule is 4
-- hours, the minimum weekly schedule is 24 hours, and the minimum monthly
-- schedule is 100 hours.
--
-- 'nextToken', 'describeScheduledInstanceAvailability_nextToken' - The token for the next set of results.
--
-- 'firstSlotStartTimeRange', 'describeScheduledInstanceAvailability_firstSlotStartTimeRange' - The time period for the first schedule to start.
--
-- 'recurrence', 'describeScheduledInstanceAvailability_recurrence' - The schedule recurrence.
newDescribeScheduledInstanceAvailability ::
  -- | 'firstSlotStartTimeRange'
  SlotDateTimeRangeRequest ->
  -- | 'recurrence'
  ScheduledInstanceRecurrenceRequest ->
  DescribeScheduledInstanceAvailability
newDescribeScheduledInstanceAvailability
  pFirstSlotStartTimeRange_
  pRecurrence_ =
    DescribeScheduledInstanceAvailability'
      { dryRun =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        maxSlotDurationInHours =
          Prelude.Nothing,
        minSlotDurationInHours =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        firstSlotStartTimeRange =
          pFirstSlotStartTimeRange_,
        recurrence = pRecurrence_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeScheduledInstanceAvailability_dryRun :: Lens.Lens' DescribeScheduledInstanceAvailability (Prelude.Maybe Prelude.Bool)
describeScheduledInstanceAvailability_dryRun = Lens.lens (\DescribeScheduledInstanceAvailability' {dryRun} -> dryRun) (\s@DescribeScheduledInstanceAvailability' {} a -> s {dryRun = a} :: DescribeScheduledInstanceAvailability)

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
describeScheduledInstanceAvailability_filters :: Lens.Lens' DescribeScheduledInstanceAvailability (Prelude.Maybe [Filter])
describeScheduledInstanceAvailability_filters = Lens.lens (\DescribeScheduledInstanceAvailability' {filters} -> filters) (\s@DescribeScheduledInstanceAvailability' {} a -> s {filters = a} :: DescribeScheduledInstanceAvailability) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. This value can
-- be between 5 and 300. The default value is 300. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeScheduledInstanceAvailability_maxResults :: Lens.Lens' DescribeScheduledInstanceAvailability (Prelude.Maybe Prelude.Natural)
describeScheduledInstanceAvailability_maxResults = Lens.lens (\DescribeScheduledInstanceAvailability' {maxResults} -> maxResults) (\s@DescribeScheduledInstanceAvailability' {} a -> s {maxResults = a} :: DescribeScheduledInstanceAvailability)

-- | The maximum available duration, in hours. This value must be greater
-- than @MinSlotDurationInHours@ and less than 1,720.
describeScheduledInstanceAvailability_maxSlotDurationInHours :: Lens.Lens' DescribeScheduledInstanceAvailability (Prelude.Maybe Prelude.Int)
describeScheduledInstanceAvailability_maxSlotDurationInHours = Lens.lens (\DescribeScheduledInstanceAvailability' {maxSlotDurationInHours} -> maxSlotDurationInHours) (\s@DescribeScheduledInstanceAvailability' {} a -> s {maxSlotDurationInHours = a} :: DescribeScheduledInstanceAvailability)

-- | The minimum available duration, in hours. The minimum required duration
-- is 1,200 hours per year. For example, the minimum daily schedule is 4
-- hours, the minimum weekly schedule is 24 hours, and the minimum monthly
-- schedule is 100 hours.
describeScheduledInstanceAvailability_minSlotDurationInHours :: Lens.Lens' DescribeScheduledInstanceAvailability (Prelude.Maybe Prelude.Int)
describeScheduledInstanceAvailability_minSlotDurationInHours = Lens.lens (\DescribeScheduledInstanceAvailability' {minSlotDurationInHours} -> minSlotDurationInHours) (\s@DescribeScheduledInstanceAvailability' {} a -> s {minSlotDurationInHours = a} :: DescribeScheduledInstanceAvailability)

-- | The token for the next set of results.
describeScheduledInstanceAvailability_nextToken :: Lens.Lens' DescribeScheduledInstanceAvailability (Prelude.Maybe Prelude.Text)
describeScheduledInstanceAvailability_nextToken = Lens.lens (\DescribeScheduledInstanceAvailability' {nextToken} -> nextToken) (\s@DescribeScheduledInstanceAvailability' {} a -> s {nextToken = a} :: DescribeScheduledInstanceAvailability)

-- | The time period for the first schedule to start.
describeScheduledInstanceAvailability_firstSlotStartTimeRange :: Lens.Lens' DescribeScheduledInstanceAvailability SlotDateTimeRangeRequest
describeScheduledInstanceAvailability_firstSlotStartTimeRange = Lens.lens (\DescribeScheduledInstanceAvailability' {firstSlotStartTimeRange} -> firstSlotStartTimeRange) (\s@DescribeScheduledInstanceAvailability' {} a -> s {firstSlotStartTimeRange = a} :: DescribeScheduledInstanceAvailability)

-- | The schedule recurrence.
describeScheduledInstanceAvailability_recurrence :: Lens.Lens' DescribeScheduledInstanceAvailability ScheduledInstanceRecurrenceRequest
describeScheduledInstanceAvailability_recurrence = Lens.lens (\DescribeScheduledInstanceAvailability' {recurrence} -> recurrence) (\s@DescribeScheduledInstanceAvailability' {} a -> s {recurrence = a} :: DescribeScheduledInstanceAvailability)

instance
  Core.AWSPager
    DescribeScheduledInstanceAvailability
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScheduledInstanceAvailabilityResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScheduledInstanceAvailabilityResponse_scheduledInstanceAvailabilitySet
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeScheduledInstanceAvailability_nextToken
          Lens..~ rs
          Lens.^? describeScheduledInstanceAvailabilityResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeScheduledInstanceAvailability
  where
  type
    AWSResponse
      DescribeScheduledInstanceAvailability =
      DescribeScheduledInstanceAvailabilityResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeScheduledInstanceAvailabilityResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "scheduledInstanceAvailabilitySet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeScheduledInstanceAvailability
  where
  hashWithSalt
    _salt
    DescribeScheduledInstanceAvailability' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` maxSlotDurationInHours
        `Prelude.hashWithSalt` minSlotDurationInHours
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` firstSlotStartTimeRange
        `Prelude.hashWithSalt` recurrence

instance
  Prelude.NFData
    DescribeScheduledInstanceAvailability
  where
  rnf DescribeScheduledInstanceAvailability' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf maxSlotDurationInHours
      `Prelude.seq` Prelude.rnf minSlotDurationInHours
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf firstSlotStartTimeRange
      `Prelude.seq` Prelude.rnf recurrence

instance
  Data.ToHeaders
    DescribeScheduledInstanceAvailability
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeScheduledInstanceAvailability
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeScheduledInstanceAvailability
  where
  toQuery DescribeScheduledInstanceAvailability' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeScheduledInstanceAvailability" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "MaxSlotDurationInHours"
          Data.=: maxSlotDurationInHours,
        "MinSlotDurationInHours"
          Data.=: minSlotDurationInHours,
        "NextToken" Data.=: nextToken,
        "FirstSlotStartTimeRange"
          Data.=: firstSlotStartTimeRange,
        "Recurrence" Data.=: recurrence
      ]

-- | Contains the output of DescribeScheduledInstanceAvailability.
--
-- /See:/ 'newDescribeScheduledInstanceAvailabilityResponse' smart constructor.
data DescribeScheduledInstanceAvailabilityResponse = DescribeScheduledInstanceAvailabilityResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the available Scheduled Instances.
    scheduledInstanceAvailabilitySet :: Prelude.Maybe [ScheduledInstanceAvailability],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScheduledInstanceAvailabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScheduledInstanceAvailabilityResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
--
-- 'scheduledInstanceAvailabilitySet', 'describeScheduledInstanceAvailabilityResponse_scheduledInstanceAvailabilitySet' - Information about the available Scheduled Instances.
--
-- 'httpStatus', 'describeScheduledInstanceAvailabilityResponse_httpStatus' - The response's http status code.
newDescribeScheduledInstanceAvailabilityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeScheduledInstanceAvailabilityResponse
newDescribeScheduledInstanceAvailabilityResponse
  pHttpStatus_ =
    DescribeScheduledInstanceAvailabilityResponse'
      { nextToken =
          Prelude.Nothing,
        scheduledInstanceAvailabilitySet =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
describeScheduledInstanceAvailabilityResponse_nextToken :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse (Prelude.Maybe Prelude.Text)
describeScheduledInstanceAvailabilityResponse_nextToken = Lens.lens (\DescribeScheduledInstanceAvailabilityResponse' {nextToken} -> nextToken) (\s@DescribeScheduledInstanceAvailabilityResponse' {} a -> s {nextToken = a} :: DescribeScheduledInstanceAvailabilityResponse)

-- | Information about the available Scheduled Instances.
describeScheduledInstanceAvailabilityResponse_scheduledInstanceAvailabilitySet :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse (Prelude.Maybe [ScheduledInstanceAvailability])
describeScheduledInstanceAvailabilityResponse_scheduledInstanceAvailabilitySet = Lens.lens (\DescribeScheduledInstanceAvailabilityResponse' {scheduledInstanceAvailabilitySet} -> scheduledInstanceAvailabilitySet) (\s@DescribeScheduledInstanceAvailabilityResponse' {} a -> s {scheduledInstanceAvailabilitySet = a} :: DescribeScheduledInstanceAvailabilityResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeScheduledInstanceAvailabilityResponse_httpStatus :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse Prelude.Int
describeScheduledInstanceAvailabilityResponse_httpStatus = Lens.lens (\DescribeScheduledInstanceAvailabilityResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduledInstanceAvailabilityResponse' {} a -> s {httpStatus = a} :: DescribeScheduledInstanceAvailabilityResponse)

instance
  Prelude.NFData
    DescribeScheduledInstanceAvailabilityResponse
  where
  rnf
    DescribeScheduledInstanceAvailabilityResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf scheduledInstanceAvailabilitySet
        `Prelude.seq` Prelude.rnf httpStatus
