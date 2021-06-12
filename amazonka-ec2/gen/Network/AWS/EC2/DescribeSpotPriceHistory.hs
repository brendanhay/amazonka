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
-- Module      : Network.AWS.EC2.DescribeSpotPriceHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Spot price history. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-spot-instances-history.html Spot Instance pricing history>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- When you specify a start and end time, this operation returns the prices
-- of the instance types within the time range that you specified and the
-- time when the price changed. The price is valid within the time period
-- that you specified; the response merely indicates the last time that the
-- price changed.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotPriceHistory
  ( -- * Creating a Request
    DescribeSpotPriceHistory (..),
    newDescribeSpotPriceHistory,

    -- * Request Lenses
    describeSpotPriceHistory_nextToken,
    describeSpotPriceHistory_dryRun,
    describeSpotPriceHistory_maxResults,
    describeSpotPriceHistory_startTime,
    describeSpotPriceHistory_endTime,
    describeSpotPriceHistory_availabilityZone,
    describeSpotPriceHistory_productDescriptions,
    describeSpotPriceHistory_filters,
    describeSpotPriceHistory_instanceTypes,

    -- * Destructuring the Response
    DescribeSpotPriceHistoryResponse (..),
    newDescribeSpotPriceHistoryResponse,

    -- * Response Lenses
    describeSpotPriceHistoryResponse_nextToken,
    describeSpotPriceHistoryResponse_spotPriceHistory,
    describeSpotPriceHistoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotPriceHistory.
--
-- /See:/ 'newDescribeSpotPriceHistory' smart constructor.
data DescribeSpotPriceHistory = DescribeSpotPriceHistory'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 1 and 1000. The default value is 1000. To retrieve the
    -- remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Core.Maybe Core.Int,
    -- | The date and time, up to the past 90 days, from which to start
    -- retrieving the price history data, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    startTime :: Core.Maybe Core.ISO8601,
    -- | The date and time, up to the current date, from which to stop retrieving
    -- the price history data, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    endTime :: Core.Maybe Core.ISO8601,
    -- | Filters the results by the specified Availability Zone.
    availabilityZone :: Core.Maybe Core.Text,
    -- | Filters the results by the specified basic product descriptions.
    productDescriptions :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @availability-zone@ - The Availability Zone for which prices should
    --     be returned.
    --
    -- -   @instance-type@ - The type of instance (for example, @m3.medium@).
    --
    -- -   @product-description@ - The product description for the Spot price
    --     (@Linux\/UNIX@ | @Red Hat Enterprise Linux@ | @SUSE Linux@ |
    --     @Windows@ | @Linux\/UNIX (Amazon VPC)@ |
    --     @Red Hat Enterprise Linux (Amazon VPC)@ | @SUSE Linux (Amazon VPC)@
    --     | @Windows (Amazon VPC)@).
    --
    -- -   @spot-price@ - The Spot price. The value must match exactly (or use
    --     wildcards; greater than or less than comparison is not supported).
    --
    -- -   @timestamp@ - The time stamp of the Spot price history, in UTC
    --     format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). You can use
    --     wildcards (* and ?). Greater than or less than comparison is not
    --     supported.
    filters :: Core.Maybe [Filter],
    -- | Filters the results by the specified instance types.
    instanceTypes :: Core.Maybe [InstanceType]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSpotPriceHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotPriceHistory_nextToken' - The token for the next set of results.
--
-- 'dryRun', 'describeSpotPriceHistory_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeSpotPriceHistory_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'startTime', 'describeSpotPriceHistory_startTime' - The date and time, up to the past 90 days, from which to start
-- retrieving the price history data, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'endTime', 'describeSpotPriceHistory_endTime' - The date and time, up to the current date, from which to stop retrieving
-- the price history data, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'availabilityZone', 'describeSpotPriceHistory_availabilityZone' - Filters the results by the specified Availability Zone.
--
-- 'productDescriptions', 'describeSpotPriceHistory_productDescriptions' - Filters the results by the specified basic product descriptions.
--
-- 'filters', 'describeSpotPriceHistory_filters' - One or more filters.
--
-- -   @availability-zone@ - The Availability Zone for which prices should
--     be returned.
--
-- -   @instance-type@ - The type of instance (for example, @m3.medium@).
--
-- -   @product-description@ - The product description for the Spot price
--     (@Linux\/UNIX@ | @Red Hat Enterprise Linux@ | @SUSE Linux@ |
--     @Windows@ | @Linux\/UNIX (Amazon VPC)@ |
--     @Red Hat Enterprise Linux (Amazon VPC)@ | @SUSE Linux (Amazon VPC)@
--     | @Windows (Amazon VPC)@).
--
-- -   @spot-price@ - The Spot price. The value must match exactly (or use
--     wildcards; greater than or less than comparison is not supported).
--
-- -   @timestamp@ - The time stamp of the Spot price history, in UTC
--     format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). You can use
--     wildcards (* and ?). Greater than or less than comparison is not
--     supported.
--
-- 'instanceTypes', 'describeSpotPriceHistory_instanceTypes' - Filters the results by the specified instance types.
newDescribeSpotPriceHistory ::
  DescribeSpotPriceHistory
newDescribeSpotPriceHistory =
  DescribeSpotPriceHistory'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      availabilityZone = Core.Nothing,
      productDescriptions = Core.Nothing,
      filters = Core.Nothing,
      instanceTypes = Core.Nothing
    }

-- | The token for the next set of results.
describeSpotPriceHistory_nextToken :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Core.Text)
describeSpotPriceHistory_nextToken = Lens.lens (\DescribeSpotPriceHistory' {nextToken} -> nextToken) (\s@DescribeSpotPriceHistory' {} a -> s {nextToken = a} :: DescribeSpotPriceHistory)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSpotPriceHistory_dryRun :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Core.Bool)
describeSpotPriceHistory_dryRun = Lens.lens (\DescribeSpotPriceHistory' {dryRun} -> dryRun) (\s@DescribeSpotPriceHistory' {} a -> s {dryRun = a} :: DescribeSpotPriceHistory)

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeSpotPriceHistory_maxResults :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Core.Int)
describeSpotPriceHistory_maxResults = Lens.lens (\DescribeSpotPriceHistory' {maxResults} -> maxResults) (\s@DescribeSpotPriceHistory' {} a -> s {maxResults = a} :: DescribeSpotPriceHistory)

-- | The date and time, up to the past 90 days, from which to start
-- retrieving the price history data, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
describeSpotPriceHistory_startTime :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Core.UTCTime)
describeSpotPriceHistory_startTime = Lens.lens (\DescribeSpotPriceHistory' {startTime} -> startTime) (\s@DescribeSpotPriceHistory' {} a -> s {startTime = a} :: DescribeSpotPriceHistory) Core.. Lens.mapping Core._Time

-- | The date and time, up to the current date, from which to stop retrieving
-- the price history data, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
describeSpotPriceHistory_endTime :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Core.UTCTime)
describeSpotPriceHistory_endTime = Lens.lens (\DescribeSpotPriceHistory' {endTime} -> endTime) (\s@DescribeSpotPriceHistory' {} a -> s {endTime = a} :: DescribeSpotPriceHistory) Core.. Lens.mapping Core._Time

-- | Filters the results by the specified Availability Zone.
describeSpotPriceHistory_availabilityZone :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe Core.Text)
describeSpotPriceHistory_availabilityZone = Lens.lens (\DescribeSpotPriceHistory' {availabilityZone} -> availabilityZone) (\s@DescribeSpotPriceHistory' {} a -> s {availabilityZone = a} :: DescribeSpotPriceHistory)

-- | Filters the results by the specified basic product descriptions.
describeSpotPriceHistory_productDescriptions :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe [Core.Text])
describeSpotPriceHistory_productDescriptions = Lens.lens (\DescribeSpotPriceHistory' {productDescriptions} -> productDescriptions) (\s@DescribeSpotPriceHistory' {} a -> s {productDescriptions = a} :: DescribeSpotPriceHistory) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @availability-zone@ - The Availability Zone for which prices should
--     be returned.
--
-- -   @instance-type@ - The type of instance (for example, @m3.medium@).
--
-- -   @product-description@ - The product description for the Spot price
--     (@Linux\/UNIX@ | @Red Hat Enterprise Linux@ | @SUSE Linux@ |
--     @Windows@ | @Linux\/UNIX (Amazon VPC)@ |
--     @Red Hat Enterprise Linux (Amazon VPC)@ | @SUSE Linux (Amazon VPC)@
--     | @Windows (Amazon VPC)@).
--
-- -   @spot-price@ - The Spot price. The value must match exactly (or use
--     wildcards; greater than or less than comparison is not supported).
--
-- -   @timestamp@ - The time stamp of the Spot price history, in UTC
--     format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). You can use
--     wildcards (* and ?). Greater than or less than comparison is not
--     supported.
describeSpotPriceHistory_filters :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe [Filter])
describeSpotPriceHistory_filters = Lens.lens (\DescribeSpotPriceHistory' {filters} -> filters) (\s@DescribeSpotPriceHistory' {} a -> s {filters = a} :: DescribeSpotPriceHistory) Core.. Lens.mapping Lens._Coerce

-- | Filters the results by the specified instance types.
describeSpotPriceHistory_instanceTypes :: Lens.Lens' DescribeSpotPriceHistory (Core.Maybe [InstanceType])
describeSpotPriceHistory_instanceTypes = Lens.lens (\DescribeSpotPriceHistory' {instanceTypes} -> instanceTypes) (\s@DescribeSpotPriceHistory' {} a -> s {instanceTypes = a} :: DescribeSpotPriceHistory) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeSpotPriceHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSpotPriceHistoryResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSpotPriceHistoryResponse_spotPriceHistory
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSpotPriceHistory_nextToken
          Lens..~ rs
          Lens.^? describeSpotPriceHistoryResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeSpotPriceHistory where
  type
    AWSResponse DescribeSpotPriceHistory =
      DescribeSpotPriceHistoryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotPriceHistoryResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "spotPriceHistorySet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSpotPriceHistory

instance Core.NFData DescribeSpotPriceHistory

instance Core.ToHeaders DescribeSpotPriceHistory where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSpotPriceHistory where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSpotPriceHistory where
  toQuery DescribeSpotPriceHistory' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeSpotPriceHistory" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "StartTime" Core.=: startTime,
        "EndTime" Core.=: endTime,
        "AvailabilityZone" Core.=: availabilityZone,
        Core.toQuery
          ( Core.toQueryList "ProductDescription"
              Core.<$> productDescriptions
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        Core.toQuery
          ( Core.toQueryList "InstanceType"
              Core.<$> instanceTypes
          )
      ]

-- | Contains the output of DescribeSpotPriceHistory.
--
-- /See:/ 'newDescribeSpotPriceHistoryResponse' smart constructor.
data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- null or an empty string when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The historical Spot prices.
    spotPriceHistory :: Core.Maybe [SpotPrice],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSpotPriceHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotPriceHistoryResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- null or an empty string when there are no more results to return.
--
-- 'spotPriceHistory', 'describeSpotPriceHistoryResponse_spotPriceHistory' - The historical Spot prices.
--
-- 'httpStatus', 'describeSpotPriceHistoryResponse_httpStatus' - The response's http status code.
newDescribeSpotPriceHistoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSpotPriceHistoryResponse
newDescribeSpotPriceHistoryResponse pHttpStatus_ =
  DescribeSpotPriceHistoryResponse'
    { nextToken =
        Core.Nothing,
      spotPriceHistory = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to retrieve the next set of results. This value is
-- null or an empty string when there are no more results to return.
describeSpotPriceHistoryResponse_nextToken :: Lens.Lens' DescribeSpotPriceHistoryResponse (Core.Maybe Core.Text)
describeSpotPriceHistoryResponse_nextToken = Lens.lens (\DescribeSpotPriceHistoryResponse' {nextToken} -> nextToken) (\s@DescribeSpotPriceHistoryResponse' {} a -> s {nextToken = a} :: DescribeSpotPriceHistoryResponse)

-- | The historical Spot prices.
describeSpotPriceHistoryResponse_spotPriceHistory :: Lens.Lens' DescribeSpotPriceHistoryResponse (Core.Maybe [SpotPrice])
describeSpotPriceHistoryResponse_spotPriceHistory = Lens.lens (\DescribeSpotPriceHistoryResponse' {spotPriceHistory} -> spotPriceHistory) (\s@DescribeSpotPriceHistoryResponse' {} a -> s {spotPriceHistory = a} :: DescribeSpotPriceHistoryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSpotPriceHistoryResponse_httpStatus :: Lens.Lens' DescribeSpotPriceHistoryResponse Core.Int
describeSpotPriceHistoryResponse_httpStatus = Lens.lens (\DescribeSpotPriceHistoryResponse' {httpStatus} -> httpStatus) (\s@DescribeSpotPriceHistoryResponse' {} a -> s {httpStatus = a} :: DescribeSpotPriceHistoryResponse)

instance Core.NFData DescribeSpotPriceHistoryResponse
