{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotPriceHistory.
--
-- /See:/ 'newDescribeSpotPriceHistory' smart constructor.
data DescribeSpotPriceHistory = DescribeSpotPriceHistory'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 1 and 1000. The default value is 1000. To retrieve the
    -- remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The date and time, up to the past 90 days, from which to start
    -- retrieving the price history data, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    startTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The date and time, up to the current date, from which to stop retrieving
    -- the price history data, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    endTime :: Prelude.Maybe Prelude.ISO8601,
    -- | Filters the results by the specified Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Filters the results by the specified basic product descriptions.
    productDescriptions :: Prelude.Maybe [Prelude.Text],
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
    filters :: Prelude.Maybe [Filter],
    -- | Filters the results by the specified instance types.
    instanceTypes :: Prelude.Maybe [InstanceType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      productDescriptions = Prelude.Nothing,
      filters = Prelude.Nothing,
      instanceTypes = Prelude.Nothing
    }

-- | The token for the next set of results.
describeSpotPriceHistory_nextToken :: Lens.Lens' DescribeSpotPriceHistory (Prelude.Maybe Prelude.Text)
describeSpotPriceHistory_nextToken = Lens.lens (\DescribeSpotPriceHistory' {nextToken} -> nextToken) (\s@DescribeSpotPriceHistory' {} a -> s {nextToken = a} :: DescribeSpotPriceHistory)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSpotPriceHistory_dryRun :: Lens.Lens' DescribeSpotPriceHistory (Prelude.Maybe Prelude.Bool)
describeSpotPriceHistory_dryRun = Lens.lens (\DescribeSpotPriceHistory' {dryRun} -> dryRun) (\s@DescribeSpotPriceHistory' {} a -> s {dryRun = a} :: DescribeSpotPriceHistory)

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeSpotPriceHistory_maxResults :: Lens.Lens' DescribeSpotPriceHistory (Prelude.Maybe Prelude.Int)
describeSpotPriceHistory_maxResults = Lens.lens (\DescribeSpotPriceHistory' {maxResults} -> maxResults) (\s@DescribeSpotPriceHistory' {} a -> s {maxResults = a} :: DescribeSpotPriceHistory)

-- | The date and time, up to the past 90 days, from which to start
-- retrieving the price history data, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
describeSpotPriceHistory_startTime :: Lens.Lens' DescribeSpotPriceHistory (Prelude.Maybe Prelude.UTCTime)
describeSpotPriceHistory_startTime = Lens.lens (\DescribeSpotPriceHistory' {startTime} -> startTime) (\s@DescribeSpotPriceHistory' {} a -> s {startTime = a} :: DescribeSpotPriceHistory) Prelude.. Lens.mapping Prelude._Time

-- | The date and time, up to the current date, from which to stop retrieving
-- the price history data, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
describeSpotPriceHistory_endTime :: Lens.Lens' DescribeSpotPriceHistory (Prelude.Maybe Prelude.UTCTime)
describeSpotPriceHistory_endTime = Lens.lens (\DescribeSpotPriceHistory' {endTime} -> endTime) (\s@DescribeSpotPriceHistory' {} a -> s {endTime = a} :: DescribeSpotPriceHistory) Prelude.. Lens.mapping Prelude._Time

-- | Filters the results by the specified Availability Zone.
describeSpotPriceHistory_availabilityZone :: Lens.Lens' DescribeSpotPriceHistory (Prelude.Maybe Prelude.Text)
describeSpotPriceHistory_availabilityZone = Lens.lens (\DescribeSpotPriceHistory' {availabilityZone} -> availabilityZone) (\s@DescribeSpotPriceHistory' {} a -> s {availabilityZone = a} :: DescribeSpotPriceHistory)

-- | Filters the results by the specified basic product descriptions.
describeSpotPriceHistory_productDescriptions :: Lens.Lens' DescribeSpotPriceHistory (Prelude.Maybe [Prelude.Text])
describeSpotPriceHistory_productDescriptions = Lens.lens (\DescribeSpotPriceHistory' {productDescriptions} -> productDescriptions) (\s@DescribeSpotPriceHistory' {} a -> s {productDescriptions = a} :: DescribeSpotPriceHistory) Prelude.. Lens.mapping Prelude._Coerce

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
describeSpotPriceHistory_filters :: Lens.Lens' DescribeSpotPriceHistory (Prelude.Maybe [Filter])
describeSpotPriceHistory_filters = Lens.lens (\DescribeSpotPriceHistory' {filters} -> filters) (\s@DescribeSpotPriceHistory' {} a -> s {filters = a} :: DescribeSpotPriceHistory) Prelude.. Lens.mapping Prelude._Coerce

-- | Filters the results by the specified instance types.
describeSpotPriceHistory_instanceTypes :: Lens.Lens' DescribeSpotPriceHistory (Prelude.Maybe [InstanceType])
describeSpotPriceHistory_instanceTypes = Lens.lens (\DescribeSpotPriceHistory' {instanceTypes} -> instanceTypes) (\s@DescribeSpotPriceHistory' {} a -> s {instanceTypes = a} :: DescribeSpotPriceHistory) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager DescribeSpotPriceHistory where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeSpotPriceHistoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeSpotPriceHistoryResponse_spotPriceHistory
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeSpotPriceHistory_nextToken
          Lens..~ rs
          Lens.^? describeSpotPriceHistoryResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeSpotPriceHistory where
  type
    Rs DescribeSpotPriceHistory =
      DescribeSpotPriceHistoryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotPriceHistoryResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
            Prelude.<*> ( x Prelude..@? "spotPriceHistorySet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSpotPriceHistory

instance Prelude.NFData DescribeSpotPriceHistory

instance Prelude.ToHeaders DescribeSpotPriceHistory where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeSpotPriceHistory where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeSpotPriceHistory where
  toQuery DescribeSpotPriceHistory' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeSpotPriceHistory" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        "StartTime" Prelude.=: startTime,
        "EndTime" Prelude.=: endTime,
        "AvailabilityZone" Prelude.=: availabilityZone,
        Prelude.toQuery
          ( Prelude.toQueryList "ProductDescription"
              Prelude.<$> productDescriptions
          ),
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters),
        Prelude.toQuery
          ( Prelude.toQueryList "InstanceType"
              Prelude.<$> instanceTypes
          )
      ]

-- | Contains the output of DescribeSpotPriceHistory.
--
-- /See:/ 'newDescribeSpotPriceHistoryResponse' smart constructor.
data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- null or an empty string when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The historical Spot prices.
    spotPriceHistory :: Prelude.Maybe [SpotPrice],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeSpotPriceHistoryResponse
newDescribeSpotPriceHistoryResponse pHttpStatus_ =
  DescribeSpotPriceHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      spotPriceHistory = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to retrieve the next set of results. This value is
-- null or an empty string when there are no more results to return.
describeSpotPriceHistoryResponse_nextToken :: Lens.Lens' DescribeSpotPriceHistoryResponse (Prelude.Maybe Prelude.Text)
describeSpotPriceHistoryResponse_nextToken = Lens.lens (\DescribeSpotPriceHistoryResponse' {nextToken} -> nextToken) (\s@DescribeSpotPriceHistoryResponse' {} a -> s {nextToken = a} :: DescribeSpotPriceHistoryResponse)

-- | The historical Spot prices.
describeSpotPriceHistoryResponse_spotPriceHistory :: Lens.Lens' DescribeSpotPriceHistoryResponse (Prelude.Maybe [SpotPrice])
describeSpotPriceHistoryResponse_spotPriceHistory = Lens.lens (\DescribeSpotPriceHistoryResponse' {spotPriceHistory} -> spotPriceHistory) (\s@DescribeSpotPriceHistoryResponse' {} a -> s {spotPriceHistory = a} :: DescribeSpotPriceHistoryResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeSpotPriceHistoryResponse_httpStatus :: Lens.Lens' DescribeSpotPriceHistoryResponse Prelude.Int
describeSpotPriceHistoryResponse_httpStatus = Lens.lens (\DescribeSpotPriceHistoryResponse' {httpStatus} -> httpStatus) (\s@DescribeSpotPriceHistoryResponse' {} a -> s {httpStatus = a} :: DescribeSpotPriceHistoryResponse)

instance
  Prelude.NFData
    DescribeSpotPriceHistoryResponse
