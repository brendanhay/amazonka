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
-- Module      : Network.AWS.EC2.PurchaseHostReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchase a reservation with configurations that match those of your
-- Dedicated Host. You must have active Dedicated Hosts in your account
-- before you purchase a reservation. This action results in the specified
-- reservation being purchased and charged to your account.
module Network.AWS.EC2.PurchaseHostReservation
  ( -- * Creating a Request
    PurchaseHostReservation (..),
    newPurchaseHostReservation,

    -- * Request Lenses
    purchaseHostReservation_tagSpecifications,
    purchaseHostReservation_currencyCode,
    purchaseHostReservation_limitPrice,
    purchaseHostReservation_clientToken,
    purchaseHostReservation_hostIdSet,
    purchaseHostReservation_offeringId,

    -- * Destructuring the Response
    PurchaseHostReservationResponse (..),
    newPurchaseHostReservationResponse,

    -- * Response Lenses
    purchaseHostReservationResponse_totalUpfrontPrice,
    purchaseHostReservationResponse_currencyCode,
    purchaseHostReservationResponse_purchase,
    purchaseHostReservationResponse_totalHourlyPrice,
    purchaseHostReservationResponse_clientToken,
    purchaseHostReservationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPurchaseHostReservation' smart constructor.
data PurchaseHostReservation = PurchaseHostReservation'
  { -- | The tags to apply to the Dedicated Host Reservation during purchase.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | The currency in which the @totalUpfrontPrice@, @LimitPrice@, and
    -- @totalHourlyPrice@ amounts are specified. At this time, the only
    -- supported currency is @USD@.
    currencyCode :: Core.Maybe CurrencyCodeValues,
    -- | The specified limit is checked against the total upfront cost of the
    -- reservation (calculated as the offering\'s upfront cost multiplied by
    -- the host count). If the total upfront cost is greater than the specified
    -- price limit, the request fails. This is used to ensure that the purchase
    -- does not exceed the expected upfront cost of the purchase. At this time,
    -- the only supported currency is @USD@. For example, to indicate a limit
    -- price of USD 100, specify 100.00.
    limitPrice :: Core.Maybe Core.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The IDs of the Dedicated Hosts with which the reservation will be
    -- associated.
    hostIdSet :: [Core.Text],
    -- | The ID of the offering.
    offeringId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PurchaseHostReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'purchaseHostReservation_tagSpecifications' - The tags to apply to the Dedicated Host Reservation during purchase.
--
-- 'currencyCode', 'purchaseHostReservation_currencyCode' - The currency in which the @totalUpfrontPrice@, @LimitPrice@, and
-- @totalHourlyPrice@ amounts are specified. At this time, the only
-- supported currency is @USD@.
--
-- 'limitPrice', 'purchaseHostReservation_limitPrice' - The specified limit is checked against the total upfront cost of the
-- reservation (calculated as the offering\'s upfront cost multiplied by
-- the host count). If the total upfront cost is greater than the specified
-- price limit, the request fails. This is used to ensure that the purchase
-- does not exceed the expected upfront cost of the purchase. At this time,
-- the only supported currency is @USD@. For example, to indicate a limit
-- price of USD 100, specify 100.00.
--
-- 'clientToken', 'purchaseHostReservation_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'hostIdSet', 'purchaseHostReservation_hostIdSet' - The IDs of the Dedicated Hosts with which the reservation will be
-- associated.
--
-- 'offeringId', 'purchaseHostReservation_offeringId' - The ID of the offering.
newPurchaseHostReservation ::
  -- | 'offeringId'
  Core.Text ->
  PurchaseHostReservation
newPurchaseHostReservation pOfferingId_ =
  PurchaseHostReservation'
    { tagSpecifications =
        Core.Nothing,
      currencyCode = Core.Nothing,
      limitPrice = Core.Nothing,
      clientToken = Core.Nothing,
      hostIdSet = Core.mempty,
      offeringId = pOfferingId_
    }

-- | The tags to apply to the Dedicated Host Reservation during purchase.
purchaseHostReservation_tagSpecifications :: Lens.Lens' PurchaseHostReservation (Core.Maybe [TagSpecification])
purchaseHostReservation_tagSpecifications = Lens.lens (\PurchaseHostReservation' {tagSpecifications} -> tagSpecifications) (\s@PurchaseHostReservation' {} a -> s {tagSpecifications = a} :: PurchaseHostReservation) Core.. Lens.mapping Lens._Coerce

-- | The currency in which the @totalUpfrontPrice@, @LimitPrice@, and
-- @totalHourlyPrice@ amounts are specified. At this time, the only
-- supported currency is @USD@.
purchaseHostReservation_currencyCode :: Lens.Lens' PurchaseHostReservation (Core.Maybe CurrencyCodeValues)
purchaseHostReservation_currencyCode = Lens.lens (\PurchaseHostReservation' {currencyCode} -> currencyCode) (\s@PurchaseHostReservation' {} a -> s {currencyCode = a} :: PurchaseHostReservation)

-- | The specified limit is checked against the total upfront cost of the
-- reservation (calculated as the offering\'s upfront cost multiplied by
-- the host count). If the total upfront cost is greater than the specified
-- price limit, the request fails. This is used to ensure that the purchase
-- does not exceed the expected upfront cost of the purchase. At this time,
-- the only supported currency is @USD@. For example, to indicate a limit
-- price of USD 100, specify 100.00.
purchaseHostReservation_limitPrice :: Lens.Lens' PurchaseHostReservation (Core.Maybe Core.Text)
purchaseHostReservation_limitPrice = Lens.lens (\PurchaseHostReservation' {limitPrice} -> limitPrice) (\s@PurchaseHostReservation' {} a -> s {limitPrice = a} :: PurchaseHostReservation)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
purchaseHostReservation_clientToken :: Lens.Lens' PurchaseHostReservation (Core.Maybe Core.Text)
purchaseHostReservation_clientToken = Lens.lens (\PurchaseHostReservation' {clientToken} -> clientToken) (\s@PurchaseHostReservation' {} a -> s {clientToken = a} :: PurchaseHostReservation)

-- | The IDs of the Dedicated Hosts with which the reservation will be
-- associated.
purchaseHostReservation_hostIdSet :: Lens.Lens' PurchaseHostReservation [Core.Text]
purchaseHostReservation_hostIdSet = Lens.lens (\PurchaseHostReservation' {hostIdSet} -> hostIdSet) (\s@PurchaseHostReservation' {} a -> s {hostIdSet = a} :: PurchaseHostReservation) Core.. Lens._Coerce

-- | The ID of the offering.
purchaseHostReservation_offeringId :: Lens.Lens' PurchaseHostReservation Core.Text
purchaseHostReservation_offeringId = Lens.lens (\PurchaseHostReservation' {offeringId} -> offeringId) (\s@PurchaseHostReservation' {} a -> s {offeringId = a} :: PurchaseHostReservation)

instance Core.AWSRequest PurchaseHostReservation where
  type
    AWSResponse PurchaseHostReservation =
      PurchaseHostReservationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          PurchaseHostReservationResponse'
            Core.<$> (x Core..@? "totalUpfrontPrice")
            Core.<*> (x Core..@? "currencyCode")
            Core.<*> ( x Core..@? "purchase" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "totalHourlyPrice")
            Core.<*> (x Core..@? "clientToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PurchaseHostReservation

instance Core.NFData PurchaseHostReservation

instance Core.ToHeaders PurchaseHostReservation where
  toHeaders = Core.const Core.mempty

instance Core.ToPath PurchaseHostReservation where
  toPath = Core.const "/"

instance Core.ToQuery PurchaseHostReservation where
  toQuery PurchaseHostReservation' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("PurchaseHostReservation" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "CurrencyCode" Core.=: currencyCode,
        "LimitPrice" Core.=: limitPrice,
        "ClientToken" Core.=: clientToken,
        Core.toQueryList "HostIdSet" hostIdSet,
        "OfferingId" Core.=: offeringId
      ]

-- | /See:/ 'newPurchaseHostReservationResponse' smart constructor.
data PurchaseHostReservationResponse = PurchaseHostReservationResponse'
  { -- | The total amount charged to your account when you purchase the
    -- reservation.
    totalUpfrontPrice :: Core.Maybe Core.Text,
    -- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@
    -- amounts are specified. At this time, the only supported currency is
    -- @USD@.
    currencyCode :: Core.Maybe CurrencyCodeValues,
    -- | Describes the details of the purchase.
    purchase :: Core.Maybe [Purchase],
    -- | The total hourly price of the reservation calculated per hour.
    totalHourlyPrice :: Core.Maybe Core.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PurchaseHostReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalUpfrontPrice', 'purchaseHostReservationResponse_totalUpfrontPrice' - The total amount charged to your account when you purchase the
-- reservation.
--
-- 'currencyCode', 'purchaseHostReservationResponse_currencyCode' - The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@
-- amounts are specified. At this time, the only supported currency is
-- @USD@.
--
-- 'purchase', 'purchaseHostReservationResponse_purchase' - Describes the details of the purchase.
--
-- 'totalHourlyPrice', 'purchaseHostReservationResponse_totalHourlyPrice' - The total hourly price of the reservation calculated per hour.
--
-- 'clientToken', 'purchaseHostReservationResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'httpStatus', 'purchaseHostReservationResponse_httpStatus' - The response's http status code.
newPurchaseHostReservationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PurchaseHostReservationResponse
newPurchaseHostReservationResponse pHttpStatus_ =
  PurchaseHostReservationResponse'
    { totalUpfrontPrice =
        Core.Nothing,
      currencyCode = Core.Nothing,
      purchase = Core.Nothing,
      totalHourlyPrice = Core.Nothing,
      clientToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total amount charged to your account when you purchase the
-- reservation.
purchaseHostReservationResponse_totalUpfrontPrice :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Core.Text)
purchaseHostReservationResponse_totalUpfrontPrice = Lens.lens (\PurchaseHostReservationResponse' {totalUpfrontPrice} -> totalUpfrontPrice) (\s@PurchaseHostReservationResponse' {} a -> s {totalUpfrontPrice = a} :: PurchaseHostReservationResponse)

-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@
-- amounts are specified. At this time, the only supported currency is
-- @USD@.
purchaseHostReservationResponse_currencyCode :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe CurrencyCodeValues)
purchaseHostReservationResponse_currencyCode = Lens.lens (\PurchaseHostReservationResponse' {currencyCode} -> currencyCode) (\s@PurchaseHostReservationResponse' {} a -> s {currencyCode = a} :: PurchaseHostReservationResponse)

-- | Describes the details of the purchase.
purchaseHostReservationResponse_purchase :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe [Purchase])
purchaseHostReservationResponse_purchase = Lens.lens (\PurchaseHostReservationResponse' {purchase} -> purchase) (\s@PurchaseHostReservationResponse' {} a -> s {purchase = a} :: PurchaseHostReservationResponse) Core.. Lens.mapping Lens._Coerce

-- | The total hourly price of the reservation calculated per hour.
purchaseHostReservationResponse_totalHourlyPrice :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Core.Text)
purchaseHostReservationResponse_totalHourlyPrice = Lens.lens (\PurchaseHostReservationResponse' {totalHourlyPrice} -> totalHourlyPrice) (\s@PurchaseHostReservationResponse' {} a -> s {totalHourlyPrice = a} :: PurchaseHostReservationResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
purchaseHostReservationResponse_clientToken :: Lens.Lens' PurchaseHostReservationResponse (Core.Maybe Core.Text)
purchaseHostReservationResponse_clientToken = Lens.lens (\PurchaseHostReservationResponse' {clientToken} -> clientToken) (\s@PurchaseHostReservationResponse' {} a -> s {clientToken = a} :: PurchaseHostReservationResponse)

-- | The response's http status code.
purchaseHostReservationResponse_httpStatus :: Lens.Lens' PurchaseHostReservationResponse Core.Int
purchaseHostReservationResponse_httpStatus = Lens.lens (\PurchaseHostReservationResponse' {httpStatus} -> httpStatus) (\s@PurchaseHostReservationResponse' {} a -> s {httpStatus = a} :: PurchaseHostReservationResponse)

instance Core.NFData PurchaseHostReservationResponse
