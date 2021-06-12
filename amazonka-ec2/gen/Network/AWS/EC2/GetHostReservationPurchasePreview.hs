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
-- Module      : Network.AWS.EC2.GetHostReservationPurchasePreview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Preview a reservation purchase with configurations that match those of
-- your Dedicated Host. You must have active Dedicated Hosts in your
-- account before you purchase a reservation.
--
-- This is a preview of the PurchaseHostReservation action and does not
-- result in the offering being purchased.
module Network.AWS.EC2.GetHostReservationPurchasePreview
  ( -- * Creating a Request
    GetHostReservationPurchasePreview (..),
    newGetHostReservationPurchasePreview,

    -- * Request Lenses
    getHostReservationPurchasePreview_hostIdSet,
    getHostReservationPurchasePreview_offeringId,

    -- * Destructuring the Response
    GetHostReservationPurchasePreviewResponse (..),
    newGetHostReservationPurchasePreviewResponse,

    -- * Response Lenses
    getHostReservationPurchasePreviewResponse_totalUpfrontPrice,
    getHostReservationPurchasePreviewResponse_currencyCode,
    getHostReservationPurchasePreviewResponse_purchase,
    getHostReservationPurchasePreviewResponse_totalHourlyPrice,
    getHostReservationPurchasePreviewResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetHostReservationPurchasePreview' smart constructor.
data GetHostReservationPurchasePreview = GetHostReservationPurchasePreview'
  { -- | The IDs of the Dedicated Hosts with which the reservation is associated.
    hostIdSet :: [Core.Text],
    -- | The offering ID of the reservation.
    offeringId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetHostReservationPurchasePreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostIdSet', 'getHostReservationPurchasePreview_hostIdSet' - The IDs of the Dedicated Hosts with which the reservation is associated.
--
-- 'offeringId', 'getHostReservationPurchasePreview_offeringId' - The offering ID of the reservation.
newGetHostReservationPurchasePreview ::
  -- | 'offeringId'
  Core.Text ->
  GetHostReservationPurchasePreview
newGetHostReservationPurchasePreview pOfferingId_ =
  GetHostReservationPurchasePreview'
    { hostIdSet =
        Core.mempty,
      offeringId = pOfferingId_
    }

-- | The IDs of the Dedicated Hosts with which the reservation is associated.
getHostReservationPurchasePreview_hostIdSet :: Lens.Lens' GetHostReservationPurchasePreview [Core.Text]
getHostReservationPurchasePreview_hostIdSet = Lens.lens (\GetHostReservationPurchasePreview' {hostIdSet} -> hostIdSet) (\s@GetHostReservationPurchasePreview' {} a -> s {hostIdSet = a} :: GetHostReservationPurchasePreview) Core.. Lens._Coerce

-- | The offering ID of the reservation.
getHostReservationPurchasePreview_offeringId :: Lens.Lens' GetHostReservationPurchasePreview Core.Text
getHostReservationPurchasePreview_offeringId = Lens.lens (\GetHostReservationPurchasePreview' {offeringId} -> offeringId) (\s@GetHostReservationPurchasePreview' {} a -> s {offeringId = a} :: GetHostReservationPurchasePreview)

instance
  Core.AWSRequest
    GetHostReservationPurchasePreview
  where
  type
    AWSResponse GetHostReservationPurchasePreview =
      GetHostReservationPurchasePreviewResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetHostReservationPurchasePreviewResponse'
            Core.<$> (x Core..@? "totalUpfrontPrice")
            Core.<*> (x Core..@? "currencyCode")
            Core.<*> ( x Core..@? "purchase" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "totalHourlyPrice")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetHostReservationPurchasePreview

instance
  Core.NFData
    GetHostReservationPurchasePreview

instance
  Core.ToHeaders
    GetHostReservationPurchasePreview
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetHostReservationPurchasePreview
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetHostReservationPurchasePreview
  where
  toQuery GetHostReservationPurchasePreview' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "GetHostReservationPurchasePreview" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQueryList "HostIdSet" hostIdSet,
        "OfferingId" Core.=: offeringId
      ]

-- | /See:/ 'newGetHostReservationPurchasePreviewResponse' smart constructor.
data GetHostReservationPurchasePreviewResponse = GetHostReservationPurchasePreviewResponse'
  { -- | The potential total upfront price. This is billed immediately.
    totalUpfrontPrice :: Core.Maybe Core.Text,
    -- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@
    -- amounts are specified. At this time, the only supported currency is
    -- @USD@.
    currencyCode :: Core.Maybe CurrencyCodeValues,
    -- | The purchase information of the Dedicated Host reservation and the
    -- Dedicated Hosts associated with it.
    purchase :: Core.Maybe [Purchase],
    -- | The potential total hourly price of the reservation per hour.
    totalHourlyPrice :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetHostReservationPurchasePreviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalUpfrontPrice', 'getHostReservationPurchasePreviewResponse_totalUpfrontPrice' - The potential total upfront price. This is billed immediately.
--
-- 'currencyCode', 'getHostReservationPurchasePreviewResponse_currencyCode' - The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@
-- amounts are specified. At this time, the only supported currency is
-- @USD@.
--
-- 'purchase', 'getHostReservationPurchasePreviewResponse_purchase' - The purchase information of the Dedicated Host reservation and the
-- Dedicated Hosts associated with it.
--
-- 'totalHourlyPrice', 'getHostReservationPurchasePreviewResponse_totalHourlyPrice' - The potential total hourly price of the reservation per hour.
--
-- 'httpStatus', 'getHostReservationPurchasePreviewResponse_httpStatus' - The response's http status code.
newGetHostReservationPurchasePreviewResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetHostReservationPurchasePreviewResponse
newGetHostReservationPurchasePreviewResponse
  pHttpStatus_ =
    GetHostReservationPurchasePreviewResponse'
      { totalUpfrontPrice =
          Core.Nothing,
        currencyCode = Core.Nothing,
        purchase = Core.Nothing,
        totalHourlyPrice = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The potential total upfront price. This is billed immediately.
getHostReservationPurchasePreviewResponse_totalUpfrontPrice :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Core.Maybe Core.Text)
getHostReservationPurchasePreviewResponse_totalUpfrontPrice = Lens.lens (\GetHostReservationPurchasePreviewResponse' {totalUpfrontPrice} -> totalUpfrontPrice) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {totalUpfrontPrice = a} :: GetHostReservationPurchasePreviewResponse)

-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@
-- amounts are specified. At this time, the only supported currency is
-- @USD@.
getHostReservationPurchasePreviewResponse_currencyCode :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Core.Maybe CurrencyCodeValues)
getHostReservationPurchasePreviewResponse_currencyCode = Lens.lens (\GetHostReservationPurchasePreviewResponse' {currencyCode} -> currencyCode) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {currencyCode = a} :: GetHostReservationPurchasePreviewResponse)

-- | The purchase information of the Dedicated Host reservation and the
-- Dedicated Hosts associated with it.
getHostReservationPurchasePreviewResponse_purchase :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Core.Maybe [Purchase])
getHostReservationPurchasePreviewResponse_purchase = Lens.lens (\GetHostReservationPurchasePreviewResponse' {purchase} -> purchase) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {purchase = a} :: GetHostReservationPurchasePreviewResponse) Core.. Lens.mapping Lens._Coerce

-- | The potential total hourly price of the reservation per hour.
getHostReservationPurchasePreviewResponse_totalHourlyPrice :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Core.Maybe Core.Text)
getHostReservationPurchasePreviewResponse_totalHourlyPrice = Lens.lens (\GetHostReservationPurchasePreviewResponse' {totalHourlyPrice} -> totalHourlyPrice) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {totalHourlyPrice = a} :: GetHostReservationPurchasePreviewResponse)

-- | The response's http status code.
getHostReservationPurchasePreviewResponse_httpStatus :: Lens.Lens' GetHostReservationPurchasePreviewResponse Core.Int
getHostReservationPurchasePreviewResponse_httpStatus = Lens.lens (\GetHostReservationPurchasePreviewResponse' {httpStatus} -> httpStatus) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {httpStatus = a} :: GetHostReservationPurchasePreviewResponse)

instance
  Core.NFData
    GetHostReservationPurchasePreviewResponse
