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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetHostReservationPurchasePreview' smart constructor.
data GetHostReservationPurchasePreview = GetHostReservationPurchasePreview'
  { -- | The IDs of the Dedicated Hosts with which the reservation is associated.
    hostIdSet :: [Prelude.Text],
    -- | The offering ID of the reservation.
    offeringId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetHostReservationPurchasePreview
newGetHostReservationPurchasePreview pOfferingId_ =
  GetHostReservationPurchasePreview'
    { hostIdSet =
        Prelude.mempty,
      offeringId = pOfferingId_
    }

-- | The IDs of the Dedicated Hosts with which the reservation is associated.
getHostReservationPurchasePreview_hostIdSet :: Lens.Lens' GetHostReservationPurchasePreview [Prelude.Text]
getHostReservationPurchasePreview_hostIdSet = Lens.lens (\GetHostReservationPurchasePreview' {hostIdSet} -> hostIdSet) (\s@GetHostReservationPurchasePreview' {} a -> s {hostIdSet = a} :: GetHostReservationPurchasePreview) Prelude.. Prelude._Coerce

-- | The offering ID of the reservation.
getHostReservationPurchasePreview_offeringId :: Lens.Lens' GetHostReservationPurchasePreview Prelude.Text
getHostReservationPurchasePreview_offeringId = Lens.lens (\GetHostReservationPurchasePreview' {offeringId} -> offeringId) (\s@GetHostReservationPurchasePreview' {} a -> s {offeringId = a} :: GetHostReservationPurchasePreview)

instance
  Prelude.AWSRequest
    GetHostReservationPurchasePreview
  where
  type
    Rs GetHostReservationPurchasePreview =
      GetHostReservationPurchasePreviewResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetHostReservationPurchasePreviewResponse'
            Prelude.<$> (x Prelude..@? "totalUpfrontPrice")
              Prelude.<*> (x Prelude..@? "currencyCode")
              Prelude.<*> ( x Prelude..@? "purchase" Prelude..!@ Prelude.mempty
                              Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                          )
              Prelude.<*> (x Prelude..@? "totalHourlyPrice")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetHostReservationPurchasePreview

instance
  Prelude.NFData
    GetHostReservationPurchasePreview

instance
  Prelude.ToHeaders
    GetHostReservationPurchasePreview
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    GetHostReservationPurchasePreview
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    GetHostReservationPurchasePreview
  where
  toQuery GetHostReservationPurchasePreview' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "GetHostReservationPurchasePreview" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQueryList "HostIdSet" hostIdSet,
        "OfferingId" Prelude.=: offeringId
      ]

-- | /See:/ 'newGetHostReservationPurchasePreviewResponse' smart constructor.
data GetHostReservationPurchasePreviewResponse = GetHostReservationPurchasePreviewResponse'
  { -- | The potential total upfront price. This is billed immediately.
    totalUpfrontPrice :: Prelude.Maybe Prelude.Text,
    -- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@
    -- amounts are specified. At this time, the only supported currency is
    -- @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The purchase information of the Dedicated Host reservation and the
    -- Dedicated Hosts associated with it.
    purchase :: Prelude.Maybe [Purchase],
    -- | The potential total hourly price of the reservation per hour.
    totalHourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetHostReservationPurchasePreviewResponse
newGetHostReservationPurchasePreviewResponse
  pHttpStatus_ =
    GetHostReservationPurchasePreviewResponse'
      { totalUpfrontPrice =
          Prelude.Nothing,
        currencyCode = Prelude.Nothing,
        purchase = Prelude.Nothing,
        totalHourlyPrice =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The potential total upfront price. This is billed immediately.
getHostReservationPurchasePreviewResponse_totalUpfrontPrice :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Prelude.Maybe Prelude.Text)
getHostReservationPurchasePreviewResponse_totalUpfrontPrice = Lens.lens (\GetHostReservationPurchasePreviewResponse' {totalUpfrontPrice} -> totalUpfrontPrice) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {totalUpfrontPrice = a} :: GetHostReservationPurchasePreviewResponse)

-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@
-- amounts are specified. At this time, the only supported currency is
-- @USD@.
getHostReservationPurchasePreviewResponse_currencyCode :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Prelude.Maybe CurrencyCodeValues)
getHostReservationPurchasePreviewResponse_currencyCode = Lens.lens (\GetHostReservationPurchasePreviewResponse' {currencyCode} -> currencyCode) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {currencyCode = a} :: GetHostReservationPurchasePreviewResponse)

-- | The purchase information of the Dedicated Host reservation and the
-- Dedicated Hosts associated with it.
getHostReservationPurchasePreviewResponse_purchase :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Prelude.Maybe [Purchase])
getHostReservationPurchasePreviewResponse_purchase = Lens.lens (\GetHostReservationPurchasePreviewResponse' {purchase} -> purchase) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {purchase = a} :: GetHostReservationPurchasePreviewResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The potential total hourly price of the reservation per hour.
getHostReservationPurchasePreviewResponse_totalHourlyPrice :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Prelude.Maybe Prelude.Text)
getHostReservationPurchasePreviewResponse_totalHourlyPrice = Lens.lens (\GetHostReservationPurchasePreviewResponse' {totalHourlyPrice} -> totalHourlyPrice) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {totalHourlyPrice = a} :: GetHostReservationPurchasePreviewResponse)

-- | The response's http status code.
getHostReservationPurchasePreviewResponse_httpStatus :: Lens.Lens' GetHostReservationPurchasePreviewResponse Prelude.Int
getHostReservationPurchasePreviewResponse_httpStatus = Lens.lens (\GetHostReservationPurchasePreviewResponse' {httpStatus} -> httpStatus) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {httpStatus = a} :: GetHostReservationPurchasePreviewResponse)

instance
  Prelude.NFData
    GetHostReservationPurchasePreviewResponse
