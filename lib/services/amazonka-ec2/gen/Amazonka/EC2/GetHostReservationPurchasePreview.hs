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
-- Module      : Amazonka.EC2.GetHostReservationPurchasePreview
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Preview a reservation purchase with configurations that match those of
-- your Dedicated Host. You must have active Dedicated Hosts in your
-- account before you purchase a reservation.
--
-- This is a preview of the PurchaseHostReservation action and does not
-- result in the offering being purchased.
module Amazonka.EC2.GetHostReservationPurchasePreview
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
    getHostReservationPurchasePreviewResponse_currencyCode,
    getHostReservationPurchasePreviewResponse_purchase,
    getHostReservationPurchasePreviewResponse_totalHourlyPrice,
    getHostReservationPurchasePreviewResponse_totalUpfrontPrice,
    getHostReservationPurchasePreviewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetHostReservationPurchasePreview' smart constructor.
data GetHostReservationPurchasePreview = GetHostReservationPurchasePreview'
  { -- | The IDs of the Dedicated Hosts with which the reservation is associated.
    hostIdSet :: [Prelude.Text],
    -- | The offering ID of the reservation.
    offeringId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
getHostReservationPurchasePreview_hostIdSet = Lens.lens (\GetHostReservationPurchasePreview' {hostIdSet} -> hostIdSet) (\s@GetHostReservationPurchasePreview' {} a -> s {hostIdSet = a} :: GetHostReservationPurchasePreview) Prelude.. Lens.coerced

-- | The offering ID of the reservation.
getHostReservationPurchasePreview_offeringId :: Lens.Lens' GetHostReservationPurchasePreview Prelude.Text
getHostReservationPurchasePreview_offeringId = Lens.lens (\GetHostReservationPurchasePreview' {offeringId} -> offeringId) (\s@GetHostReservationPurchasePreview' {} a -> s {offeringId = a} :: GetHostReservationPurchasePreview)

instance
  Core.AWSRequest
    GetHostReservationPurchasePreview
  where
  type
    AWSResponse GetHostReservationPurchasePreview =
      GetHostReservationPurchasePreviewResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetHostReservationPurchasePreviewResponse'
            Prelude.<$> (x Data..@? "currencyCode")
            Prelude.<*> ( x Data..@? "purchase" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "totalHourlyPrice")
            Prelude.<*> (x Data..@? "totalUpfrontPrice")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetHostReservationPurchasePreview
  where
  hashWithSalt
    _salt
    GetHostReservationPurchasePreview' {..} =
      _salt
        `Prelude.hashWithSalt` hostIdSet
        `Prelude.hashWithSalt` offeringId

instance
  Prelude.NFData
    GetHostReservationPurchasePreview
  where
  rnf GetHostReservationPurchasePreview' {..} =
    Prelude.rnf hostIdSet `Prelude.seq`
      Prelude.rnf offeringId

instance
  Data.ToHeaders
    GetHostReservationPurchasePreview
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetHostReservationPurchasePreview
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetHostReservationPurchasePreview
  where
  toQuery GetHostReservationPurchasePreview' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetHostReservationPurchasePreview" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQueryList "HostIdSet" hostIdSet,
        "OfferingId" Data.=: offeringId
      ]

-- | /See:/ 'newGetHostReservationPurchasePreviewResponse' smart constructor.
data GetHostReservationPurchasePreviewResponse = GetHostReservationPurchasePreviewResponse'
  { -- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@
    -- amounts are specified. At this time, the only supported currency is
    -- @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The purchase information of the Dedicated Host reservation and the
    -- Dedicated Hosts associated with it.
    purchase :: Prelude.Maybe [Purchase],
    -- | The potential total hourly price of the reservation per hour.
    totalHourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The potential total upfront price. This is billed immediately.
    totalUpfrontPrice :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostReservationPurchasePreviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'totalUpfrontPrice', 'getHostReservationPurchasePreviewResponse_totalUpfrontPrice' - The potential total upfront price. This is billed immediately.
--
-- 'httpStatus', 'getHostReservationPurchasePreviewResponse_httpStatus' - The response's http status code.
newGetHostReservationPurchasePreviewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetHostReservationPurchasePreviewResponse
newGetHostReservationPurchasePreviewResponse
  pHttpStatus_ =
    GetHostReservationPurchasePreviewResponse'
      { currencyCode =
          Prelude.Nothing,
        purchase = Prelude.Nothing,
        totalHourlyPrice =
          Prelude.Nothing,
        totalUpfrontPrice =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@
-- amounts are specified. At this time, the only supported currency is
-- @USD@.
getHostReservationPurchasePreviewResponse_currencyCode :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Prelude.Maybe CurrencyCodeValues)
getHostReservationPurchasePreviewResponse_currencyCode = Lens.lens (\GetHostReservationPurchasePreviewResponse' {currencyCode} -> currencyCode) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {currencyCode = a} :: GetHostReservationPurchasePreviewResponse)

-- | The purchase information of the Dedicated Host reservation and the
-- Dedicated Hosts associated with it.
getHostReservationPurchasePreviewResponse_purchase :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Prelude.Maybe [Purchase])
getHostReservationPurchasePreviewResponse_purchase = Lens.lens (\GetHostReservationPurchasePreviewResponse' {purchase} -> purchase) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {purchase = a} :: GetHostReservationPurchasePreviewResponse) Prelude.. Lens.mapping Lens.coerced

-- | The potential total hourly price of the reservation per hour.
getHostReservationPurchasePreviewResponse_totalHourlyPrice :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Prelude.Maybe Prelude.Text)
getHostReservationPurchasePreviewResponse_totalHourlyPrice = Lens.lens (\GetHostReservationPurchasePreviewResponse' {totalHourlyPrice} -> totalHourlyPrice) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {totalHourlyPrice = a} :: GetHostReservationPurchasePreviewResponse)

-- | The potential total upfront price. This is billed immediately.
getHostReservationPurchasePreviewResponse_totalUpfrontPrice :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Prelude.Maybe Prelude.Text)
getHostReservationPurchasePreviewResponse_totalUpfrontPrice = Lens.lens (\GetHostReservationPurchasePreviewResponse' {totalUpfrontPrice} -> totalUpfrontPrice) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {totalUpfrontPrice = a} :: GetHostReservationPurchasePreviewResponse)

-- | The response's http status code.
getHostReservationPurchasePreviewResponse_httpStatus :: Lens.Lens' GetHostReservationPurchasePreviewResponse Prelude.Int
getHostReservationPurchasePreviewResponse_httpStatus = Lens.lens (\GetHostReservationPurchasePreviewResponse' {httpStatus} -> httpStatus) (\s@GetHostReservationPurchasePreviewResponse' {} a -> s {httpStatus = a} :: GetHostReservationPurchasePreviewResponse)

instance
  Prelude.NFData
    GetHostReservationPurchasePreviewResponse
  where
  rnf GetHostReservationPurchasePreviewResponse' {..} =
    Prelude.rnf currencyCode `Prelude.seq`
      Prelude.rnf purchase `Prelude.seq`
        Prelude.rnf totalHourlyPrice `Prelude.seq`
          Prelude.rnf totalUpfrontPrice `Prelude.seq`
            Prelude.rnf httpStatus
