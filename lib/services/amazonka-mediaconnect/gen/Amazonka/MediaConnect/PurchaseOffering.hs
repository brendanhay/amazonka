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
-- Module      : Amazonka.MediaConnect.PurchaseOffering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a request to purchase an offering. If you already have an active
-- reservation, you can\'t purchase another offering.
module Amazonka.MediaConnect.PurchaseOffering
  ( -- * Creating a Request
    PurchaseOffering (..),
    newPurchaseOffering,

    -- * Request Lenses
    purchaseOffering_offeringArn,
    purchaseOffering_start,
    purchaseOffering_reservationName,

    -- * Destructuring the Response
    PurchaseOfferingResponse (..),
    newPurchaseOfferingResponse,

    -- * Response Lenses
    purchaseOfferingResponse_reservation,
    purchaseOfferingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to purchase a offering.
--
-- /See:/ 'newPurchaseOffering' smart constructor.
data PurchaseOffering = PurchaseOffering'
  { -- | The Amazon Resource Name (ARN) of the offering.
    offeringArn :: Prelude.Text,
    -- | The date and time that you want the reservation to begin, in Coordinated
    -- Universal Time (UTC). You can specify any date and time between 12:00am
    -- on the first day of the current month to the current time on today\'s
    -- date, inclusive. Specify the start in a 24-hour notation. Use the
    -- following format: YYYY-MM-DDTHH:mm:SSZ, where T and Z are literal
    -- characters. For example, to specify 11:30pm on March 5, 2020, enter
    -- 2020-03-05T23:30:00Z.
    start :: Prelude.Text,
    -- | The name that you want to use for the reservation.
    reservationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offeringArn', 'purchaseOffering_offeringArn' - The Amazon Resource Name (ARN) of the offering.
--
-- 'start', 'purchaseOffering_start' - The date and time that you want the reservation to begin, in Coordinated
-- Universal Time (UTC). You can specify any date and time between 12:00am
-- on the first day of the current month to the current time on today\'s
-- date, inclusive. Specify the start in a 24-hour notation. Use the
-- following format: YYYY-MM-DDTHH:mm:SSZ, where T and Z are literal
-- characters. For example, to specify 11:30pm on March 5, 2020, enter
-- 2020-03-05T23:30:00Z.
--
-- 'reservationName', 'purchaseOffering_reservationName' - The name that you want to use for the reservation.
newPurchaseOffering ::
  -- | 'offeringArn'
  Prelude.Text ->
  -- | 'start'
  Prelude.Text ->
  -- | 'reservationName'
  Prelude.Text ->
  PurchaseOffering
newPurchaseOffering
  pOfferingArn_
  pStart_
  pReservationName_ =
    PurchaseOffering'
      { offeringArn = pOfferingArn_,
        start = pStart_,
        reservationName = pReservationName_
      }

-- | The Amazon Resource Name (ARN) of the offering.
purchaseOffering_offeringArn :: Lens.Lens' PurchaseOffering Prelude.Text
purchaseOffering_offeringArn = Lens.lens (\PurchaseOffering' {offeringArn} -> offeringArn) (\s@PurchaseOffering' {} a -> s {offeringArn = a} :: PurchaseOffering)

-- | The date and time that you want the reservation to begin, in Coordinated
-- Universal Time (UTC). You can specify any date and time between 12:00am
-- on the first day of the current month to the current time on today\'s
-- date, inclusive. Specify the start in a 24-hour notation. Use the
-- following format: YYYY-MM-DDTHH:mm:SSZ, where T and Z are literal
-- characters. For example, to specify 11:30pm on March 5, 2020, enter
-- 2020-03-05T23:30:00Z.
purchaseOffering_start :: Lens.Lens' PurchaseOffering Prelude.Text
purchaseOffering_start = Lens.lens (\PurchaseOffering' {start} -> start) (\s@PurchaseOffering' {} a -> s {start = a} :: PurchaseOffering)

-- | The name that you want to use for the reservation.
purchaseOffering_reservationName :: Lens.Lens' PurchaseOffering Prelude.Text
purchaseOffering_reservationName = Lens.lens (\PurchaseOffering' {reservationName} -> reservationName) (\s@PurchaseOffering' {} a -> s {reservationName = a} :: PurchaseOffering)

instance Core.AWSRequest PurchaseOffering where
  type
    AWSResponse PurchaseOffering =
      PurchaseOfferingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PurchaseOfferingResponse'
            Prelude.<$> (x Data..?> "reservation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PurchaseOffering where
  hashWithSalt _salt PurchaseOffering' {..} =
    _salt
      `Prelude.hashWithSalt` offeringArn
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` reservationName

instance Prelude.NFData PurchaseOffering where
  rnf PurchaseOffering' {..} =
    Prelude.rnf offeringArn
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf reservationName

instance Data.ToHeaders PurchaseOffering where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PurchaseOffering where
  toJSON PurchaseOffering' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("start" Data..= start),
            Prelude.Just
              ("reservationName" Data..= reservationName)
          ]
      )

instance Data.ToPath PurchaseOffering where
  toPath PurchaseOffering' {..} =
    Prelude.mconcat
      ["/v1/offerings/", Data.toBS offeringArn]

instance Data.ToQuery PurchaseOffering where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPurchaseOfferingResponse' smart constructor.
data PurchaseOfferingResponse = PurchaseOfferingResponse'
  { reservation :: Prelude.Maybe Reservation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservation', 'purchaseOfferingResponse_reservation' - Undocumented member.
--
-- 'httpStatus', 'purchaseOfferingResponse_httpStatus' - The response's http status code.
newPurchaseOfferingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PurchaseOfferingResponse
newPurchaseOfferingResponse pHttpStatus_ =
  PurchaseOfferingResponse'
    { reservation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
purchaseOfferingResponse_reservation :: Lens.Lens' PurchaseOfferingResponse (Prelude.Maybe Reservation)
purchaseOfferingResponse_reservation = Lens.lens (\PurchaseOfferingResponse' {reservation} -> reservation) (\s@PurchaseOfferingResponse' {} a -> s {reservation = a} :: PurchaseOfferingResponse)

-- | The response's http status code.
purchaseOfferingResponse_httpStatus :: Lens.Lens' PurchaseOfferingResponse Prelude.Int
purchaseOfferingResponse_httpStatus = Lens.lens (\PurchaseOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseOfferingResponse)

instance Prelude.NFData PurchaseOfferingResponse where
  rnf PurchaseOfferingResponse' {..} =
    Prelude.rnf reservation
      `Prelude.seq` Prelude.rnf httpStatus
