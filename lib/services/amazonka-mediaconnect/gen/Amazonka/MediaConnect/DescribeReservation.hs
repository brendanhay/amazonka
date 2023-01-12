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
-- Module      : Amazonka.MediaConnect.DescribeReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the details of a reservation. The response includes the
-- reservation name, state, start date and time, and the details of the
-- offering that make up the rest of the reservation (such as price,
-- duration, and outbound bandwidth).
module Amazonka.MediaConnect.DescribeReservation
  ( -- * Creating a Request
    DescribeReservation (..),
    newDescribeReservation,

    -- * Request Lenses
    describeReservation_reservationArn,

    -- * Destructuring the Response
    DescribeReservationResponse (..),
    newDescribeReservationResponse,

    -- * Response Lenses
    describeReservationResponse_reservation,
    describeReservationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeReservation' smart constructor.
data DescribeReservation = DescribeReservation'
  { -- | The Amazon Resource Name (ARN) of the reservation.
    reservationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservationArn', 'describeReservation_reservationArn' - The Amazon Resource Name (ARN) of the reservation.
newDescribeReservation ::
  -- | 'reservationArn'
  Prelude.Text ->
  DescribeReservation
newDescribeReservation pReservationArn_ =
  DescribeReservation'
    { reservationArn =
        pReservationArn_
    }

-- | The Amazon Resource Name (ARN) of the reservation.
describeReservation_reservationArn :: Lens.Lens' DescribeReservation Prelude.Text
describeReservation_reservationArn = Lens.lens (\DescribeReservation' {reservationArn} -> reservationArn) (\s@DescribeReservation' {} a -> s {reservationArn = a} :: DescribeReservation)

instance Core.AWSRequest DescribeReservation where
  type
    AWSResponse DescribeReservation =
      DescribeReservationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservationResponse'
            Prelude.<$> (x Data..?> "reservation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReservation where
  hashWithSalt _salt DescribeReservation' {..} =
    _salt `Prelude.hashWithSalt` reservationArn

instance Prelude.NFData DescribeReservation where
  rnf DescribeReservation' {..} =
    Prelude.rnf reservationArn

instance Data.ToHeaders DescribeReservation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeReservation where
  toPath DescribeReservation' {..} =
    Prelude.mconcat
      ["/v1/reservations/", Data.toBS reservationArn]

instance Data.ToQuery DescribeReservation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeReservationResponse' smart constructor.
data DescribeReservationResponse = DescribeReservationResponse'
  { reservation :: Prelude.Maybe Reservation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservation', 'describeReservationResponse_reservation' - Undocumented member.
--
-- 'httpStatus', 'describeReservationResponse_httpStatus' - The response's http status code.
newDescribeReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservationResponse
newDescribeReservationResponse pHttpStatus_ =
  DescribeReservationResponse'
    { reservation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeReservationResponse_reservation :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Reservation)
describeReservationResponse_reservation = Lens.lens (\DescribeReservationResponse' {reservation} -> reservation) (\s@DescribeReservationResponse' {} a -> s {reservation = a} :: DescribeReservationResponse)

-- | The response's http status code.
describeReservationResponse_httpStatus :: Lens.Lens' DescribeReservationResponse Prelude.Int
describeReservationResponse_httpStatus = Lens.lens (\DescribeReservationResponse' {httpStatus} -> httpStatus) (\s@DescribeReservationResponse' {} a -> s {httpStatus = a} :: DescribeReservationResponse)

instance Prelude.NFData DescribeReservationResponse where
  rnf DescribeReservationResponse' {..} =
    Prelude.rnf reservation
      `Prelude.seq` Prelude.rnf httpStatus
