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
-- Module      : Amazonka.MediaLive.UpdateReservation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update reservation.
module Amazonka.MediaLive.UpdateReservation
  ( -- * Creating a Request
    UpdateReservation' (..),
    newUpdateReservation',

    -- * Request Lenses
    updateReservation'_name,
    updateReservation'_renewalSettings,
    updateReservation'_reservationId,

    -- * Destructuring the Response
    UpdateReservationResponse (..),
    newUpdateReservationResponse,

    -- * Response Lenses
    updateReservationResponse_reservation,
    updateReservationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to update a reservation
--
-- /See:/ 'newUpdateReservation'' smart constructor.
data UpdateReservation' = UpdateReservation''
  { -- | Name of the reservation
    name :: Prelude.Maybe Prelude.Text,
    -- | Renewal settings for the reservation
    renewalSettings :: Prelude.Maybe RenewalSettings,
    -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReservation'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateReservation'_name' - Name of the reservation
--
-- 'renewalSettings', 'updateReservation'_renewalSettings' - Renewal settings for the reservation
--
-- 'reservationId', 'updateReservation'_reservationId' - Unique reservation ID, e.g. \'1234567\'
newUpdateReservation' ::
  -- | 'reservationId'
  Prelude.Text ->
  UpdateReservation'
newUpdateReservation' pReservationId_ =
  UpdateReservation''
    { name = Prelude.Nothing,
      renewalSettings = Prelude.Nothing,
      reservationId = pReservationId_
    }

-- | Name of the reservation
updateReservation'_name :: Lens.Lens' UpdateReservation' (Prelude.Maybe Prelude.Text)
updateReservation'_name = Lens.lens (\UpdateReservation'' {name} -> name) (\s@UpdateReservation'' {} a -> s {name = a} :: UpdateReservation')

-- | Renewal settings for the reservation
updateReservation'_renewalSettings :: Lens.Lens' UpdateReservation' (Prelude.Maybe RenewalSettings)
updateReservation'_renewalSettings = Lens.lens (\UpdateReservation'' {renewalSettings} -> renewalSettings) (\s@UpdateReservation'' {} a -> s {renewalSettings = a} :: UpdateReservation')

-- | Unique reservation ID, e.g. \'1234567\'
updateReservation'_reservationId :: Lens.Lens' UpdateReservation' Prelude.Text
updateReservation'_reservationId = Lens.lens (\UpdateReservation'' {reservationId} -> reservationId) (\s@UpdateReservation'' {} a -> s {reservationId = a} :: UpdateReservation')

instance Core.AWSRequest UpdateReservation' where
  type
    AWSResponse UpdateReservation' =
      UpdateReservationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateReservationResponse'
            Prelude.<$> (x Data..?> "reservation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateReservation' where
  hashWithSalt _salt UpdateReservation'' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` renewalSettings
      `Prelude.hashWithSalt` reservationId

instance Prelude.NFData UpdateReservation' where
  rnf UpdateReservation'' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf renewalSettings
      `Prelude.seq` Prelude.rnf reservationId

instance Data.ToHeaders UpdateReservation' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateReservation' where
  toJSON UpdateReservation'' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("renewalSettings" Data..=)
              Prelude.<$> renewalSettings
          ]
      )

instance Data.ToPath UpdateReservation' where
  toPath UpdateReservation'' {..} =
    Prelude.mconcat
      ["/prod/reservations/", Data.toBS reservationId]

instance Data.ToQuery UpdateReservation' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for UpdateReservationResponse
--
-- /See:/ 'newUpdateReservationResponse' smart constructor.
data UpdateReservationResponse = UpdateReservationResponse'
  { reservation :: Prelude.Maybe Reservation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservation', 'updateReservationResponse_reservation' - Undocumented member.
--
-- 'httpStatus', 'updateReservationResponse_httpStatus' - The response's http status code.
newUpdateReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateReservationResponse
newUpdateReservationResponse pHttpStatus_ =
  UpdateReservationResponse'
    { reservation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateReservationResponse_reservation :: Lens.Lens' UpdateReservationResponse (Prelude.Maybe Reservation)
updateReservationResponse_reservation = Lens.lens (\UpdateReservationResponse' {reservation} -> reservation) (\s@UpdateReservationResponse' {} a -> s {reservation = a} :: UpdateReservationResponse)

-- | The response's http status code.
updateReservationResponse_httpStatus :: Lens.Lens' UpdateReservationResponse Prelude.Int
updateReservationResponse_httpStatus = Lens.lens (\UpdateReservationResponse' {httpStatus} -> httpStatus) (\s@UpdateReservationResponse' {} a -> s {httpStatus = a} :: UpdateReservationResponse)

instance Prelude.NFData UpdateReservationResponse where
  rnf UpdateReservationResponse' {..} =
    Prelude.rnf reservation
      `Prelude.seq` Prelude.rnf httpStatus
