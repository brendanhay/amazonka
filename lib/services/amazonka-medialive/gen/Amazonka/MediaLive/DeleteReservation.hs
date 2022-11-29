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
-- Module      : Amazonka.MediaLive.DeleteReservation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an expired reservation.
module Amazonka.MediaLive.DeleteReservation
  ( -- * Creating a Request
    DeleteReservation (..),
    newDeleteReservation,

    -- * Request Lenses
    deleteReservation_reservationId,

    -- * Destructuring the Response
    DeleteReservationResponse (..),
    newDeleteReservationResponse,

    -- * Response Lenses
    deleteReservationResponse_tags,
    deleteReservationResponse_name,
    deleteReservationResponse_renewalSettings,
    deleteReservationResponse_start,
    deleteReservationResponse_arn,
    deleteReservationResponse_resourceSpecification,
    deleteReservationResponse_state,
    deleteReservationResponse_offeringType,
    deleteReservationResponse_durationUnits,
    deleteReservationResponse_count,
    deleteReservationResponse_duration,
    deleteReservationResponse_currencyCode,
    deleteReservationResponse_end,
    deleteReservationResponse_region,
    deleteReservationResponse_offeringId,
    deleteReservationResponse_offeringDescription,
    deleteReservationResponse_reservationId,
    deleteReservationResponse_fixedPrice,
    deleteReservationResponse_usagePrice,
    deleteReservationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DeleteReservationRequest
--
-- /See:/ 'newDeleteReservation' smart constructor.
data DeleteReservation = DeleteReservation'
  { -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservationId', 'deleteReservation_reservationId' - Unique reservation ID, e.g. \'1234567\'
newDeleteReservation ::
  -- | 'reservationId'
  Prelude.Text ->
  DeleteReservation
newDeleteReservation pReservationId_ =
  DeleteReservation' {reservationId = pReservationId_}

-- | Unique reservation ID, e.g. \'1234567\'
deleteReservation_reservationId :: Lens.Lens' DeleteReservation Prelude.Text
deleteReservation_reservationId = Lens.lens (\DeleteReservation' {reservationId} -> reservationId) (\s@DeleteReservation' {} a -> s {reservationId = a} :: DeleteReservation)

instance Core.AWSRequest DeleteReservation where
  type
    AWSResponse DeleteReservation =
      DeleteReservationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReservationResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "renewalSettings")
            Prelude.<*> (x Core..?> "start")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "resourceSpecification")
            Prelude.<*> (x Core..?> "state")
            Prelude.<*> (x Core..?> "offeringType")
            Prelude.<*> (x Core..?> "durationUnits")
            Prelude.<*> (x Core..?> "count")
            Prelude.<*> (x Core..?> "duration")
            Prelude.<*> (x Core..?> "currencyCode")
            Prelude.<*> (x Core..?> "end")
            Prelude.<*> (x Core..?> "region")
            Prelude.<*> (x Core..?> "offeringId")
            Prelude.<*> (x Core..?> "offeringDescription")
            Prelude.<*> (x Core..?> "reservationId")
            Prelude.<*> (x Core..?> "fixedPrice")
            Prelude.<*> (x Core..?> "usagePrice")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReservation where
  hashWithSalt _salt DeleteReservation' {..} =
    _salt `Prelude.hashWithSalt` reservationId

instance Prelude.NFData DeleteReservation where
  rnf DeleteReservation' {..} =
    Prelude.rnf reservationId

instance Core.ToHeaders DeleteReservation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteReservation where
  toPath DeleteReservation' {..} =
    Prelude.mconcat
      ["/prod/reservations/", Core.toBS reservationId]

instance Core.ToQuery DeleteReservation where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DeleteReservationResponse
--
-- /See:/ 'newDeleteReservationResponse' smart constructor.
data DeleteReservationResponse = DeleteReservationResponse'
  { -- | A collection of key-value pairs
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | User specified reservation name
    name :: Prelude.Maybe Prelude.Text,
    -- | Renewal settings for the reservation
    renewalSettings :: Prelude.Maybe RenewalSettings,
    -- | Reservation UTC start date and time in ISO-8601 format, e.g.
    -- \'2018-03-01T00:00:00\'
    start :: Prelude.Maybe Prelude.Text,
    -- | Unique reservation ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
    arn :: Prelude.Maybe Prelude.Text,
    -- | Resource configuration details
    resourceSpecification :: Prelude.Maybe ReservationResourceSpecification,
    -- | Current state of reservation, e.g. \'ACTIVE\'
    state :: Prelude.Maybe ReservationState,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Prelude.Maybe OfferingType,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Prelude.Maybe OfferingDurationUnits,
    -- | Number of reserved resources
    count :: Prelude.Maybe Prelude.Int,
    -- | Lease duration, e.g. \'12\'
    duration :: Prelude.Maybe Prelude.Int,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | Reservation UTC end date and time in ISO-8601 format, e.g.
    -- \'2019-03-01T00:00:00\'
    end :: Prelude.Maybe Prelude.Text,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Prelude.Maybe Prelude.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Prelude.Maybe Prelude.Text,
    -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Prelude.Maybe Prelude.Text,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'deleteReservationResponse_tags' - A collection of key-value pairs
--
-- 'name', 'deleteReservationResponse_name' - User specified reservation name
--
-- 'renewalSettings', 'deleteReservationResponse_renewalSettings' - Renewal settings for the reservation
--
-- 'start', 'deleteReservationResponse_start' - Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
--
-- 'arn', 'deleteReservationResponse_arn' - Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
--
-- 'resourceSpecification', 'deleteReservationResponse_resourceSpecification' - Resource configuration details
--
-- 'state', 'deleteReservationResponse_state' - Current state of reservation, e.g. \'ACTIVE\'
--
-- 'offeringType', 'deleteReservationResponse_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'durationUnits', 'deleteReservationResponse_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'count', 'deleteReservationResponse_count' - Number of reserved resources
--
-- 'duration', 'deleteReservationResponse_duration' - Lease duration, e.g. \'12\'
--
-- 'currencyCode', 'deleteReservationResponse_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'end', 'deleteReservationResponse_end' - Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
--
-- 'region', 'deleteReservationResponse_region' - AWS region, e.g. \'us-west-2\'
--
-- 'offeringId', 'deleteReservationResponse_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'offeringDescription', 'deleteReservationResponse_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'reservationId', 'deleteReservationResponse_reservationId' - Unique reservation ID, e.g. \'1234567\'
--
-- 'fixedPrice', 'deleteReservationResponse_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'usagePrice', 'deleteReservationResponse_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'httpStatus', 'deleteReservationResponse_httpStatus' - The response's http status code.
newDeleteReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReservationResponse
newDeleteReservationResponse pHttpStatus_ =
  DeleteReservationResponse'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      renewalSettings = Prelude.Nothing,
      start = Prelude.Nothing,
      arn = Prelude.Nothing,
      resourceSpecification = Prelude.Nothing,
      state = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      durationUnits = Prelude.Nothing,
      count = Prelude.Nothing,
      duration = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      end = Prelude.Nothing,
      region = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      offeringDescription = Prelude.Nothing,
      reservationId = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of key-value pairs
deleteReservationResponse_tags :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deleteReservationResponse_tags = Lens.lens (\DeleteReservationResponse' {tags} -> tags) (\s@DeleteReservationResponse' {} a -> s {tags = a} :: DeleteReservationResponse) Prelude.. Lens.mapping Lens.coerced

-- | User specified reservation name
deleteReservationResponse_name :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Text)
deleteReservationResponse_name = Lens.lens (\DeleteReservationResponse' {name} -> name) (\s@DeleteReservationResponse' {} a -> s {name = a} :: DeleteReservationResponse)

-- | Renewal settings for the reservation
deleteReservationResponse_renewalSettings :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe RenewalSettings)
deleteReservationResponse_renewalSettings = Lens.lens (\DeleteReservationResponse' {renewalSettings} -> renewalSettings) (\s@DeleteReservationResponse' {} a -> s {renewalSettings = a} :: DeleteReservationResponse)

-- | Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
deleteReservationResponse_start :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Text)
deleteReservationResponse_start = Lens.lens (\DeleteReservationResponse' {start} -> start) (\s@DeleteReservationResponse' {} a -> s {start = a} :: DeleteReservationResponse)

-- | Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
deleteReservationResponse_arn :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Text)
deleteReservationResponse_arn = Lens.lens (\DeleteReservationResponse' {arn} -> arn) (\s@DeleteReservationResponse' {} a -> s {arn = a} :: DeleteReservationResponse)

-- | Resource configuration details
deleteReservationResponse_resourceSpecification :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe ReservationResourceSpecification)
deleteReservationResponse_resourceSpecification = Lens.lens (\DeleteReservationResponse' {resourceSpecification} -> resourceSpecification) (\s@DeleteReservationResponse' {} a -> s {resourceSpecification = a} :: DeleteReservationResponse)

-- | Current state of reservation, e.g. \'ACTIVE\'
deleteReservationResponse_state :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe ReservationState)
deleteReservationResponse_state = Lens.lens (\DeleteReservationResponse' {state} -> state) (\s@DeleteReservationResponse' {} a -> s {state = a} :: DeleteReservationResponse)

-- | Offering type, e.g. \'NO_UPFRONT\'
deleteReservationResponse_offeringType :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe OfferingType)
deleteReservationResponse_offeringType = Lens.lens (\DeleteReservationResponse' {offeringType} -> offeringType) (\s@DeleteReservationResponse' {} a -> s {offeringType = a} :: DeleteReservationResponse)

-- | Units for duration, e.g. \'MONTHS\'
deleteReservationResponse_durationUnits :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe OfferingDurationUnits)
deleteReservationResponse_durationUnits = Lens.lens (\DeleteReservationResponse' {durationUnits} -> durationUnits) (\s@DeleteReservationResponse' {} a -> s {durationUnits = a} :: DeleteReservationResponse)

-- | Number of reserved resources
deleteReservationResponse_count :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Int)
deleteReservationResponse_count = Lens.lens (\DeleteReservationResponse' {count} -> count) (\s@DeleteReservationResponse' {} a -> s {count = a} :: DeleteReservationResponse)

-- | Lease duration, e.g. \'12\'
deleteReservationResponse_duration :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Int)
deleteReservationResponse_duration = Lens.lens (\DeleteReservationResponse' {duration} -> duration) (\s@DeleteReservationResponse' {} a -> s {duration = a} :: DeleteReservationResponse)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
deleteReservationResponse_currencyCode :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Text)
deleteReservationResponse_currencyCode = Lens.lens (\DeleteReservationResponse' {currencyCode} -> currencyCode) (\s@DeleteReservationResponse' {} a -> s {currencyCode = a} :: DeleteReservationResponse)

-- | Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
deleteReservationResponse_end :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Text)
deleteReservationResponse_end = Lens.lens (\DeleteReservationResponse' {end} -> end) (\s@DeleteReservationResponse' {} a -> s {end = a} :: DeleteReservationResponse)

-- | AWS region, e.g. \'us-west-2\'
deleteReservationResponse_region :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Text)
deleteReservationResponse_region = Lens.lens (\DeleteReservationResponse' {region} -> region) (\s@DeleteReservationResponse' {} a -> s {region = a} :: DeleteReservationResponse)

-- | Unique offering ID, e.g. \'87654321\'
deleteReservationResponse_offeringId :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Text)
deleteReservationResponse_offeringId = Lens.lens (\DeleteReservationResponse' {offeringId} -> offeringId) (\s@DeleteReservationResponse' {} a -> s {offeringId = a} :: DeleteReservationResponse)

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
deleteReservationResponse_offeringDescription :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Text)
deleteReservationResponse_offeringDescription = Lens.lens (\DeleteReservationResponse' {offeringDescription} -> offeringDescription) (\s@DeleteReservationResponse' {} a -> s {offeringDescription = a} :: DeleteReservationResponse)

-- | Unique reservation ID, e.g. \'1234567\'
deleteReservationResponse_reservationId :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Text)
deleteReservationResponse_reservationId = Lens.lens (\DeleteReservationResponse' {reservationId} -> reservationId) (\s@DeleteReservationResponse' {} a -> s {reservationId = a} :: DeleteReservationResponse)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
deleteReservationResponse_fixedPrice :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Double)
deleteReservationResponse_fixedPrice = Lens.lens (\DeleteReservationResponse' {fixedPrice} -> fixedPrice) (\s@DeleteReservationResponse' {} a -> s {fixedPrice = a} :: DeleteReservationResponse)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
deleteReservationResponse_usagePrice :: Lens.Lens' DeleteReservationResponse (Prelude.Maybe Prelude.Double)
deleteReservationResponse_usagePrice = Lens.lens (\DeleteReservationResponse' {usagePrice} -> usagePrice) (\s@DeleteReservationResponse' {} a -> s {usagePrice = a} :: DeleteReservationResponse)

-- | The response's http status code.
deleteReservationResponse_httpStatus :: Lens.Lens' DeleteReservationResponse Prelude.Int
deleteReservationResponse_httpStatus = Lens.lens (\DeleteReservationResponse' {httpStatus} -> httpStatus) (\s@DeleteReservationResponse' {} a -> s {httpStatus = a} :: DeleteReservationResponse)

instance Prelude.NFData DeleteReservationResponse where
  rnf DeleteReservationResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf renewalSettings
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf resourceSpecification
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf durationUnits
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf end
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf offeringId
      `Prelude.seq` Prelude.rnf offeringDescription
      `Prelude.seq` Prelude.rnf reservationId
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf usagePrice
      `Prelude.seq` Prelude.rnf httpStatus
