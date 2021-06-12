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
-- Module      : Network.AWS.MediaLive.DeleteReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an expired reservation.
module Network.AWS.MediaLive.DeleteReservation
  ( -- * Creating a Request
    DeleteReservation (..),
    newDeleteReservation,

    -- * Request Lenses
    deleteReservation_reservationId,

    -- * Destructuring the Response
    DeleteReservationResponse (..),
    newDeleteReservationResponse,

    -- * Response Lenses
    deleteReservationResponse_end,
    deleteReservationResponse_duration,
    deleteReservationResponse_durationUnits,
    deleteReservationResponse_arn,
    deleteReservationResponse_offeringId,
    deleteReservationResponse_currencyCode,
    deleteReservationResponse_resourceSpecification,
    deleteReservationResponse_state,
    deleteReservationResponse_name,
    deleteReservationResponse_tags,
    deleteReservationResponse_offeringDescription,
    deleteReservationResponse_count,
    deleteReservationResponse_fixedPrice,
    deleteReservationResponse_usagePrice,
    deleteReservationResponse_offeringType,
    deleteReservationResponse_region,
    deleteReservationResponse_start,
    deleteReservationResponse_reservationId,
    deleteReservationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteReservationRequest
--
-- /See:/ 'newDeleteReservation' smart constructor.
data DeleteReservation = DeleteReservation'
  { -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteReservation
newDeleteReservation pReservationId_ =
  DeleteReservation' {reservationId = pReservationId_}

-- | Unique reservation ID, e.g. \'1234567\'
deleteReservation_reservationId :: Lens.Lens' DeleteReservation Core.Text
deleteReservation_reservationId = Lens.lens (\DeleteReservation' {reservationId} -> reservationId) (\s@DeleteReservation' {} a -> s {reservationId = a} :: DeleteReservation)

instance Core.AWSRequest DeleteReservation where
  type
    AWSResponse DeleteReservation =
      DeleteReservationResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReservationResponse'
            Core.<$> (x Core..?> "end")
            Core.<*> (x Core..?> "duration")
            Core.<*> (x Core..?> "durationUnits")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "offeringId")
            Core.<*> (x Core..?> "currencyCode")
            Core.<*> (x Core..?> "resourceSpecification")
            Core.<*> (x Core..?> "state")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "offeringDescription")
            Core.<*> (x Core..?> "count")
            Core.<*> (x Core..?> "fixedPrice")
            Core.<*> (x Core..?> "usagePrice")
            Core.<*> (x Core..?> "offeringType")
            Core.<*> (x Core..?> "region")
            Core.<*> (x Core..?> "start")
            Core.<*> (x Core..?> "reservationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteReservation

instance Core.NFData DeleteReservation

instance Core.ToHeaders DeleteReservation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteReservation where
  toPath DeleteReservation' {..} =
    Core.mconcat
      ["/prod/reservations/", Core.toBS reservationId]

instance Core.ToQuery DeleteReservation where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DeleteReservationResponse
--
-- /See:/ 'newDeleteReservationResponse' smart constructor.
data DeleteReservationResponse = DeleteReservationResponse'
  { -- | Reservation UTC end date and time in ISO-8601 format, e.g.
    -- \'2019-03-01T00:00:00\'
    end :: Core.Maybe Core.Text,
    -- | Lease duration, e.g. \'12\'
    duration :: Core.Maybe Core.Int,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Core.Maybe OfferingDurationUnits,
    -- | Unique reservation ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
    arn :: Core.Maybe Core.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Core.Maybe Core.Text,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Core.Maybe Core.Text,
    -- | Resource configuration details
    resourceSpecification :: Core.Maybe ReservationResourceSpecification,
    -- | Current state of reservation, e.g. \'ACTIVE\'
    state :: Core.Maybe ReservationState,
    -- | User specified reservation name
    name :: Core.Maybe Core.Text,
    -- | A collection of key-value pairs
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Core.Maybe Core.Text,
    -- | Number of reserved resources
    count :: Core.Maybe Core.Int,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Core.Maybe Core.Double,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Core.Maybe Core.Double,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Core.Maybe OfferingType,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Core.Maybe Core.Text,
    -- | Reservation UTC start date and time in ISO-8601 format, e.g.
    -- \'2018-03-01T00:00:00\'
    start :: Core.Maybe Core.Text,
    -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'deleteReservationResponse_end' - Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
--
-- 'duration', 'deleteReservationResponse_duration' - Lease duration, e.g. \'12\'
--
-- 'durationUnits', 'deleteReservationResponse_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'arn', 'deleteReservationResponse_arn' - Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
--
-- 'offeringId', 'deleteReservationResponse_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'currencyCode', 'deleteReservationResponse_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'resourceSpecification', 'deleteReservationResponse_resourceSpecification' - Resource configuration details
--
-- 'state', 'deleteReservationResponse_state' - Current state of reservation, e.g. \'ACTIVE\'
--
-- 'name', 'deleteReservationResponse_name' - User specified reservation name
--
-- 'tags', 'deleteReservationResponse_tags' - A collection of key-value pairs
--
-- 'offeringDescription', 'deleteReservationResponse_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'count', 'deleteReservationResponse_count' - Number of reserved resources
--
-- 'fixedPrice', 'deleteReservationResponse_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'usagePrice', 'deleteReservationResponse_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'offeringType', 'deleteReservationResponse_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'region', 'deleteReservationResponse_region' - AWS region, e.g. \'us-west-2\'
--
-- 'start', 'deleteReservationResponse_start' - Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
--
-- 'reservationId', 'deleteReservationResponse_reservationId' - Unique reservation ID, e.g. \'1234567\'
--
-- 'httpStatus', 'deleteReservationResponse_httpStatus' - The response's http status code.
newDeleteReservationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteReservationResponse
newDeleteReservationResponse pHttpStatus_ =
  DeleteReservationResponse'
    { end = Core.Nothing,
      duration = Core.Nothing,
      durationUnits = Core.Nothing,
      arn = Core.Nothing,
      offeringId = Core.Nothing,
      currencyCode = Core.Nothing,
      resourceSpecification = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      offeringDescription = Core.Nothing,
      count = Core.Nothing,
      fixedPrice = Core.Nothing,
      usagePrice = Core.Nothing,
      offeringType = Core.Nothing,
      region = Core.Nothing,
      start = Core.Nothing,
      reservationId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
deleteReservationResponse_end :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
deleteReservationResponse_end = Lens.lens (\DeleteReservationResponse' {end} -> end) (\s@DeleteReservationResponse' {} a -> s {end = a} :: DeleteReservationResponse)

-- | Lease duration, e.g. \'12\'
deleteReservationResponse_duration :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Int)
deleteReservationResponse_duration = Lens.lens (\DeleteReservationResponse' {duration} -> duration) (\s@DeleteReservationResponse' {} a -> s {duration = a} :: DeleteReservationResponse)

-- | Units for duration, e.g. \'MONTHS\'
deleteReservationResponse_durationUnits :: Lens.Lens' DeleteReservationResponse (Core.Maybe OfferingDurationUnits)
deleteReservationResponse_durationUnits = Lens.lens (\DeleteReservationResponse' {durationUnits} -> durationUnits) (\s@DeleteReservationResponse' {} a -> s {durationUnits = a} :: DeleteReservationResponse)

-- | Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
deleteReservationResponse_arn :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
deleteReservationResponse_arn = Lens.lens (\DeleteReservationResponse' {arn} -> arn) (\s@DeleteReservationResponse' {} a -> s {arn = a} :: DeleteReservationResponse)

-- | Unique offering ID, e.g. \'87654321\'
deleteReservationResponse_offeringId :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
deleteReservationResponse_offeringId = Lens.lens (\DeleteReservationResponse' {offeringId} -> offeringId) (\s@DeleteReservationResponse' {} a -> s {offeringId = a} :: DeleteReservationResponse)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
deleteReservationResponse_currencyCode :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
deleteReservationResponse_currencyCode = Lens.lens (\DeleteReservationResponse' {currencyCode} -> currencyCode) (\s@DeleteReservationResponse' {} a -> s {currencyCode = a} :: DeleteReservationResponse)

-- | Resource configuration details
deleteReservationResponse_resourceSpecification :: Lens.Lens' DeleteReservationResponse (Core.Maybe ReservationResourceSpecification)
deleteReservationResponse_resourceSpecification = Lens.lens (\DeleteReservationResponse' {resourceSpecification} -> resourceSpecification) (\s@DeleteReservationResponse' {} a -> s {resourceSpecification = a} :: DeleteReservationResponse)

-- | Current state of reservation, e.g. \'ACTIVE\'
deleteReservationResponse_state :: Lens.Lens' DeleteReservationResponse (Core.Maybe ReservationState)
deleteReservationResponse_state = Lens.lens (\DeleteReservationResponse' {state} -> state) (\s@DeleteReservationResponse' {} a -> s {state = a} :: DeleteReservationResponse)

-- | User specified reservation name
deleteReservationResponse_name :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
deleteReservationResponse_name = Lens.lens (\DeleteReservationResponse' {name} -> name) (\s@DeleteReservationResponse' {} a -> s {name = a} :: DeleteReservationResponse)

-- | A collection of key-value pairs
deleteReservationResponse_tags :: Lens.Lens' DeleteReservationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
deleteReservationResponse_tags = Lens.lens (\DeleteReservationResponse' {tags} -> tags) (\s@DeleteReservationResponse' {} a -> s {tags = a} :: DeleteReservationResponse) Core.. Lens.mapping Lens._Coerce

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
deleteReservationResponse_offeringDescription :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
deleteReservationResponse_offeringDescription = Lens.lens (\DeleteReservationResponse' {offeringDescription} -> offeringDescription) (\s@DeleteReservationResponse' {} a -> s {offeringDescription = a} :: DeleteReservationResponse)

-- | Number of reserved resources
deleteReservationResponse_count :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Int)
deleteReservationResponse_count = Lens.lens (\DeleteReservationResponse' {count} -> count) (\s@DeleteReservationResponse' {} a -> s {count = a} :: DeleteReservationResponse)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
deleteReservationResponse_fixedPrice :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Double)
deleteReservationResponse_fixedPrice = Lens.lens (\DeleteReservationResponse' {fixedPrice} -> fixedPrice) (\s@DeleteReservationResponse' {} a -> s {fixedPrice = a} :: DeleteReservationResponse)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
deleteReservationResponse_usagePrice :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Double)
deleteReservationResponse_usagePrice = Lens.lens (\DeleteReservationResponse' {usagePrice} -> usagePrice) (\s@DeleteReservationResponse' {} a -> s {usagePrice = a} :: DeleteReservationResponse)

-- | Offering type, e.g. \'NO_UPFRONT\'
deleteReservationResponse_offeringType :: Lens.Lens' DeleteReservationResponse (Core.Maybe OfferingType)
deleteReservationResponse_offeringType = Lens.lens (\DeleteReservationResponse' {offeringType} -> offeringType) (\s@DeleteReservationResponse' {} a -> s {offeringType = a} :: DeleteReservationResponse)

-- | AWS region, e.g. \'us-west-2\'
deleteReservationResponse_region :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
deleteReservationResponse_region = Lens.lens (\DeleteReservationResponse' {region} -> region) (\s@DeleteReservationResponse' {} a -> s {region = a} :: DeleteReservationResponse)

-- | Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
deleteReservationResponse_start :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
deleteReservationResponse_start = Lens.lens (\DeleteReservationResponse' {start} -> start) (\s@DeleteReservationResponse' {} a -> s {start = a} :: DeleteReservationResponse)

-- | Unique reservation ID, e.g. \'1234567\'
deleteReservationResponse_reservationId :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
deleteReservationResponse_reservationId = Lens.lens (\DeleteReservationResponse' {reservationId} -> reservationId) (\s@DeleteReservationResponse' {} a -> s {reservationId = a} :: DeleteReservationResponse)

-- | The response's http status code.
deleteReservationResponse_httpStatus :: Lens.Lens' DeleteReservationResponse Core.Int
deleteReservationResponse_httpStatus = Lens.lens (\DeleteReservationResponse' {httpStatus} -> httpStatus) (\s@DeleteReservationResponse' {} a -> s {httpStatus = a} :: DeleteReservationResponse)

instance Core.NFData DeleteReservationResponse
