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
-- Module      : Network.AWS.MediaLive.DescribeReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details for a reservation.
module Network.AWS.MediaLive.DescribeReservation
  ( -- * Creating a Request
    DescribeReservation (..),
    newDescribeReservation,

    -- * Request Lenses
    describeReservation_reservationId,

    -- * Destructuring the Response
    DescribeReservationResponse (..),
    newDescribeReservationResponse,

    -- * Response Lenses
    describeReservationResponse_end,
    describeReservationResponse_duration,
    describeReservationResponse_durationUnits,
    describeReservationResponse_arn,
    describeReservationResponse_offeringId,
    describeReservationResponse_currencyCode,
    describeReservationResponse_resourceSpecification,
    describeReservationResponse_state,
    describeReservationResponse_name,
    describeReservationResponse_tags,
    describeReservationResponse_offeringDescription,
    describeReservationResponse_count,
    describeReservationResponse_fixedPrice,
    describeReservationResponse_usagePrice,
    describeReservationResponse_offeringType,
    describeReservationResponse_region,
    describeReservationResponse_start,
    describeReservationResponse_reservationId,
    describeReservationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeReservationRequest
--
-- /See:/ 'newDescribeReservation' smart constructor.
data DescribeReservation = DescribeReservation'
  { -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservationId', 'describeReservation_reservationId' - Unique reservation ID, e.g. \'1234567\'
newDescribeReservation ::
  -- | 'reservationId'
  Core.Text ->
  DescribeReservation
newDescribeReservation pReservationId_ =
  DescribeReservation'
    { reservationId =
        pReservationId_
    }

-- | Unique reservation ID, e.g. \'1234567\'
describeReservation_reservationId :: Lens.Lens' DescribeReservation Core.Text
describeReservation_reservationId = Lens.lens (\DescribeReservation' {reservationId} -> reservationId) (\s@DescribeReservation' {} a -> s {reservationId = a} :: DescribeReservation)

instance Core.AWSRequest DescribeReservation where
  type
    AWSResponse DescribeReservation =
      DescribeReservationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservationResponse'
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

instance Core.Hashable DescribeReservation

instance Core.NFData DescribeReservation

instance Core.ToHeaders DescribeReservation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeReservation where
  toPath DescribeReservation' {..} =
    Core.mconcat
      ["/prod/reservations/", Core.toBS reservationId]

instance Core.ToQuery DescribeReservation where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DescribeReservationResponse
--
-- /See:/ 'newDescribeReservationResponse' smart constructor.
data DescribeReservationResponse = DescribeReservationResponse'
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
-- Create a value of 'DescribeReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'describeReservationResponse_end' - Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
--
-- 'duration', 'describeReservationResponse_duration' - Lease duration, e.g. \'12\'
--
-- 'durationUnits', 'describeReservationResponse_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'arn', 'describeReservationResponse_arn' - Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
--
-- 'offeringId', 'describeReservationResponse_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'currencyCode', 'describeReservationResponse_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'resourceSpecification', 'describeReservationResponse_resourceSpecification' - Resource configuration details
--
-- 'state', 'describeReservationResponse_state' - Current state of reservation, e.g. \'ACTIVE\'
--
-- 'name', 'describeReservationResponse_name' - User specified reservation name
--
-- 'tags', 'describeReservationResponse_tags' - A collection of key-value pairs
--
-- 'offeringDescription', 'describeReservationResponse_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'count', 'describeReservationResponse_count' - Number of reserved resources
--
-- 'fixedPrice', 'describeReservationResponse_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'usagePrice', 'describeReservationResponse_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'offeringType', 'describeReservationResponse_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'region', 'describeReservationResponse_region' - AWS region, e.g. \'us-west-2\'
--
-- 'start', 'describeReservationResponse_start' - Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
--
-- 'reservationId', 'describeReservationResponse_reservationId' - Unique reservation ID, e.g. \'1234567\'
--
-- 'httpStatus', 'describeReservationResponse_httpStatus' - The response's http status code.
newDescribeReservationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReservationResponse
newDescribeReservationResponse pHttpStatus_ =
  DescribeReservationResponse'
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
describeReservationResponse_end :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
describeReservationResponse_end = Lens.lens (\DescribeReservationResponse' {end} -> end) (\s@DescribeReservationResponse' {} a -> s {end = a} :: DescribeReservationResponse)

-- | Lease duration, e.g. \'12\'
describeReservationResponse_duration :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Int)
describeReservationResponse_duration = Lens.lens (\DescribeReservationResponse' {duration} -> duration) (\s@DescribeReservationResponse' {} a -> s {duration = a} :: DescribeReservationResponse)

-- | Units for duration, e.g. \'MONTHS\'
describeReservationResponse_durationUnits :: Lens.Lens' DescribeReservationResponse (Core.Maybe OfferingDurationUnits)
describeReservationResponse_durationUnits = Lens.lens (\DescribeReservationResponse' {durationUnits} -> durationUnits) (\s@DescribeReservationResponse' {} a -> s {durationUnits = a} :: DescribeReservationResponse)

-- | Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
describeReservationResponse_arn :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
describeReservationResponse_arn = Lens.lens (\DescribeReservationResponse' {arn} -> arn) (\s@DescribeReservationResponse' {} a -> s {arn = a} :: DescribeReservationResponse)

-- | Unique offering ID, e.g. \'87654321\'
describeReservationResponse_offeringId :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
describeReservationResponse_offeringId = Lens.lens (\DescribeReservationResponse' {offeringId} -> offeringId) (\s@DescribeReservationResponse' {} a -> s {offeringId = a} :: DescribeReservationResponse)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
describeReservationResponse_currencyCode :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
describeReservationResponse_currencyCode = Lens.lens (\DescribeReservationResponse' {currencyCode} -> currencyCode) (\s@DescribeReservationResponse' {} a -> s {currencyCode = a} :: DescribeReservationResponse)

-- | Resource configuration details
describeReservationResponse_resourceSpecification :: Lens.Lens' DescribeReservationResponse (Core.Maybe ReservationResourceSpecification)
describeReservationResponse_resourceSpecification = Lens.lens (\DescribeReservationResponse' {resourceSpecification} -> resourceSpecification) (\s@DescribeReservationResponse' {} a -> s {resourceSpecification = a} :: DescribeReservationResponse)

-- | Current state of reservation, e.g. \'ACTIVE\'
describeReservationResponse_state :: Lens.Lens' DescribeReservationResponse (Core.Maybe ReservationState)
describeReservationResponse_state = Lens.lens (\DescribeReservationResponse' {state} -> state) (\s@DescribeReservationResponse' {} a -> s {state = a} :: DescribeReservationResponse)

-- | User specified reservation name
describeReservationResponse_name :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
describeReservationResponse_name = Lens.lens (\DescribeReservationResponse' {name} -> name) (\s@DescribeReservationResponse' {} a -> s {name = a} :: DescribeReservationResponse)

-- | A collection of key-value pairs
describeReservationResponse_tags :: Lens.Lens' DescribeReservationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeReservationResponse_tags = Lens.lens (\DescribeReservationResponse' {tags} -> tags) (\s@DescribeReservationResponse' {} a -> s {tags = a} :: DescribeReservationResponse) Core.. Lens.mapping Lens._Coerce

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
describeReservationResponse_offeringDescription :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
describeReservationResponse_offeringDescription = Lens.lens (\DescribeReservationResponse' {offeringDescription} -> offeringDescription) (\s@DescribeReservationResponse' {} a -> s {offeringDescription = a} :: DescribeReservationResponse)

-- | Number of reserved resources
describeReservationResponse_count :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Int)
describeReservationResponse_count = Lens.lens (\DescribeReservationResponse' {count} -> count) (\s@DescribeReservationResponse' {} a -> s {count = a} :: DescribeReservationResponse)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
describeReservationResponse_fixedPrice :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Double)
describeReservationResponse_fixedPrice = Lens.lens (\DescribeReservationResponse' {fixedPrice} -> fixedPrice) (\s@DescribeReservationResponse' {} a -> s {fixedPrice = a} :: DescribeReservationResponse)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
describeReservationResponse_usagePrice :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Double)
describeReservationResponse_usagePrice = Lens.lens (\DescribeReservationResponse' {usagePrice} -> usagePrice) (\s@DescribeReservationResponse' {} a -> s {usagePrice = a} :: DescribeReservationResponse)

-- | Offering type, e.g. \'NO_UPFRONT\'
describeReservationResponse_offeringType :: Lens.Lens' DescribeReservationResponse (Core.Maybe OfferingType)
describeReservationResponse_offeringType = Lens.lens (\DescribeReservationResponse' {offeringType} -> offeringType) (\s@DescribeReservationResponse' {} a -> s {offeringType = a} :: DescribeReservationResponse)

-- | AWS region, e.g. \'us-west-2\'
describeReservationResponse_region :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
describeReservationResponse_region = Lens.lens (\DescribeReservationResponse' {region} -> region) (\s@DescribeReservationResponse' {} a -> s {region = a} :: DescribeReservationResponse)

-- | Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
describeReservationResponse_start :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
describeReservationResponse_start = Lens.lens (\DescribeReservationResponse' {start} -> start) (\s@DescribeReservationResponse' {} a -> s {start = a} :: DescribeReservationResponse)

-- | Unique reservation ID, e.g. \'1234567\'
describeReservationResponse_reservationId :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
describeReservationResponse_reservationId = Lens.lens (\DescribeReservationResponse' {reservationId} -> reservationId) (\s@DescribeReservationResponse' {} a -> s {reservationId = a} :: DescribeReservationResponse)

-- | The response's http status code.
describeReservationResponse_httpStatus :: Lens.Lens' DescribeReservationResponse Core.Int
describeReservationResponse_httpStatus = Lens.lens (\DescribeReservationResponse' {httpStatus} -> httpStatus) (\s@DescribeReservationResponse' {} a -> s {httpStatus = a} :: DescribeReservationResponse)

instance Core.NFData DescribeReservationResponse
