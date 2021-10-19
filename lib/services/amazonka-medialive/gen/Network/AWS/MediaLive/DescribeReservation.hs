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
    describeReservationResponse_state,
    describeReservationResponse_resourceSpecification,
    describeReservationResponse_currencyCode,
    describeReservationResponse_arn,
    describeReservationResponse_start,
    describeReservationResponse_count,
    describeReservationResponse_end,
    describeReservationResponse_name,
    describeReservationResponse_reservationId,
    describeReservationResponse_offeringId,
    describeReservationResponse_region,
    describeReservationResponse_offeringType,
    describeReservationResponse_usagePrice,
    describeReservationResponse_fixedPrice,
    describeReservationResponse_durationUnits,
    describeReservationResponse_offeringDescription,
    describeReservationResponse_duration,
    describeReservationResponse_tags,
    describeReservationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeReservationRequest
--
-- /See:/ 'newDescribeReservation' smart constructor.
data DescribeReservation = DescribeReservation'
  { -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Prelude.Text
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
-- 'reservationId', 'describeReservation_reservationId' - Unique reservation ID, e.g. \'1234567\'
newDescribeReservation ::
  -- | 'reservationId'
  Prelude.Text ->
  DescribeReservation
newDescribeReservation pReservationId_ =
  DescribeReservation'
    { reservationId =
        pReservationId_
    }

-- | Unique reservation ID, e.g. \'1234567\'
describeReservation_reservationId :: Lens.Lens' DescribeReservation Prelude.Text
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
            Prelude.<$> (x Core..?> "state")
            Prelude.<*> (x Core..?> "resourceSpecification")
            Prelude.<*> (x Core..?> "currencyCode")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "start")
            Prelude.<*> (x Core..?> "count")
            Prelude.<*> (x Core..?> "end")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "reservationId")
            Prelude.<*> (x Core..?> "offeringId")
            Prelude.<*> (x Core..?> "region")
            Prelude.<*> (x Core..?> "offeringType")
            Prelude.<*> (x Core..?> "usagePrice")
            Prelude.<*> (x Core..?> "fixedPrice")
            Prelude.<*> (x Core..?> "durationUnits")
            Prelude.<*> (x Core..?> "offeringDescription")
            Prelude.<*> (x Core..?> "duration")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReservation

instance Prelude.NFData DescribeReservation

instance Core.ToHeaders DescribeReservation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeReservation where
  toPath DescribeReservation' {..} =
    Prelude.mconcat
      ["/prod/reservations/", Core.toBS reservationId]

instance Core.ToQuery DescribeReservation where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeReservationResponse
--
-- /See:/ 'newDescribeReservationResponse' smart constructor.
data DescribeReservationResponse = DescribeReservationResponse'
  { -- | Current state of reservation, e.g. \'ACTIVE\'
    state :: Prelude.Maybe ReservationState,
    -- | Resource configuration details
    resourceSpecification :: Prelude.Maybe ReservationResourceSpecification,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | Unique reservation ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
    arn :: Prelude.Maybe Prelude.Text,
    -- | Reservation UTC start date and time in ISO-8601 format, e.g.
    -- \'2018-03-01T00:00:00\'
    start :: Prelude.Maybe Prelude.Text,
    -- | Number of reserved resources
    count :: Prelude.Maybe Prelude.Int,
    -- | Reservation UTC end date and time in ISO-8601 format, e.g.
    -- \'2019-03-01T00:00:00\'
    end :: Prelude.Maybe Prelude.Text,
    -- | User specified reservation name
    name :: Prelude.Maybe Prelude.Text,
    -- | Unique reservation ID, e.g. \'1234567\'
    reservationId :: Prelude.Maybe Prelude.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Prelude.Maybe Prelude.Text,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Prelude.Maybe OfferingType,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Prelude.Maybe OfferingDurationUnits,
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Prelude.Maybe Prelude.Text,
    -- | Lease duration, e.g. \'12\'
    duration :: Prelude.Maybe Prelude.Int,
    -- | A collection of key-value pairs
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'state', 'describeReservationResponse_state' - Current state of reservation, e.g. \'ACTIVE\'
--
-- 'resourceSpecification', 'describeReservationResponse_resourceSpecification' - Resource configuration details
--
-- 'currencyCode', 'describeReservationResponse_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'arn', 'describeReservationResponse_arn' - Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
--
-- 'start', 'describeReservationResponse_start' - Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
--
-- 'count', 'describeReservationResponse_count' - Number of reserved resources
--
-- 'end', 'describeReservationResponse_end' - Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
--
-- 'name', 'describeReservationResponse_name' - User specified reservation name
--
-- 'reservationId', 'describeReservationResponse_reservationId' - Unique reservation ID, e.g. \'1234567\'
--
-- 'offeringId', 'describeReservationResponse_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'region', 'describeReservationResponse_region' - AWS region, e.g. \'us-west-2\'
--
-- 'offeringType', 'describeReservationResponse_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'usagePrice', 'describeReservationResponse_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'fixedPrice', 'describeReservationResponse_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'durationUnits', 'describeReservationResponse_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'offeringDescription', 'describeReservationResponse_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'duration', 'describeReservationResponse_duration' - Lease duration, e.g. \'12\'
--
-- 'tags', 'describeReservationResponse_tags' - A collection of key-value pairs
--
-- 'httpStatus', 'describeReservationResponse_httpStatus' - The response's http status code.
newDescribeReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservationResponse
newDescribeReservationResponse pHttpStatus_ =
  DescribeReservationResponse'
    { state =
        Prelude.Nothing,
      resourceSpecification = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      arn = Prelude.Nothing,
      start = Prelude.Nothing,
      count = Prelude.Nothing,
      end = Prelude.Nothing,
      name = Prelude.Nothing,
      reservationId = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      region = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      durationUnits = Prelude.Nothing,
      offeringDescription = Prelude.Nothing,
      duration = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Current state of reservation, e.g. \'ACTIVE\'
describeReservationResponse_state :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe ReservationState)
describeReservationResponse_state = Lens.lens (\DescribeReservationResponse' {state} -> state) (\s@DescribeReservationResponse' {} a -> s {state = a} :: DescribeReservationResponse)

-- | Resource configuration details
describeReservationResponse_resourceSpecification :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe ReservationResourceSpecification)
describeReservationResponse_resourceSpecification = Lens.lens (\DescribeReservationResponse' {resourceSpecification} -> resourceSpecification) (\s@DescribeReservationResponse' {} a -> s {resourceSpecification = a} :: DescribeReservationResponse)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
describeReservationResponse_currencyCode :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_currencyCode = Lens.lens (\DescribeReservationResponse' {currencyCode} -> currencyCode) (\s@DescribeReservationResponse' {} a -> s {currencyCode = a} :: DescribeReservationResponse)

-- | Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
describeReservationResponse_arn :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_arn = Lens.lens (\DescribeReservationResponse' {arn} -> arn) (\s@DescribeReservationResponse' {} a -> s {arn = a} :: DescribeReservationResponse)

-- | Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
describeReservationResponse_start :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_start = Lens.lens (\DescribeReservationResponse' {start} -> start) (\s@DescribeReservationResponse' {} a -> s {start = a} :: DescribeReservationResponse)

-- | Number of reserved resources
describeReservationResponse_count :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Int)
describeReservationResponse_count = Lens.lens (\DescribeReservationResponse' {count} -> count) (\s@DescribeReservationResponse' {} a -> s {count = a} :: DescribeReservationResponse)

-- | Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
describeReservationResponse_end :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_end = Lens.lens (\DescribeReservationResponse' {end} -> end) (\s@DescribeReservationResponse' {} a -> s {end = a} :: DescribeReservationResponse)

-- | User specified reservation name
describeReservationResponse_name :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_name = Lens.lens (\DescribeReservationResponse' {name} -> name) (\s@DescribeReservationResponse' {} a -> s {name = a} :: DescribeReservationResponse)

-- | Unique reservation ID, e.g. \'1234567\'
describeReservationResponse_reservationId :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_reservationId = Lens.lens (\DescribeReservationResponse' {reservationId} -> reservationId) (\s@DescribeReservationResponse' {} a -> s {reservationId = a} :: DescribeReservationResponse)

-- | Unique offering ID, e.g. \'87654321\'
describeReservationResponse_offeringId :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_offeringId = Lens.lens (\DescribeReservationResponse' {offeringId} -> offeringId) (\s@DescribeReservationResponse' {} a -> s {offeringId = a} :: DescribeReservationResponse)

-- | AWS region, e.g. \'us-west-2\'
describeReservationResponse_region :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_region = Lens.lens (\DescribeReservationResponse' {region} -> region) (\s@DescribeReservationResponse' {} a -> s {region = a} :: DescribeReservationResponse)

-- | Offering type, e.g. \'NO_UPFRONT\'
describeReservationResponse_offeringType :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe OfferingType)
describeReservationResponse_offeringType = Lens.lens (\DescribeReservationResponse' {offeringType} -> offeringType) (\s@DescribeReservationResponse' {} a -> s {offeringType = a} :: DescribeReservationResponse)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
describeReservationResponse_usagePrice :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Double)
describeReservationResponse_usagePrice = Lens.lens (\DescribeReservationResponse' {usagePrice} -> usagePrice) (\s@DescribeReservationResponse' {} a -> s {usagePrice = a} :: DescribeReservationResponse)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
describeReservationResponse_fixedPrice :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Double)
describeReservationResponse_fixedPrice = Lens.lens (\DescribeReservationResponse' {fixedPrice} -> fixedPrice) (\s@DescribeReservationResponse' {} a -> s {fixedPrice = a} :: DescribeReservationResponse)

-- | Units for duration, e.g. \'MONTHS\'
describeReservationResponse_durationUnits :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe OfferingDurationUnits)
describeReservationResponse_durationUnits = Lens.lens (\DescribeReservationResponse' {durationUnits} -> durationUnits) (\s@DescribeReservationResponse' {} a -> s {durationUnits = a} :: DescribeReservationResponse)

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
describeReservationResponse_offeringDescription :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_offeringDescription = Lens.lens (\DescribeReservationResponse' {offeringDescription} -> offeringDescription) (\s@DescribeReservationResponse' {} a -> s {offeringDescription = a} :: DescribeReservationResponse)

-- | Lease duration, e.g. \'12\'
describeReservationResponse_duration :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Int)
describeReservationResponse_duration = Lens.lens (\DescribeReservationResponse' {duration} -> duration) (\s@DescribeReservationResponse' {} a -> s {duration = a} :: DescribeReservationResponse)

-- | A collection of key-value pairs
describeReservationResponse_tags :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeReservationResponse_tags = Lens.lens (\DescribeReservationResponse' {tags} -> tags) (\s@DescribeReservationResponse' {} a -> s {tags = a} :: DescribeReservationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReservationResponse_httpStatus :: Lens.Lens' DescribeReservationResponse Prelude.Int
describeReservationResponse_httpStatus = Lens.lens (\DescribeReservationResponse' {httpStatus} -> httpStatus) (\s@DescribeReservationResponse' {} a -> s {httpStatus = a} :: DescribeReservationResponse)

instance Prelude.NFData DescribeReservationResponse
