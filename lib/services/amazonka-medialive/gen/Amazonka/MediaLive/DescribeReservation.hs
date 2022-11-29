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
-- Module      : Amazonka.MediaLive.DescribeReservation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details for a reservation.
module Amazonka.MediaLive.DescribeReservation
  ( -- * Creating a Request
    DescribeReservation (..),
    newDescribeReservation,

    -- * Request Lenses
    describeReservation_reservationId,

    -- * Destructuring the Response
    DescribeReservationResponse (..),
    newDescribeReservationResponse,

    -- * Response Lenses
    describeReservationResponse_tags,
    describeReservationResponse_name,
    describeReservationResponse_renewalSettings,
    describeReservationResponse_start,
    describeReservationResponse_arn,
    describeReservationResponse_resourceSpecification,
    describeReservationResponse_state,
    describeReservationResponse_offeringType,
    describeReservationResponse_durationUnits,
    describeReservationResponse_count,
    describeReservationResponse_duration,
    describeReservationResponse_currencyCode,
    describeReservationResponse_end,
    describeReservationResponse_region,
    describeReservationResponse_offeringId,
    describeReservationResponse_offeringDescription,
    describeReservationResponse_reservationId,
    describeReservationResponse_fixedPrice,
    describeReservationResponse_usagePrice,
    describeReservationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservationResponse'
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

instance Prelude.Hashable DescribeReservation where
  hashWithSalt _salt DescribeReservation' {..} =
    _salt `Prelude.hashWithSalt` reservationId

instance Prelude.NFData DescribeReservation where
  rnf DescribeReservation' {..} =
    Prelude.rnf reservationId

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
-- Create a value of 'DescribeReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeReservationResponse_tags' - A collection of key-value pairs
--
-- 'name', 'describeReservationResponse_name' - User specified reservation name
--
-- 'renewalSettings', 'describeReservationResponse_renewalSettings' - Renewal settings for the reservation
--
-- 'start', 'describeReservationResponse_start' - Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
--
-- 'arn', 'describeReservationResponse_arn' - Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
--
-- 'resourceSpecification', 'describeReservationResponse_resourceSpecification' - Resource configuration details
--
-- 'state', 'describeReservationResponse_state' - Current state of reservation, e.g. \'ACTIVE\'
--
-- 'offeringType', 'describeReservationResponse_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'durationUnits', 'describeReservationResponse_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'count', 'describeReservationResponse_count' - Number of reserved resources
--
-- 'duration', 'describeReservationResponse_duration' - Lease duration, e.g. \'12\'
--
-- 'currencyCode', 'describeReservationResponse_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'end', 'describeReservationResponse_end' - Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
--
-- 'region', 'describeReservationResponse_region' - AWS region, e.g. \'us-west-2\'
--
-- 'offeringId', 'describeReservationResponse_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'offeringDescription', 'describeReservationResponse_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'reservationId', 'describeReservationResponse_reservationId' - Unique reservation ID, e.g. \'1234567\'
--
-- 'fixedPrice', 'describeReservationResponse_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'usagePrice', 'describeReservationResponse_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'httpStatus', 'describeReservationResponse_httpStatus' - The response's http status code.
newDescribeReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservationResponse
newDescribeReservationResponse pHttpStatus_ =
  DescribeReservationResponse'
    { tags =
        Prelude.Nothing,
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
describeReservationResponse_tags :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeReservationResponse_tags = Lens.lens (\DescribeReservationResponse' {tags} -> tags) (\s@DescribeReservationResponse' {} a -> s {tags = a} :: DescribeReservationResponse) Prelude.. Lens.mapping Lens.coerced

-- | User specified reservation name
describeReservationResponse_name :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_name = Lens.lens (\DescribeReservationResponse' {name} -> name) (\s@DescribeReservationResponse' {} a -> s {name = a} :: DescribeReservationResponse)

-- | Renewal settings for the reservation
describeReservationResponse_renewalSettings :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe RenewalSettings)
describeReservationResponse_renewalSettings = Lens.lens (\DescribeReservationResponse' {renewalSettings} -> renewalSettings) (\s@DescribeReservationResponse' {} a -> s {renewalSettings = a} :: DescribeReservationResponse)

-- | Reservation UTC start date and time in ISO-8601 format, e.g.
-- \'2018-03-01T00:00:00\'
describeReservationResponse_start :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_start = Lens.lens (\DescribeReservationResponse' {start} -> start) (\s@DescribeReservationResponse' {} a -> s {start = a} :: DescribeReservationResponse)

-- | Unique reservation ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:reservation:1234567\'
describeReservationResponse_arn :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_arn = Lens.lens (\DescribeReservationResponse' {arn} -> arn) (\s@DescribeReservationResponse' {} a -> s {arn = a} :: DescribeReservationResponse)

-- | Resource configuration details
describeReservationResponse_resourceSpecification :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe ReservationResourceSpecification)
describeReservationResponse_resourceSpecification = Lens.lens (\DescribeReservationResponse' {resourceSpecification} -> resourceSpecification) (\s@DescribeReservationResponse' {} a -> s {resourceSpecification = a} :: DescribeReservationResponse)

-- | Current state of reservation, e.g. \'ACTIVE\'
describeReservationResponse_state :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe ReservationState)
describeReservationResponse_state = Lens.lens (\DescribeReservationResponse' {state} -> state) (\s@DescribeReservationResponse' {} a -> s {state = a} :: DescribeReservationResponse)

-- | Offering type, e.g. \'NO_UPFRONT\'
describeReservationResponse_offeringType :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe OfferingType)
describeReservationResponse_offeringType = Lens.lens (\DescribeReservationResponse' {offeringType} -> offeringType) (\s@DescribeReservationResponse' {} a -> s {offeringType = a} :: DescribeReservationResponse)

-- | Units for duration, e.g. \'MONTHS\'
describeReservationResponse_durationUnits :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe OfferingDurationUnits)
describeReservationResponse_durationUnits = Lens.lens (\DescribeReservationResponse' {durationUnits} -> durationUnits) (\s@DescribeReservationResponse' {} a -> s {durationUnits = a} :: DescribeReservationResponse)

-- | Number of reserved resources
describeReservationResponse_count :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Int)
describeReservationResponse_count = Lens.lens (\DescribeReservationResponse' {count} -> count) (\s@DescribeReservationResponse' {} a -> s {count = a} :: DescribeReservationResponse)

-- | Lease duration, e.g. \'12\'
describeReservationResponse_duration :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Int)
describeReservationResponse_duration = Lens.lens (\DescribeReservationResponse' {duration} -> duration) (\s@DescribeReservationResponse' {} a -> s {duration = a} :: DescribeReservationResponse)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
describeReservationResponse_currencyCode :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_currencyCode = Lens.lens (\DescribeReservationResponse' {currencyCode} -> currencyCode) (\s@DescribeReservationResponse' {} a -> s {currencyCode = a} :: DescribeReservationResponse)

-- | Reservation UTC end date and time in ISO-8601 format, e.g.
-- \'2019-03-01T00:00:00\'
describeReservationResponse_end :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_end = Lens.lens (\DescribeReservationResponse' {end} -> end) (\s@DescribeReservationResponse' {} a -> s {end = a} :: DescribeReservationResponse)

-- | AWS region, e.g. \'us-west-2\'
describeReservationResponse_region :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_region = Lens.lens (\DescribeReservationResponse' {region} -> region) (\s@DescribeReservationResponse' {} a -> s {region = a} :: DescribeReservationResponse)

-- | Unique offering ID, e.g. \'87654321\'
describeReservationResponse_offeringId :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_offeringId = Lens.lens (\DescribeReservationResponse' {offeringId} -> offeringId) (\s@DescribeReservationResponse' {} a -> s {offeringId = a} :: DescribeReservationResponse)

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
describeReservationResponse_offeringDescription :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_offeringDescription = Lens.lens (\DescribeReservationResponse' {offeringDescription} -> offeringDescription) (\s@DescribeReservationResponse' {} a -> s {offeringDescription = a} :: DescribeReservationResponse)

-- | Unique reservation ID, e.g. \'1234567\'
describeReservationResponse_reservationId :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Text)
describeReservationResponse_reservationId = Lens.lens (\DescribeReservationResponse' {reservationId} -> reservationId) (\s@DescribeReservationResponse' {} a -> s {reservationId = a} :: DescribeReservationResponse)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
describeReservationResponse_fixedPrice :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Double)
describeReservationResponse_fixedPrice = Lens.lens (\DescribeReservationResponse' {fixedPrice} -> fixedPrice) (\s@DescribeReservationResponse' {} a -> s {fixedPrice = a} :: DescribeReservationResponse)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
describeReservationResponse_usagePrice :: Lens.Lens' DescribeReservationResponse (Prelude.Maybe Prelude.Double)
describeReservationResponse_usagePrice = Lens.lens (\DescribeReservationResponse' {usagePrice} -> usagePrice) (\s@DescribeReservationResponse' {} a -> s {usagePrice = a} :: DescribeReservationResponse)

-- | The response's http status code.
describeReservationResponse_httpStatus :: Lens.Lens' DescribeReservationResponse Prelude.Int
describeReservationResponse_httpStatus = Lens.lens (\DescribeReservationResponse' {httpStatus} -> httpStatus) (\s@DescribeReservationResponse' {} a -> s {httpStatus = a} :: DescribeReservationResponse)

instance Prelude.NFData DescribeReservationResponse where
  rnf DescribeReservationResponse' {..} =
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
