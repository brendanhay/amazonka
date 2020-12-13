{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details for a reservation.
module Network.AWS.MediaLive.DescribeReservation
  ( -- * Creating a request
    DescribeReservation (..),
    mkDescribeReservation,

    -- ** Request lenses
    drReservationId,

    -- * Destructuring the response
    DescribeReservationResponse (..),
    mkDescribeReservationResponse,

    -- ** Response lenses
    drfrsState,
    drfrsResourceSpecification,
    drfrsCurrencyCode,
    drfrsARN,
    drfrsStart,
    drfrsCount,
    drfrsEnd,
    drfrsName,
    drfrsReservationId,
    drfrsOfferingId,
    drfrsRegion,
    drfrsOfferingType,
    drfrsUsagePrice,
    drfrsFixedPrice,
    drfrsDurationUnits,
    drfrsOfferingDescription,
    drfrsDuration,
    drfrsTags,
    drfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DescribeReservationRequest
--
-- /See:/ 'mkDescribeReservation' smart constructor.
newtype DescribeReservation = DescribeReservation'
  { -- | Unique reservation ID, e.g. '1234567'
    reservationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservation' with the minimum fields required to make a request.
--
-- * 'reservationId' - Unique reservation ID, e.g. '1234567'
mkDescribeReservation ::
  -- | 'reservationId'
  Lude.Text ->
  DescribeReservation
mkDescribeReservation pReservationId_ =
  DescribeReservation' {reservationId = pReservationId_}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drReservationId :: Lens.Lens' DescribeReservation Lude.Text
drReservationId = Lens.lens (reservationId :: DescribeReservation -> Lude.Text) (\s a -> s {reservationId = a} :: DescribeReservation)
{-# DEPRECATED drReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

instance Lude.AWSRequest DescribeReservation where
  type Rs DescribeReservation = DescribeReservationResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReservationResponse'
            Lude.<$> (x Lude..?> "state")
            Lude.<*> (x Lude..?> "resourceSpecification")
            Lude.<*> (x Lude..?> "currencyCode")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "start")
            Lude.<*> (x Lude..?> "count")
            Lude.<*> (x Lude..?> "end")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "reservationId")
            Lude.<*> (x Lude..?> "offeringId")
            Lude.<*> (x Lude..?> "region")
            Lude.<*> (x Lude..?> "offeringType")
            Lude.<*> (x Lude..?> "usagePrice")
            Lude.<*> (x Lude..?> "fixedPrice")
            Lude.<*> (x Lude..?> "durationUnits")
            Lude.<*> (x Lude..?> "offeringDescription")
            Lude.<*> (x Lude..?> "duration")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeReservation where
  toPath DescribeReservation' {..} =
    Lude.mconcat ["/prod/reservations/", Lude.toBS reservationId]

instance Lude.ToQuery DescribeReservation where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DescribeReservationResponse
--
-- /See:/ 'mkDescribeReservationResponse' smart constructor.
data DescribeReservationResponse = DescribeReservationResponse'
  { -- | Current state of reservation, e.g. 'ACTIVE'
    state :: Lude.Maybe ReservationState,
    -- | Resource configuration details
    resourceSpecification :: Lude.Maybe ReservationResourceSpecification,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
    currencyCode :: Lude.Maybe Lude.Text,
    -- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
    arn :: Lude.Maybe Lude.Text,
    -- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
    start :: Lude.Maybe Lude.Text,
    -- | Number of reserved resources
    count :: Lude.Maybe Lude.Int,
    -- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
    end :: Lude.Maybe Lude.Text,
    -- | User specified reservation name
    name :: Lude.Maybe Lude.Text,
    -- | Unique reservation ID, e.g. '1234567'
    reservationId :: Lude.Maybe Lude.Text,
    -- | Unique offering ID, e.g. '87654321'
    offeringId :: Lude.Maybe Lude.Text,
    -- | AWS region, e.g. 'us-west-2'
    region :: Lude.Maybe Lude.Text,
    -- | Offering type, e.g. 'NO_UPFRONT'
    offeringType :: Lude.Maybe OfferingType,
    -- | Recurring usage charge for each reserved resource, e.g. '157.0'
    usagePrice :: Lude.Maybe Lude.Double,
    -- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
    fixedPrice :: Lude.Maybe Lude.Double,
    -- | Units for duration, e.g. 'MONTHS'
    durationUnits :: Lude.Maybe OfferingDurationUnits,
    -- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
    offeringDescription :: Lude.Maybe Lude.Text,
    -- | Lease duration, e.g. '12'
    duration :: Lude.Maybe Lude.Int,
    -- | A collection of key-value pairs
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservationResponse' with the minimum fields required to make a request.
--
-- * 'state' - Current state of reservation, e.g. 'ACTIVE'
-- * 'resourceSpecification' - Resource configuration details
-- * 'currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
-- * 'arn' - Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
-- * 'start' - Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
-- * 'count' - Number of reserved resources
-- * 'end' - Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
-- * 'name' - User specified reservation name
-- * 'reservationId' - Unique reservation ID, e.g. '1234567'
-- * 'offeringId' - Unique offering ID, e.g. '87654321'
-- * 'region' - AWS region, e.g. 'us-west-2'
-- * 'offeringType' - Offering type, e.g. 'NO_UPFRONT'
-- * 'usagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
-- * 'fixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
-- * 'durationUnits' - Units for duration, e.g. 'MONTHS'
-- * 'offeringDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
-- * 'duration' - Lease duration, e.g. '12'
-- * 'tags' - A collection of key-value pairs
-- * 'responseStatus' - The response status code.
mkDescribeReservationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservationResponse
mkDescribeReservationResponse pResponseStatus_ =
  DescribeReservationResponse'
    { state = Lude.Nothing,
      resourceSpecification = Lude.Nothing,
      currencyCode = Lude.Nothing,
      arn = Lude.Nothing,
      start = Lude.Nothing,
      count = Lude.Nothing,
      end = Lude.Nothing,
      name = Lude.Nothing,
      reservationId = Lude.Nothing,
      offeringId = Lude.Nothing,
      region = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      durationUnits = Lude.Nothing,
      offeringDescription = Lude.Nothing,
      duration = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Current state of reservation, e.g. 'ACTIVE'
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsState :: Lens.Lens' DescribeReservationResponse (Lude.Maybe ReservationState)
drfrsState = Lens.lens (state :: DescribeReservationResponse -> Lude.Maybe ReservationState) (\s a -> s {state = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Resource configuration details
--
-- /Note:/ Consider using 'resourceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsResourceSpecification :: Lens.Lens' DescribeReservationResponse (Lude.Maybe ReservationResourceSpecification)
drfrsResourceSpecification = Lens.lens (resourceSpecification :: DescribeReservationResponse -> Lude.Maybe ReservationResourceSpecification) (\s a -> s {resourceSpecification = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsResourceSpecification "Use generic-lens or generic-optics with 'resourceSpecification' instead." #-}

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsCurrencyCode :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drfrsCurrencyCode = Lens.lens (currencyCode :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsARN :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drfrsARN = Lens.lens (arn :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsStart :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drfrsStart = Lens.lens (start :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {start = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | Number of reserved resources
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsCount :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Int)
drfrsCount = Lens.lens (count :: DescribeReservationResponse -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsEnd :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drfrsEnd = Lens.lens (end :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {end = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | User specified reservation name
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsName :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drfrsName = Lens.lens (name :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsReservationId :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drfrsReservationId = Lens.lens (reservationId :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {reservationId = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsOfferingId :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drfrsOfferingId = Lens.lens (offeringId :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | AWS region, e.g. 'us-west-2'
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsRegion :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drfrsRegion = Lens.lens (region :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Offering type, e.g. 'NO_UPFRONT'
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsOfferingType :: Lens.Lens' DescribeReservationResponse (Lude.Maybe OfferingType)
drfrsOfferingType = Lens.lens (offeringType :: DescribeReservationResponse -> Lude.Maybe OfferingType) (\s a -> s {offeringType = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsUsagePrice :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Double)
drfrsUsagePrice = Lens.lens (usagePrice :: DescribeReservationResponse -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsFixedPrice :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Double)
drfrsFixedPrice = Lens.lens (fixedPrice :: DescribeReservationResponse -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | Units for duration, e.g. 'MONTHS'
--
-- /Note:/ Consider using 'durationUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsDurationUnits :: Lens.Lens' DescribeReservationResponse (Lude.Maybe OfferingDurationUnits)
drfrsDurationUnits = Lens.lens (durationUnits :: DescribeReservationResponse -> Lude.Maybe OfferingDurationUnits) (\s a -> s {durationUnits = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsDurationUnits "Use generic-lens or generic-optics with 'durationUnits' instead." #-}

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- /Note:/ Consider using 'offeringDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsOfferingDescription :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drfrsOfferingDescription = Lens.lens (offeringDescription :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {offeringDescription = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsOfferingDescription "Use generic-lens or generic-optics with 'offeringDescription' instead." #-}

-- | Lease duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsDuration :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Int)
drfrsDuration = Lens.lens (duration :: DescribeReservationResponse -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | A collection of key-value pairs
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsTags :: Lens.Lens' DescribeReservationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
drfrsTags = Lens.lens (tags :: DescribeReservationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsResponseStatus :: Lens.Lens' DescribeReservationResponse Lude.Int
drfrsResponseStatus = Lens.lens (responseStatus :: DescribeReservationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservationResponse)
{-# DEPRECATED drfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
