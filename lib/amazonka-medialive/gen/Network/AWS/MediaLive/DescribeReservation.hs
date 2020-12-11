{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    drrrsState,
    drrrsResourceSpecification,
    drrrsCurrencyCode,
    drrrsARN,
    drrrsStart,
    drrrsCount,
    drrrsEnd,
    drrrsName,
    drrrsReservationId,
    drrrsOfferingId,
    drrrsRegion,
    drrrsOfferingType,
    drrrsUsagePrice,
    drrrsFixedPrice,
    drrrsDurationUnits,
    drrrsOfferingDescription,
    drrrsDuration,
    drrrsTags,
    drrrsResponseStatus,
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
  { reservationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
  { state ::
      Lude.Maybe ReservationState,
    resourceSpecification ::
      Lude.Maybe
        ReservationResourceSpecification,
    currencyCode ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    start :: Lude.Maybe Lude.Text,
    count :: Lude.Maybe Lude.Int,
    end :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    reservationId ::
      Lude.Maybe Lude.Text,
    offeringId :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    offeringType ::
      Lude.Maybe OfferingType,
    usagePrice ::
      Lude.Maybe Lude.Double,
    fixedPrice ::
      Lude.Maybe Lude.Double,
    durationUnits ::
      Lude.Maybe OfferingDurationUnits,
    offeringDescription ::
      Lude.Maybe Lude.Text,
    duration :: Lude.Maybe Lude.Int,
    tags ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservationResponse' with the minimum fields required to make a request.
--
-- * 'arn' - Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
-- * 'count' - Number of reserved resources
-- * 'currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
-- * 'duration' - Lease duration, e.g. '12'
-- * 'durationUnits' - Units for duration, e.g. 'MONTHS'
-- * 'end' - Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
-- * 'fixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
-- * 'name' - User specified reservation name
-- * 'offeringDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
-- * 'offeringId' - Unique offering ID, e.g. '87654321'
-- * 'offeringType' - Offering type, e.g. 'NO_UPFRONT'
-- * 'region' - AWS region, e.g. 'us-west-2'
-- * 'reservationId' - Unique reservation ID, e.g. '1234567'
-- * 'resourceSpecification' - Resource configuration details
-- * 'responseStatus' - The response status code.
-- * 'start' - Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
-- * 'state' - Current state of reservation, e.g. 'ACTIVE'
-- * 'tags' - A collection of key-value pairs
-- * 'usagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
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
drrrsState :: Lens.Lens' DescribeReservationResponse (Lude.Maybe ReservationState)
drrrsState = Lens.lens (state :: DescribeReservationResponse -> Lude.Maybe ReservationState) (\s a -> s {state = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Resource configuration details
--
-- /Note:/ Consider using 'resourceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResourceSpecification :: Lens.Lens' DescribeReservationResponse (Lude.Maybe ReservationResourceSpecification)
drrrsResourceSpecification = Lens.lens (resourceSpecification :: DescribeReservationResponse -> Lude.Maybe ReservationResourceSpecification) (\s a -> s {resourceSpecification = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsResourceSpecification "Use generic-lens or generic-optics with 'resourceSpecification' instead." #-}

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsCurrencyCode :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drrrsCurrencyCode = Lens.lens (currencyCode :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsARN :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drrrsARN = Lens.lens (arn :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsStart :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drrrsStart = Lens.lens (start :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {start = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | Number of reserved resources
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsCount :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Int)
drrrsCount = Lens.lens (count :: DescribeReservationResponse -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsEnd :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drrrsEnd = Lens.lens (end :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {end = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | User specified reservation name
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsName :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drrrsName = Lens.lens (name :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsReservationId :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drrrsReservationId = Lens.lens (reservationId :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {reservationId = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsOfferingId :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drrrsOfferingId = Lens.lens (offeringId :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | AWS region, e.g. 'us-west-2'
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRegion :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drrrsRegion = Lens.lens (region :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Offering type, e.g. 'NO_UPFRONT'
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsOfferingType :: Lens.Lens' DescribeReservationResponse (Lude.Maybe OfferingType)
drrrsOfferingType = Lens.lens (offeringType :: DescribeReservationResponse -> Lude.Maybe OfferingType) (\s a -> s {offeringType = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsUsagePrice :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Double)
drrrsUsagePrice = Lens.lens (usagePrice :: DescribeReservationResponse -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsFixedPrice :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Double)
drrrsFixedPrice = Lens.lens (fixedPrice :: DescribeReservationResponse -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | Units for duration, e.g. 'MONTHS'
--
-- /Note:/ Consider using 'durationUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsDurationUnits :: Lens.Lens' DescribeReservationResponse (Lude.Maybe OfferingDurationUnits)
drrrsDurationUnits = Lens.lens (durationUnits :: DescribeReservationResponse -> Lude.Maybe OfferingDurationUnits) (\s a -> s {durationUnits = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsDurationUnits "Use generic-lens or generic-optics with 'durationUnits' instead." #-}

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- /Note:/ Consider using 'offeringDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsOfferingDescription :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Text)
drrrsOfferingDescription = Lens.lens (offeringDescription :: DescribeReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {offeringDescription = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsOfferingDescription "Use generic-lens or generic-optics with 'offeringDescription' instead." #-}

-- | Lease duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsDuration :: Lens.Lens' DescribeReservationResponse (Lude.Maybe Lude.Int)
drrrsDuration = Lens.lens (duration :: DescribeReservationResponse -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | A collection of key-value pairs
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsTags :: Lens.Lens' DescribeReservationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
drrrsTags = Lens.lens (tags :: DescribeReservationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DescribeReservationResponse Lude.Int
drrrsResponseStatus = Lens.lens (responseStatus :: DescribeReservationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservationResponse)
{-# DEPRECATED drrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
