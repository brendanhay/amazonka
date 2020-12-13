{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an expired reservation.
module Network.AWS.MediaLive.DeleteReservation
  ( -- * Creating a request
    DeleteReservation (..),
    mkDeleteReservation,

    -- ** Request lenses
    dReservationId,

    -- * Destructuring the response
    DeleteReservationResponse (..),
    mkDeleteReservationResponse,

    -- ** Response lenses
    drrsState,
    drrsResourceSpecification,
    drrsCurrencyCode,
    drrsARN,
    drrsStart,
    drrsCount,
    drrsEnd,
    drrsName,
    drrsReservationId,
    drrsOfferingId,
    drrsRegion,
    drrsOfferingType,
    drrsUsagePrice,
    drrsFixedPrice,
    drrsDurationUnits,
    drrsOfferingDescription,
    drrsDuration,
    drrsTags,
    drrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DeleteReservationRequest
--
-- /See:/ 'mkDeleteReservation' smart constructor.
newtype DeleteReservation = DeleteReservation'
  { -- | Unique reservation ID, e.g. '1234567'
    reservationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReservation' with the minimum fields required to make a request.
--
-- * 'reservationId' - Unique reservation ID, e.g. '1234567'
mkDeleteReservation ::
  -- | 'reservationId'
  Lude.Text ->
  DeleteReservation
mkDeleteReservation pReservationId_ =
  DeleteReservation' {reservationId = pReservationId_}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dReservationId :: Lens.Lens' DeleteReservation Lude.Text
dReservationId = Lens.lens (reservationId :: DeleteReservation -> Lude.Text) (\s a -> s {reservationId = a} :: DeleteReservation)
{-# DEPRECATED dReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

instance Lude.AWSRequest DeleteReservation where
  type Rs DeleteReservation = DeleteReservationResponse
  request = Req.delete mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteReservationResponse'
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

instance Lude.ToHeaders DeleteReservation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteReservation where
  toPath DeleteReservation' {..} =
    Lude.mconcat ["/prod/reservations/", Lude.toBS reservationId]

instance Lude.ToQuery DeleteReservation where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DeleteReservationResponse
--
-- /See:/ 'mkDeleteReservationResponse' smart constructor.
data DeleteReservationResponse = DeleteReservationResponse'
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

-- | Creates a value of 'DeleteReservationResponse' with the minimum fields required to make a request.
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
mkDeleteReservationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReservationResponse
mkDeleteReservationResponse pResponseStatus_ =
  DeleteReservationResponse'
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
drrsState :: Lens.Lens' DeleteReservationResponse (Lude.Maybe ReservationState)
drrsState = Lens.lens (state :: DeleteReservationResponse -> Lude.Maybe ReservationState) (\s a -> s {state = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Resource configuration details
--
-- /Note:/ Consider using 'resourceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResourceSpecification :: Lens.Lens' DeleteReservationResponse (Lude.Maybe ReservationResourceSpecification)
drrsResourceSpecification = Lens.lens (resourceSpecification :: DeleteReservationResponse -> Lude.Maybe ReservationResourceSpecification) (\s a -> s {resourceSpecification = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsResourceSpecification "Use generic-lens or generic-optics with 'resourceSpecification' instead." #-}

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsCurrencyCode :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Text)
drrsCurrencyCode = Lens.lens (currencyCode :: DeleteReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsARN :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Text)
drrsARN = Lens.lens (arn :: DeleteReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsStart :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Text)
drrsStart = Lens.lens (start :: DeleteReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {start = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | Number of reserved resources
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsCount :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Int)
drrsCount = Lens.lens (count :: DeleteReservationResponse -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsEnd :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Text)
drrsEnd = Lens.lens (end :: DeleteReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {end = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | User specified reservation name
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsName :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Text)
drrsName = Lens.lens (name :: DeleteReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsReservationId :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Text)
drrsReservationId = Lens.lens (reservationId :: DeleteReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {reservationId = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsOfferingId :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Text)
drrsOfferingId = Lens.lens (offeringId :: DeleteReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | AWS region, e.g. 'us-west-2'
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRegion :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Text)
drrsRegion = Lens.lens (region :: DeleteReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Offering type, e.g. 'NO_UPFRONT'
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsOfferingType :: Lens.Lens' DeleteReservationResponse (Lude.Maybe OfferingType)
drrsOfferingType = Lens.lens (offeringType :: DeleteReservationResponse -> Lude.Maybe OfferingType) (\s a -> s {offeringType = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsUsagePrice :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Double)
drrsUsagePrice = Lens.lens (usagePrice :: DeleteReservationResponse -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsFixedPrice :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Double)
drrsFixedPrice = Lens.lens (fixedPrice :: DeleteReservationResponse -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | Units for duration, e.g. 'MONTHS'
--
-- /Note:/ Consider using 'durationUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsDurationUnits :: Lens.Lens' DeleteReservationResponse (Lude.Maybe OfferingDurationUnits)
drrsDurationUnits = Lens.lens (durationUnits :: DeleteReservationResponse -> Lude.Maybe OfferingDurationUnits) (\s a -> s {durationUnits = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsDurationUnits "Use generic-lens or generic-optics with 'durationUnits' instead." #-}

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- /Note:/ Consider using 'offeringDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsOfferingDescription :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Text)
drrsOfferingDescription = Lens.lens (offeringDescription :: DeleteReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {offeringDescription = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsOfferingDescription "Use generic-lens or generic-optics with 'offeringDescription' instead." #-}

-- | Lease duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsDuration :: Lens.Lens' DeleteReservationResponse (Lude.Maybe Lude.Int)
drrsDuration = Lens.lens (duration :: DeleteReservationResponse -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | A collection of key-value pairs
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsTags :: Lens.Lens' DeleteReservationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
drrsTags = Lens.lens (tags :: DeleteReservationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DeleteReservationResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DeleteReservationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReservationResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
