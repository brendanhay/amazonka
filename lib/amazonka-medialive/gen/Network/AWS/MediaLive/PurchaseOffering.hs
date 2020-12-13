{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.PurchaseOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchase an offering and create a reservation.
module Network.AWS.MediaLive.PurchaseOffering
  ( -- * Creating a request
    PurchaseOffering (..),
    mkPurchaseOffering,

    -- ** Request lenses
    poRequestId,
    poStart,
    poCount,
    poName,
    poOfferingId,
    poTags,

    -- * Destructuring the response
    PurchaseOfferingResponse (..),
    mkPurchaseOfferingResponse,

    -- ** Response lenses
    porsReservation,
    porsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for PurchaseOfferingRequest
--
-- /See:/ 'mkPurchaseOffering' smart constructor.
data PurchaseOffering = PurchaseOffering'
  { -- | Unique request ID to be specified. This is needed to prevent retries from creating multiple resources.
    requestId :: Lude.Maybe Lude.Text,
    -- | Requested reservation start time (UTC) in ISO-8601 format. The specified time must be between the first day of the current month and one year from now. If no value is given, the default is now.
    start :: Lude.Maybe Lude.Text,
    -- | Number of resources
    count :: Lude.Natural,
    -- | Name for the new reservation
    name :: Lude.Maybe Lude.Text,
    -- | Offering to purchase, e.g. '87654321'
    offeringId :: Lude.Text,
    -- | A collection of key-value pairs
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseOffering' with the minimum fields required to make a request.
--
-- * 'requestId' - Unique request ID to be specified. This is needed to prevent retries from creating multiple resources.
-- * 'start' - Requested reservation start time (UTC) in ISO-8601 format. The specified time must be between the first day of the current month and one year from now. If no value is given, the default is now.
-- * 'count' - Number of resources
-- * 'name' - Name for the new reservation
-- * 'offeringId' - Offering to purchase, e.g. '87654321'
-- * 'tags' - A collection of key-value pairs
mkPurchaseOffering ::
  -- | 'count'
  Lude.Natural ->
  -- | 'offeringId'
  Lude.Text ->
  PurchaseOffering
mkPurchaseOffering pCount_ pOfferingId_ =
  PurchaseOffering'
    { requestId = Lude.Nothing,
      start = Lude.Nothing,
      count = pCount_,
      name = Lude.Nothing,
      offeringId = pOfferingId_,
      tags = Lude.Nothing
    }

-- | Unique request ID to be specified. This is needed to prevent retries from creating multiple resources.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poRequestId :: Lens.Lens' PurchaseOffering (Lude.Maybe Lude.Text)
poRequestId = Lens.lens (requestId :: PurchaseOffering -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: PurchaseOffering)
{-# DEPRECATED poRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | Requested reservation start time (UTC) in ISO-8601 format. The specified time must be between the first day of the current month and one year from now. If no value is given, the default is now.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poStart :: Lens.Lens' PurchaseOffering (Lude.Maybe Lude.Text)
poStart = Lens.lens (start :: PurchaseOffering -> Lude.Maybe Lude.Text) (\s a -> s {start = a} :: PurchaseOffering)
{-# DEPRECATED poStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | Number of resources
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poCount :: Lens.Lens' PurchaseOffering Lude.Natural
poCount = Lens.lens (count :: PurchaseOffering -> Lude.Natural) (\s a -> s {count = a} :: PurchaseOffering)
{-# DEPRECATED poCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | Name for the new reservation
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poName :: Lens.Lens' PurchaseOffering (Lude.Maybe Lude.Text)
poName = Lens.lens (name :: PurchaseOffering -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PurchaseOffering)
{-# DEPRECATED poName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Offering to purchase, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOfferingId :: Lens.Lens' PurchaseOffering Lude.Text
poOfferingId = Lens.lens (offeringId :: PurchaseOffering -> Lude.Text) (\s a -> s {offeringId = a} :: PurchaseOffering)
{-# DEPRECATED poOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | A collection of key-value pairs
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poTags :: Lens.Lens' PurchaseOffering (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
poTags = Lens.lens (tags :: PurchaseOffering -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: PurchaseOffering)
{-# DEPRECATED poTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest PurchaseOffering where
  type Rs PurchaseOffering = PurchaseOfferingResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          PurchaseOfferingResponse'
            Lude.<$> (x Lude..?> "reservation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PurchaseOffering where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PurchaseOffering where
  toJSON PurchaseOffering' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("requestId" Lude..=) Lude.<$> requestId,
            ("start" Lude..=) Lude.<$> start,
            Lude.Just ("count" Lude..= count),
            ("name" Lude..=) Lude.<$> name,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath PurchaseOffering where
  toPath PurchaseOffering' {..} =
    Lude.mconcat
      ["/prod/offerings/", Lude.toBS offeringId, "/purchase"]

instance Lude.ToQuery PurchaseOffering where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for PurchaseOfferingResponse
--
-- /See:/ 'mkPurchaseOfferingResponse' smart constructor.
data PurchaseOfferingResponse = PurchaseOfferingResponse'
  { reservation :: Lude.Maybe Reservation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseOfferingResponse' with the minimum fields required to make a request.
--
-- * 'reservation' -
-- * 'responseStatus' - The response status code.
mkPurchaseOfferingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PurchaseOfferingResponse
mkPurchaseOfferingResponse pResponseStatus_ =
  PurchaseOfferingResponse'
    { reservation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsReservation :: Lens.Lens' PurchaseOfferingResponse (Lude.Maybe Reservation)
porsReservation = Lens.lens (reservation :: PurchaseOfferingResponse -> Lude.Maybe Reservation) (\s a -> s {reservation = a} :: PurchaseOfferingResponse)
{-# DEPRECATED porsReservation "Use generic-lens or generic-optics with 'reservation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsResponseStatus :: Lens.Lens' PurchaseOfferingResponse Lude.Int
porsResponseStatus = Lens.lens (responseStatus :: PurchaseOfferingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PurchaseOfferingResponse)
{-# DEPRECATED porsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
