{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update reservation.
module Network.AWS.MediaLive.UpdateReservation
  ( -- * Creating a request
    UpdateReservation (..),
    mkUpdateReservation,

    -- ** Request lenses
    urName,
    urReservationId,

    -- * Destructuring the response
    UpdateReservationResponse (..),
    mkUpdateReservationResponse,

    -- ** Response lenses
    urrsReservation,
    urrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to update a reservation
--
-- /See:/ 'mkUpdateReservation' smart constructor.
data UpdateReservation = UpdateReservation'
  { -- | Name of the reservation
    name :: Lude.Maybe Lude.Text,
    -- | Unique reservation ID, e.g. '1234567'
    reservationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateReservation' with the minimum fields required to make a request.
--
-- * 'name' - Name of the reservation
-- * 'reservationId' - Unique reservation ID, e.g. '1234567'
mkUpdateReservation ::
  -- | 'reservationId'
  Lude.Text ->
  UpdateReservation
mkUpdateReservation pReservationId_ =
  UpdateReservation'
    { name = Lude.Nothing,
      reservationId = pReservationId_
    }

-- | Name of the reservation
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urName :: Lens.Lens' UpdateReservation (Lude.Maybe Lude.Text)
urName = Lens.lens (name :: UpdateReservation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateReservation)
{-# DEPRECATED urName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urReservationId :: Lens.Lens' UpdateReservation Lude.Text
urReservationId = Lens.lens (reservationId :: UpdateReservation -> Lude.Text) (\s a -> s {reservationId = a} :: UpdateReservation)
{-# DEPRECATED urReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

instance Lude.AWSRequest UpdateReservation where
  type Rs UpdateReservation = UpdateReservationResponse
  request = Req.putJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateReservationResponse'
            Lude.<$> (x Lude..?> "reservation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateReservation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateReservation where
  toJSON UpdateReservation' {..} =
    Lude.object (Lude.catMaybes [("name" Lude..=) Lude.<$> name])

instance Lude.ToPath UpdateReservation where
  toPath UpdateReservation' {..} =
    Lude.mconcat ["/prod/reservations/", Lude.toBS reservationId]

instance Lude.ToQuery UpdateReservation where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for UpdateReservationResponse
--
-- /See:/ 'mkUpdateReservationResponse' smart constructor.
data UpdateReservationResponse = UpdateReservationResponse'
  { reservation :: Lude.Maybe Reservation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateReservationResponse' with the minimum fields required to make a request.
--
-- * 'reservation' -
-- * 'responseStatus' - The response status code.
mkUpdateReservationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateReservationResponse
mkUpdateReservationResponse pResponseStatus_ =
  UpdateReservationResponse'
    { reservation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsReservation :: Lens.Lens' UpdateReservationResponse (Lude.Maybe Reservation)
urrsReservation = Lens.lens (reservation :: UpdateReservationResponse -> Lude.Maybe Reservation) (\s a -> s {reservation = a} :: UpdateReservationResponse)
{-# DEPRECATED urrsReservation "Use generic-lens or generic-optics with 'reservation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UpdateReservationResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UpdateReservationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateReservationResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
