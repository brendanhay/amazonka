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
    urReservationId,
    urName,

    -- * Destructuring the response
    UpdateReservationResponse (..),
    mkUpdateReservationResponse,

    -- ** Response lenses
    urrrsReservation,
    urrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update a reservation
--
-- /See:/ 'mkUpdateReservation' smart constructor.
data UpdateReservation = UpdateReservation'
  { -- | Unique reservation ID, e.g. '1234567'
    reservationId :: Core.Text,
    -- | Name of the reservation
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateReservation' value with any optional fields omitted.
mkUpdateReservation ::
  -- | 'reservationId'
  Core.Text ->
  UpdateReservation
mkUpdateReservation reservationId =
  UpdateReservation' {reservationId, name = Core.Nothing}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urReservationId :: Lens.Lens' UpdateReservation Core.Text
urReservationId = Lens.field @"reservationId"
{-# DEPRECATED urReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

-- | Name of the reservation
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urName :: Lens.Lens' UpdateReservation (Core.Maybe Core.Text)
urName = Lens.field @"name"
{-# DEPRECATED urName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateReservation where
  toJSON UpdateReservation {..} =
    Core.object (Core.catMaybes [("name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateReservation where
  type Rs UpdateReservation = UpdateReservationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/prod/reservations/" Core.<> (Core.toText reservationId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateReservationResponse'
            Core.<$> (x Core..:? "reservation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for UpdateReservationResponse
--
-- /See:/ 'mkUpdateReservationResponse' smart constructor.
data UpdateReservationResponse = UpdateReservationResponse'
  { reservation :: Core.Maybe Types.Reservation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateReservationResponse' value with any optional fields omitted.
mkUpdateReservationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateReservationResponse
mkUpdateReservationResponse responseStatus =
  UpdateReservationResponse'
    { reservation = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsReservation :: Lens.Lens' UpdateReservationResponse (Core.Maybe Types.Reservation)
urrrsReservation = Lens.field @"reservation"
{-# DEPRECATED urrrsReservation "Use generic-lens or generic-optics with 'reservation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateReservationResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
