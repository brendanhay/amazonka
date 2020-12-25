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
    poOfferingId,
    poCount,
    poName,
    poRequestId,
    poStart,
    poTags,

    -- * Destructuring the response
    PurchaseOfferingResponse (..),
    mkPurchaseOfferingResponse,

    -- ** Response lenses
    porrsReservation,
    porrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for PurchaseOfferingRequest
--
-- /See:/ 'mkPurchaseOffering' smart constructor.
data PurchaseOffering = PurchaseOffering'
  { -- | Offering to purchase, e.g. '87654321'
    offeringId :: Core.Text,
    -- | Number of resources
    count :: Core.Natural,
    -- | Name for the new reservation
    name :: Core.Maybe Core.Text,
    -- | Unique request ID to be specified. This is needed to prevent retries from creating multiple resources.
    requestId :: Core.Maybe Core.Text,
    -- | Requested reservation start time (UTC) in ISO-8601 format. The specified time must be between the first day of the current month and one year from now. If no value is given, the default is now.
    start :: Core.Maybe Core.Text,
    -- | A collection of key-value pairs
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseOffering' value with any optional fields omitted.
mkPurchaseOffering ::
  -- | 'offeringId'
  Core.Text ->
  -- | 'count'
  Core.Natural ->
  PurchaseOffering
mkPurchaseOffering offeringId count =
  PurchaseOffering'
    { offeringId,
      count,
      name = Core.Nothing,
      requestId = Core.Nothing,
      start = Core.Nothing,
      tags = Core.Nothing
    }

-- | Offering to purchase, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOfferingId :: Lens.Lens' PurchaseOffering Core.Text
poOfferingId = Lens.field @"offeringId"
{-# DEPRECATED poOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | Number of resources
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poCount :: Lens.Lens' PurchaseOffering Core.Natural
poCount = Lens.field @"count"
{-# DEPRECATED poCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | Name for the new reservation
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poName :: Lens.Lens' PurchaseOffering (Core.Maybe Core.Text)
poName = Lens.field @"name"
{-# DEPRECATED poName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Unique request ID to be specified. This is needed to prevent retries from creating multiple resources.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poRequestId :: Lens.Lens' PurchaseOffering (Core.Maybe Core.Text)
poRequestId = Lens.field @"requestId"
{-# DEPRECATED poRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | Requested reservation start time (UTC) in ISO-8601 format. The specified time must be between the first day of the current month and one year from now. If no value is given, the default is now.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poStart :: Lens.Lens' PurchaseOffering (Core.Maybe Core.Text)
poStart = Lens.field @"start"
{-# DEPRECATED poStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | A collection of key-value pairs
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poTags :: Lens.Lens' PurchaseOffering (Core.Maybe (Core.HashMap Core.Text Core.Text))
poTags = Lens.field @"tags"
{-# DEPRECATED poTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON PurchaseOffering where
  toJSON PurchaseOffering {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("count" Core..= count),
            ("name" Core..=) Core.<$> name,
            ("requestId" Core..=) Core.<$> requestId,
            ("start" Core..=) Core.<$> start,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest PurchaseOffering where
  type Rs PurchaseOffering = PurchaseOfferingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/prod/offerings/" Core.<> (Core.toText offeringId)
                Core.<> ("/purchase")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PurchaseOfferingResponse'
            Core.<$> (x Core..:? "reservation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for PurchaseOfferingResponse
--
-- /See:/ 'mkPurchaseOfferingResponse' smart constructor.
data PurchaseOfferingResponse = PurchaseOfferingResponse'
  { reservation :: Core.Maybe Types.Reservation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseOfferingResponse' value with any optional fields omitted.
mkPurchaseOfferingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PurchaseOfferingResponse
mkPurchaseOfferingResponse responseStatus =
  PurchaseOfferingResponse'
    { reservation = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsReservation :: Lens.Lens' PurchaseOfferingResponse (Core.Maybe Types.Reservation)
porrsReservation = Lens.field @"reservation"
{-# DEPRECATED porrsReservation "Use generic-lens or generic-optics with 'reservation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsResponseStatus :: Lens.Lens' PurchaseOfferingResponse Core.Int
porrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED porrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
