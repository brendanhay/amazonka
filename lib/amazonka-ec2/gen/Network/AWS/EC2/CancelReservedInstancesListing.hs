{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelReservedInstancesListing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Reserved Instance listing in the Reserved Instance Marketplace.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CancelReservedInstancesListing
  ( -- * Creating a request
    CancelReservedInstancesListing (..),
    mkCancelReservedInstancesListing,

    -- ** Request lenses
    crilReservedInstancesListingId,

    -- * Destructuring the response
    CancelReservedInstancesListingResponse (..),
    mkCancelReservedInstancesListingResponse,

    -- ** Response lenses
    crilrrsReservedInstancesListings,
    crilrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CancelReservedInstancesListing.
--
-- /See:/ 'mkCancelReservedInstancesListing' smart constructor.
newtype CancelReservedInstancesListing = CancelReservedInstancesListing'
  { -- | The ID of the Reserved Instance listing.
    reservedInstancesListingId :: Types.ReservedInstancesListingId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelReservedInstancesListing' value with any optional fields omitted.
mkCancelReservedInstancesListing ::
  -- | 'reservedInstancesListingId'
  Types.ReservedInstancesListingId ->
  CancelReservedInstancesListing
mkCancelReservedInstancesListing reservedInstancesListingId =
  CancelReservedInstancesListing' {reservedInstancesListingId}

-- | The ID of the Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilReservedInstancesListingId :: Lens.Lens' CancelReservedInstancesListing Types.ReservedInstancesListingId
crilReservedInstancesListingId = Lens.field @"reservedInstancesListingId"
{-# DEPRECATED crilReservedInstancesListingId "Use generic-lens or generic-optics with 'reservedInstancesListingId' instead." #-}

instance Core.AWSRequest CancelReservedInstancesListing where
  type
    Rs CancelReservedInstancesListing =
      CancelReservedInstancesListingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CancelReservedInstancesListing")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "ReservedInstancesListingId"
                            reservedInstancesListingId
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CancelReservedInstancesListingResponse'
            Core.<$> ( x Core..@? "reservedInstancesListingsSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of CancelReservedInstancesListing.
--
-- /See:/ 'mkCancelReservedInstancesListingResponse' smart constructor.
data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse'
  { -- | The Reserved Instance listing.
    reservedInstancesListings :: Core.Maybe [Types.ReservedInstancesListing],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CancelReservedInstancesListingResponse' value with any optional fields omitted.
mkCancelReservedInstancesListingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelReservedInstancesListingResponse
mkCancelReservedInstancesListingResponse responseStatus =
  CancelReservedInstancesListingResponse'
    { reservedInstancesListings =
        Core.Nothing,
      responseStatus
    }

-- | The Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilrrsReservedInstancesListings :: Lens.Lens' CancelReservedInstancesListingResponse (Core.Maybe [Types.ReservedInstancesListing])
crilrrsReservedInstancesListings = Lens.field @"reservedInstancesListings"
{-# DEPRECATED crilrrsReservedInstancesListings "Use generic-lens or generic-optics with 'reservedInstancesListings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilrrsResponseStatus :: Lens.Lens' CancelReservedInstancesListingResponse Core.Int
crilrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crilrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
