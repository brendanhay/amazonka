{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelCapacityReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Capacity Reservation, releases the reserved capacity, and changes the Capacity Reservation's state to @cancelled@ .
--
-- Instances running in the reserved capacity continue running until you stop them. Stopped instances that target the Capacity Reservation can no longer launch. Modify these instances to either target a different Capacity Reservation, launch On-Demand Instance capacity, or run in any open Capacity Reservation that has matching attributes and sufficient capacity.
module Network.AWS.EC2.CancelCapacityReservation
    (
    -- * Creating a request
      CancelCapacityReservation (..)
    , mkCancelCapacityReservation
    -- ** Request lenses
    , ccrfCapacityReservationId
    , ccrfDryRun

    -- * Destructuring the response
    , CancelCapacityReservationResponse (..)
    , mkCancelCapacityReservationResponse
    -- ** Response lenses
    , ccrrfrsReturn
    , ccrrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelCapacityReservation' smart constructor.
data CancelCapacityReservation = CancelCapacityReservation'
  { capacityReservationId :: Types.CapacityReservationId
    -- ^ The ID of the Capacity Reservation to be cancelled.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelCapacityReservation' value with any optional fields omitted.
mkCancelCapacityReservation
    :: Types.CapacityReservationId -- ^ 'capacityReservationId'
    -> CancelCapacityReservation
mkCancelCapacityReservation capacityReservationId
  = CancelCapacityReservation'{capacityReservationId,
                               dryRun = Core.Nothing}

-- | The ID of the Capacity Reservation to be cancelled.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfCapacityReservationId :: Lens.Lens' CancelCapacityReservation Types.CapacityReservationId
ccrfCapacityReservationId = Lens.field @"capacityReservationId"
{-# INLINEABLE ccrfCapacityReservationId #-}
{-# DEPRECATED capacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfDryRun :: Lens.Lens' CancelCapacityReservation (Core.Maybe Core.Bool)
ccrfDryRun = Lens.field @"dryRun"
{-# INLINEABLE ccrfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery CancelCapacityReservation where
        toQuery CancelCapacityReservation{..}
          = Core.toQueryPair "Action"
              ("CancelCapacityReservation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "CapacityReservationId" capacityReservationId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders CancelCapacityReservation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CancelCapacityReservation where
        type Rs CancelCapacityReservation =
             CancelCapacityReservationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CancelCapacityReservationResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelCapacityReservationResponse' smart constructor.
data CancelCapacityReservationResponse = CancelCapacityReservationResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelCapacityReservationResponse' value with any optional fields omitted.
mkCancelCapacityReservationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelCapacityReservationResponse
mkCancelCapacityReservationResponse responseStatus
  = CancelCapacityReservationResponse'{return = Core.Nothing,
                                       responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrfrsReturn :: Lens.Lens' CancelCapacityReservationResponse (Core.Maybe Core.Bool)
ccrrfrsReturn = Lens.field @"return"
{-# INLINEABLE ccrrfrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrfrsResponseStatus :: Lens.Lens' CancelCapacityReservationResponse Core.Int
ccrrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
