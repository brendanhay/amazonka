{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CancelReservedInstancesListing (..)
    , mkCancelReservedInstancesListing
    -- ** Request lenses
    , crilReservedInstancesListingId

    -- * Destructuring the response
    , CancelReservedInstancesListingResponse (..)
    , mkCancelReservedInstancesListingResponse
    -- ** Response lenses
    , crilrrsReservedInstancesListings
    , crilrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CancelReservedInstancesListing.
--
-- /See:/ 'mkCancelReservedInstancesListing' smart constructor.
newtype CancelReservedInstancesListing = CancelReservedInstancesListing'
  { reservedInstancesListingId :: Types.ReservedInstancesListingId
    -- ^ The ID of the Reserved Instance listing.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelReservedInstancesListing' value with any optional fields omitted.
mkCancelReservedInstancesListing
    :: Types.ReservedInstancesListingId -- ^ 'reservedInstancesListingId'
    -> CancelReservedInstancesListing
mkCancelReservedInstancesListing reservedInstancesListingId
  = CancelReservedInstancesListing'{reservedInstancesListingId}

-- | The ID of the Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilReservedInstancesListingId :: Lens.Lens' CancelReservedInstancesListing Types.ReservedInstancesListingId
crilReservedInstancesListingId = Lens.field @"reservedInstancesListingId"
{-# INLINEABLE crilReservedInstancesListingId #-}
{-# DEPRECATED reservedInstancesListingId "Use generic-lens or generic-optics with 'reservedInstancesListingId' instead"  #-}

instance Core.ToQuery CancelReservedInstancesListing where
        toQuery CancelReservedInstancesListing{..}
          = Core.toQueryPair "Action"
              ("CancelReservedInstancesListing" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "ReservedInstancesListingId"
                reservedInstancesListingId

instance Core.ToHeaders CancelReservedInstancesListing where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CancelReservedInstancesListing where
        type Rs CancelReservedInstancesListing =
             CancelReservedInstancesListingResponse
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
                 CancelReservedInstancesListingResponse' Core.<$>
                   (x Core..@? "reservedInstancesListingsSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CancelReservedInstancesListing.
--
-- /See:/ 'mkCancelReservedInstancesListingResponse' smart constructor.
data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse'
  { reservedInstancesListings :: Core.Maybe [Types.ReservedInstancesListing]
    -- ^ The Reserved Instance listing.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CancelReservedInstancesListingResponse' value with any optional fields omitted.
mkCancelReservedInstancesListingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelReservedInstancesListingResponse
mkCancelReservedInstancesListingResponse responseStatus
  = CancelReservedInstancesListingResponse'{reservedInstancesListings
                                              = Core.Nothing,
                                            responseStatus}

-- | The Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilrrsReservedInstancesListings :: Lens.Lens' CancelReservedInstancesListingResponse (Core.Maybe [Types.ReservedInstancesListing])
crilrrsReservedInstancesListings = Lens.field @"reservedInstancesListings"
{-# INLINEABLE crilrrsReservedInstancesListings #-}
{-# DEPRECATED reservedInstancesListings "Use generic-lens or generic-optics with 'reservedInstancesListings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilrrsResponseStatus :: Lens.Lens' CancelReservedInstancesListingResponse Core.Int
crilrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crilrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
