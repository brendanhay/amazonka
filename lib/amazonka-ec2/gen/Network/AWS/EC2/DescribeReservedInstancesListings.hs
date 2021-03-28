{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeReservedInstancesListings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your account's Reserved Instance listings in the Reserved Instance Marketplace.
--
-- The Reserved Instance Marketplace matches sellers who want to resell Reserved Instance capacity that they no longer need with buyers who want to purchase additional capacity. Reserved Instances bought and sold through the Reserved Instance Marketplace work like any other Reserved Instances.
-- As a seller, you choose to list some or all of your Reserved Instances, and you specify the upfront price to receive for them. Your Reserved Instances are then listed in the Reserved Instance Marketplace and are available for purchase.
-- As a buyer, you specify the configuration of the Reserved Instance to purchase, and the Marketplace matches what you're searching for with what's available. The Marketplace first sells the lowest priced Reserved Instances to you, and continues to sell available Reserved Instance listings to you until your demand is met. You are charged based on the total price of all of the listings that you purchase.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeReservedInstancesListings
    (
    -- * Creating a request
      DescribeReservedInstancesListings (..)
    , mkDescribeReservedInstancesListings
    -- ** Request lenses
    , drilFilters
    , drilReservedInstancesId
    , drilReservedInstancesListingId

    -- * Destructuring the response
    , DescribeReservedInstancesListingsResponse (..)
    , mkDescribeReservedInstancesListingsResponse
    -- ** Response lenses
    , drilrrsReservedInstancesListings
    , drilrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeReservedInstancesListings.
--
-- /See:/ 'mkDescribeReservedInstancesListings' smart constructor.
data DescribeReservedInstancesListings = DescribeReservedInstancesListings'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @reserved-instances-id@ - The ID of the Reserved Instances.
--
--
--     * @reserved-instances-listing-id@ - The ID of the Reserved Instances listing.
--
--
--     * @status@ - The status of the Reserved Instance listing (@pending@ | @active@ | @cancelled@ | @closed@ ).
--
--
--     * @status-message@ - The reason for the status.
--
--
  , reservedInstancesId :: Core.Maybe Types.ReservationId
    -- ^ One or more Reserved Instance IDs.
  , reservedInstancesListingId :: Core.Maybe Types.ReservedInstancesListingId
    -- ^ One or more Reserved Instance listing IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedInstancesListings' value with any optional fields omitted.
mkDescribeReservedInstancesListings
    :: DescribeReservedInstancesListings
mkDescribeReservedInstancesListings
  = DescribeReservedInstancesListings'{filters = Core.Nothing,
                                       reservedInstancesId = Core.Nothing,
                                       reservedInstancesListingId = Core.Nothing}

-- | One or more filters.
--
--
--     * @reserved-instances-id@ - The ID of the Reserved Instances.
--
--
--     * @reserved-instances-listing-id@ - The ID of the Reserved Instances listing.
--
--
--     * @status@ - The status of the Reserved Instance listing (@pending@ | @active@ | @cancelled@ | @closed@ ).
--
--
--     * @status-message@ - The reason for the status.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drilFilters :: Lens.Lens' DescribeReservedInstancesListings (Core.Maybe [Types.Filter])
drilFilters = Lens.field @"filters"
{-# INLINEABLE drilFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | One or more Reserved Instance IDs.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drilReservedInstancesId :: Lens.Lens' DescribeReservedInstancesListings (Core.Maybe Types.ReservationId)
drilReservedInstancesId = Lens.field @"reservedInstancesId"
{-# INLINEABLE drilReservedInstancesId #-}
{-# DEPRECATED reservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead"  #-}

-- | One or more Reserved Instance listing IDs.
--
-- /Note:/ Consider using 'reservedInstancesListingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drilReservedInstancesListingId :: Lens.Lens' DescribeReservedInstancesListings (Core.Maybe Types.ReservedInstancesListingId)
drilReservedInstancesListingId = Lens.field @"reservedInstancesListingId"
{-# INLINEABLE drilReservedInstancesListingId #-}
{-# DEPRECATED reservedInstancesListingId "Use generic-lens or generic-optics with 'reservedInstancesListingId' instead"  #-}

instance Core.ToQuery DescribeReservedInstancesListings where
        toQuery DescribeReservedInstancesListings{..}
          = Core.toQueryPair "Action"
              ("DescribeReservedInstancesListings" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReservedInstancesId")
                reservedInstancesId
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ReservedInstancesListingId")
                reservedInstancesListingId

instance Core.ToHeaders DescribeReservedInstancesListings where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeReservedInstancesListings where
        type Rs DescribeReservedInstancesListings =
             DescribeReservedInstancesListingsResponse
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
                 DescribeReservedInstancesListingsResponse' Core.<$>
                   (x Core..@? "reservedInstancesListingsSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of DescribeReservedInstancesListings.
--
-- /See:/ 'mkDescribeReservedInstancesListingsResponse' smart constructor.
data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse'
  { reservedInstancesListings :: Core.Maybe [Types.ReservedInstancesListing]
    -- ^ Information about the Reserved Instance listing.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeReservedInstancesListingsResponse' value with any optional fields omitted.
mkDescribeReservedInstancesListingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReservedInstancesListingsResponse
mkDescribeReservedInstancesListingsResponse responseStatus
  = DescribeReservedInstancesListingsResponse'{reservedInstancesListings
                                                 = Core.Nothing,
                                               responseStatus}

-- | Information about the Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drilrrsReservedInstancesListings :: Lens.Lens' DescribeReservedInstancesListingsResponse (Core.Maybe [Types.ReservedInstancesListing])
drilrrsReservedInstancesListings = Lens.field @"reservedInstancesListings"
{-# INLINEABLE drilrrsReservedInstancesListings #-}
{-# DEPRECATED reservedInstancesListings "Use generic-lens or generic-optics with 'reservedInstancesListings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drilrrsResponseStatus :: Lens.Lens' DescribeReservedInstancesListingsResponse Core.Int
drilrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drilrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
