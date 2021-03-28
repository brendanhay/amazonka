{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateReservedInstancesListing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a listing for Amazon EC2 Standard Reserved Instances to be sold in the Reserved Instance Marketplace. You can submit one Standard Reserved Instance listing at a time. To get a list of your Standard Reserved Instances, you can use the 'DescribeReservedInstances' operation.
--
-- The Reserved Instance Marketplace matches sellers who want to resell Standard Reserved Instance capacity that they no longer need with buyers who want to purchase additional capacity. Reserved Instances bought and sold through the Reserved Instance Marketplace work like any other Reserved Instances.
-- To sell your Standard Reserved Instances, you must first register as a seller in the Reserved Instance Marketplace. After completing the registration process, you can create a Reserved Instance Marketplace listing of some or all of your Standard Reserved Instances, and specify the upfront price to receive for them. Your Standard Reserved Instance listings then become available for purchase. To view the details of your Standard Reserved Instance listing, you can use the 'DescribeReservedInstancesListings' operation.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateReservedInstancesListing
    (
    -- * Creating a request
      CreateReservedInstancesListing (..)
    , mkCreateReservedInstancesListing
    -- ** Request lenses
    , crilClientToken
    , crilInstanceCount
    , crilPriceSchedules
    , crilReservedInstancesId

    -- * Destructuring the response
    , CreateReservedInstancesListingResponse (..)
    , mkCreateReservedInstancesListingResponse
    -- ** Response lenses
    , crilrfrsReservedInstancesListings
    , crilrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateReservedInstancesListing.
--
-- /See:/ 'mkCreateReservedInstancesListing' smart constructor.
data CreateReservedInstancesListing = CreateReservedInstancesListing'
  { clientToken :: Core.Text
    -- ^ Unique, case-sensitive identifier you provide to ensure idempotency of your listings. This helps avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
  , instanceCount :: Core.Int
    -- ^ The number of instances that are a part of a Reserved Instance account to be listed in the Reserved Instance Marketplace. This number should be less than or equal to the instance count associated with the Reserved Instance ID specified in this call.
  , priceSchedules :: [Types.PriceScheduleSpecification]
    -- ^ A list specifying the price of the Standard Reserved Instance for each month remaining in the Reserved Instance term.
  , reservedInstancesId :: Types.ReservationId
    -- ^ The ID of the active Standard Reserved Instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReservedInstancesListing' value with any optional fields omitted.
mkCreateReservedInstancesListing
    :: Core.Text -- ^ 'clientToken'
    -> Core.Int -- ^ 'instanceCount'
    -> Types.ReservationId -- ^ 'reservedInstancesId'
    -> CreateReservedInstancesListing
mkCreateReservedInstancesListing clientToken instanceCount
  reservedInstancesId
  = CreateReservedInstancesListing'{clientToken, instanceCount,
                                    priceSchedules = Core.mempty, reservedInstancesId}

-- | Unique, case-sensitive identifier you provide to ensure idempotency of your listings. This helps avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilClientToken :: Lens.Lens' CreateReservedInstancesListing Core.Text
crilClientToken = Lens.field @"clientToken"
{-# INLINEABLE crilClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The number of instances that are a part of a Reserved Instance account to be listed in the Reserved Instance Marketplace. This number should be less than or equal to the instance count associated with the Reserved Instance ID specified in this call.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilInstanceCount :: Lens.Lens' CreateReservedInstancesListing Core.Int
crilInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE crilInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | A list specifying the price of the Standard Reserved Instance for each month remaining in the Reserved Instance term.
--
-- /Note:/ Consider using 'priceSchedules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilPriceSchedules :: Lens.Lens' CreateReservedInstancesListing [Types.PriceScheduleSpecification]
crilPriceSchedules = Lens.field @"priceSchedules"
{-# INLINEABLE crilPriceSchedules #-}
{-# DEPRECATED priceSchedules "Use generic-lens or generic-optics with 'priceSchedules' instead"  #-}

-- | The ID of the active Standard Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilReservedInstancesId :: Lens.Lens' CreateReservedInstancesListing Types.ReservationId
crilReservedInstancesId = Lens.field @"reservedInstancesId"
{-# INLINEABLE crilReservedInstancesId #-}
{-# DEPRECATED reservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead"  #-}

instance Core.ToQuery CreateReservedInstancesListing where
        toQuery CreateReservedInstancesListing{..}
          = Core.toQueryPair "Action"
              ("CreateReservedInstancesListing" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientToken" clientToken
              Core.<> Core.toQueryPair "InstanceCount" instanceCount
              Core.<> Core.toQueryList "PriceSchedules" priceSchedules
              Core.<> Core.toQueryPair "ReservedInstancesId" reservedInstancesId

instance Core.ToHeaders CreateReservedInstancesListing where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateReservedInstancesListing where
        type Rs CreateReservedInstancesListing =
             CreateReservedInstancesListingResponse
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
                 CreateReservedInstancesListingResponse' Core.<$>
                   (x Core..@? "reservedInstancesListingsSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CreateReservedInstancesListing.
--
-- /See:/ 'mkCreateReservedInstancesListingResponse' smart constructor.
data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse'
  { reservedInstancesListings :: Core.Maybe [Types.ReservedInstancesListing]
    -- ^ Information about the Standard Reserved Instance listing.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateReservedInstancesListingResponse' value with any optional fields omitted.
mkCreateReservedInstancesListingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateReservedInstancesListingResponse
mkCreateReservedInstancesListingResponse responseStatus
  = CreateReservedInstancesListingResponse'{reservedInstancesListings
                                              = Core.Nothing,
                                            responseStatus}

-- | Information about the Standard Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilrfrsReservedInstancesListings :: Lens.Lens' CreateReservedInstancesListingResponse (Core.Maybe [Types.ReservedInstancesListing])
crilrfrsReservedInstancesListings = Lens.field @"reservedInstancesListings"
{-# INLINEABLE crilrfrsReservedInstancesListings #-}
{-# DEPRECATED reservedInstancesListings "Use generic-lens or generic-optics with 'reservedInstancesListings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilrfrsResponseStatus :: Lens.Lens' CreateReservedInstancesListingResponse Core.Int
crilrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crilrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
